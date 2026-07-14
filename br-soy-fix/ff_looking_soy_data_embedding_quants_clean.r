#==============================================================================#
# Embedding Soy Deforestation (Avoiding Normalization of Deforestation)
#
# Purpose: Uses remotely sensed soy area as truth rather than IBGE-reported
# area, and calculates production using IBGE-reported yields. This ensures
# that the tons divided by correspond directly to the physical hectares monitored.
#==============================================================================#

library(tidyverse)
library(arrow)
library(aws.s3)
library(zoo)

# Global Configuration
options(scipen = 9999)
aws.signature::use_credentials()

# Define target processing years
years_v260 <- 2013:2020
years_v261 <- 2021:2022
all_years <- c(years_v260, years_v261)

#------------------------------------------------------------------------------#
# STEP 1: Load and Prepare Quantities (Deforestation & Production Data)
#------------------------------------------------------------------------------#
message("--- Step 1: Preparing Quants & Yield Data [Start] ---")

# 1. Load and clean remote sensing annual metrics
quants <- read_parquet(
  "~/documents/data/annual_metrics/soy_annual_br_muni_v3.parquet"
) |>
  pivot_wider(
    id_cols = c(ibge_munic, ibge_state, name, trase_id, year),
    names_from = variable,
    values_from = ha
  ) |>
  ungroup() |>
  group_by(ibge_munic, ibge_state, name, trase_id) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    soy_def_harvest5y_5y_amort = rollmean(
      soy_def_harvest5y,
      k = 5,
      align = "right",
      fill = NA
    ),
    soy_def_harvest3y_5y_amort = rollmean(
      soy_def_harvest3y,
      k = 5,
      align = "right",
      fill = NA
    )
  ) |>
  ungroup()

# 2. Load IBGE baseline production data from S3
ibge_production <- s3read_using(
  FUN = read.table,
  sep = ";",
  header = TRUE,
  as.is = TRUE,
  object = "brazil/soy/indicators/out/q4_2023/soy_production_IBGE_2003_2023_multilevel.csv",
  opts = c("check_region" = TRUE),
  bucket = "trase-storage"
) |>
  as_tibble() |>
  filter(level == "municipality") |>
  select(TRASE_ID, YEAR, HA, TN) |>
  mutate(yield = TN / HA)

# 3. Handle yield gaps (Impute missing data with hierarchy: Municipality Mean -> State Mean -> 0)
quants_prod <- quants |>
  left_join(
    ibge_production,
    by = join_by(trase_id == TRASE_ID, year == YEAR)
  ) |>
  group_by(trase_id) |>
  mutate(mean_yield_muni = mean(yield, na.rm = TRUE)) |>
  ungroup() |>
  group_by(ibge_state) |>
  mutate(mean_yield_state = mean(yield, na.rm = TRUE)) |>
  ungroup() |>
  replace_na(list(yield = 0, mean_yield_muni = 0, mean_yield_state = 0)) |>
  mutate(
    yield_filled = case_when(
      (yield == 0 & mean_yield_muni != 0) ~ mean_yield_muni,
      (yield == 0 & mean_yield_muni == 0) ~ mean_yield_state,
      TRUE ~ yield
    ),
    rs_tons = yield_filled * soy_forest_back
  )

# # 4. Calculate Deforestation per Ton (with Safety Net boundaries)
# quants_prod_per_ton <- quants_prod |>
#   transmute(
#     trase_id,
#     year,
#     rs_tons,
#     across(
#       soy_def_def5y:soy_def_harvest3y_5y_amort,
#       ~ if_else(rs_tons == 0 | is.na(rs_tons) | is.na(.x), 0, .x / rs_tons),
#       .names = "{.col}_per_ton"
#     )
#   )

message("--- Step 1 Completed ---")
#------------------------------------------------------------------------------#
# STEPS 2 - 5: Process Supply Chain Flows Year-by-Year
#------------------------------------------------------------------------------#
message("--- Pipeline Processing (Steps 2-5) [Start] ---")

for (current_year in all_years) {
  message("\n==================================================")
  message("PROCESSING YEAR: ", current_year)
  message("==================================================")

  version <- if_else(current_year %in% years_v261, "v2.6.1", "v2.6.0")
  s3_object <- paste0(
    "brazil/soy/sei_pcs/",
    version,
    "/with_exporter/SEIPCS_BRAZIL_SOY_",
    current_year,
    "_WITH_EXPORTER.parquet"
  )

  flows_raw <- s3read_using(
    FUN = read_parquet,
    show_col_types = FALSE,
    as.is = TRUE,
    object = s3_object,
    opts = c("check_region" = TRUE),
    bucket = "trase-storage"
  )

  # 1. Calculate total known export volume (in ktons) per municipality for this year
  muni_export_totals <- flows_raw |>
    filter(LVL6_TRASE_ID_PROD != "BR-XXXXXXX") |>
    group_by(LVL6_TRASE_ID_PROD) |>
    summarise(
      total_export_ktons = sum(VOLUME_RAW, na.rm = TRUE) / 1000,
      .groups = "drop"
    )

  # 2. Join exports with production metrics, apply override, and create tracking audit columns
  quants_year_adjusted <- quants_prod |>
    filter(year == current_year) |>
    left_join(
      muni_export_totals,
      by = join_by(trase_id == LVL6_TRASE_ID_PROD)
    ) |>
    replace_na(list(total_export_ktons = 0)) |>
    mutate(
      # COLUMN 1: Indicator flag (TRUE if exports exceeded physical RS production)
      is_prod_corrected = total_export_ktons > rs_tons,

      # COLUMN 2: The volume mismatch gap (positive value = amount added to enforce mass balance)
      prod_volume_diff = total_export_ktons - rs_tons,

      # Apply the actual dynamic baseline adjustment
      rs_tons_adjusted = if_else(is_prod_corrected, total_export_ktons, rs_tons)
    ) |>
    # Transmute and preserve our new audit metrics alongside the calculated rates
    transmute(
      trase_id,
      year,
      rs_tons = rs_tons_adjusted,
      is_prod_corrected,
      prod_volume_diff,
      across(
        soy_def_def5y:soy_def_harvest3y_5y_amort,
        ~ if_else(
          rs_tons_adjusted == 0 | is.na(rs_tons_adjusted) | is.na(.x),
          0,
          .x / rs_tons_adjusted
        ),
        .names = "{.col}_per_ton"
      )
    )

  # 3. Separate Known vs Unknown Origins from raw data
  flows_known <- flows_raw |> filter(LVL6_TRASE_ID_PROD != "BR-XXXXXXX")
  flows_unknown <- flows_raw |> filter(LVL6_TRASE_ID_PROD == "BR-XXXXXXX")

  # 4. Embed Known Flows (The audit columns will automatically attach here)
  flows_known_embedded <- flows_known |>
    left_join(
      quants_year_adjusted,
      by = join_by(YEAR == year, LVL6_TRASE_ID_PROD == trase_id)
    ) |>
    mutate(across(
      ends_with("_per_ton"),
      ~ .x * (VOLUME_RAW / 1000),
      .names = "{gsub('_per_ton', '_exp', .col)}"
    ))
  #--- Step 3 (Cont.): Embed Unknown Flows (Calculate leftovers) ---
  # Aggregate total known exposures calculated so far
  known_flows_agg <- flows_known_embedded |>
    group_by(YEAR) |>
    summarise(across(ends_with("_exp"), ~ sum(.x, na.rm = TRUE))) |>
    pivot_longer(-YEAR, names_to = "metric", values_to = "exp_val") |>
    mutate(metric = str_remove(metric, "_exp$"))

  # Get total original baseline tracking values for this year
  total_quants_year <- quants |>
    filter(year == current_year) |>
    summarise(across(starts_with("soy"), ~ sum(.x, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "metric", values_to = "total_val")

  # Calculate structural differences to distribute to unknowns
  quant_difference <- total_quants_year |>
    inner_join(known_flows_agg, by = "metric") |>
    mutate(difference = total_val - exp_val) |>
    # Key Fix: Keep ONLY the columns needed for the pivot to prevent multi-row blocks
    select(metric, difference) |>
    pivot_wider(names_from = metric, values_from = difference) |>
    mutate(LVL6_TRASE_ID_PROD = "BR-XXXXXXX")

  # Aggregate raw volume for unknown targets
  unknown_vol_agg <- flows_unknown |>
    group_by(YEAR) |>
    summarise(VOLUME_RAW_UNKNOWN = sum(VOLUME_RAW, na.rm = TRUE))

  # Build per-ton rates for unknown items and scale out to exposure
  flows_unknown_embedded <- flows_unknown |>
    left_join(quant_difference, by = "LVL6_TRASE_ID_PROD") |>
    left_join(unknown_vol_agg, by = "YEAR") |>
    mutate(across(
      starts_with("soy_def"),
      ~ (.x / (VOLUME_RAW_UNKNOWN / 1000)) * (VOLUME_RAW / 1000),
      .names = "{.col}_exp"
    )) |>
    # Drop intermediate calculation columns to match dataframe definitions seamlessly
    select(
      -VOLUME_RAW_UNKNOWN,
      -all_of(names(quant_difference)[
        names(quant_difference) != "LVL6_TRASE_ID_PROD"
      ])
    ) #--- Step 4: Combine Streams ---
  flows_full <- bind_rows(flows_known_embedded, flows_unknown_embedded)

  #--- Step 4 (Cont.): Automated Quality Assurance Checks ---
  message(">>> Running QA Validation checks...")

  # Check 1: Missing Entry Audit (Focusing strictly on shared exposure metrics)
  na_new <- nrow(
    flows_full |>
      filter(is.na(soy_def_harvest5y_exp) | is.na(soy_def_harvest3y_exp))
  )
  na_prod <- nrow(flows_full |> filter(is.na(VOLUME_RAW)))

  message(
    "  [QA 1/3] Missing Data Integrity: ",
    if_else(na_new == na_prod, "PASSED ✅", "FAILED ❌")
  )

  # Check 2: Row Count Stability Check
  message(
    "  [QA 2/3] Row Count Preservation: ",
    if_else(nrow(flows_raw) == nrow(flows_full), "PASSED ✅", "FAILED ❌")
  )

  # Check 3: Exposure Percentage Alignment Report
  sum_def_original <- quants |>
    filter(year == current_year) |>
    summarise(val = sum(soy_def_def5y, na.rm = TRUE)) |>
    pull(val)
  sum_def_embedded <- flows_full |>
    summarise(val = sum(soy_def_def5y_exp, na.rm = TRUE)) |>
    pull(val)
  perc_def <- (sum_def_embedded / sum_def_original) * 100

  message(
    "  [QA 3/3] Mass Balance Reconciliation: ",
    round(perc_def, 2),
    "% Embedded vs Source"
  )

  #--- Step 6: Test Aggregations (Uncommented Metrics View) ---
  message("\n>>> Top 3 Exporters by Deforestation Exposure:")
  flows_full |>
    group_by(EXPORTER_GROUP) |>
    summarise(
      product_vol_kton = sum(VOLUME_PRODUCT, na.rm = TRUE) / 1000,
      soy_def_5y_exp_kha = sum(soy_def_harvest5y_exp, na.rm = TRUE) / 1000,
      soy_def_3y_exp_kha = sum(soy_def_harvest3y_exp, na.rm = TRUE) / 1000
    ) |>
    arrange(desc(soy_def_5y_exp_kha)) |>
    head(3) |>
    print()

  #--- Step 5: Save System State ---
  output_path <- paste0(
    "~/documents/data/annual_metrics/soy_",
    current_year,
    "_post_embedding_quants_v3.parquet"
  )
  write_parquet(flows_full, output_path)
  message(">>> Successfully saved data state to: ", output_path)
}

message("\n--- Pipeline Complete! All years updated successfully ---")

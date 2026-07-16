# Load required libraries
library(tidyverse)
library(arrow)
library(aws.s3)
options(scipen = 9999)
library(glue)
library(readr)

# Authenticate AWS credentials
aws.signature::use_credentials()

# Configuration: file paths and parameters
# --------------------------------------------------------

# Paths to input and output files in S3
PATH_SPATIAL_METRICS <- '~/documents/data/annual_metrics/beef_annual_br_muni_v2.parquet'
CW_PRODUCTION <- "s3://trase-storage/brazil/production/statistics/anualpec/gold/cattle_production_4_annual_and_5year_2025-10-02.parquet"

PATH_SEIPCS_V221 <- 'brazil/beef/sei_pcs/v2.2.1/SEIPCS_BRAZIL_BEEF_{year}.csv'
PATH_OUTPUT_MUN_STATISTICS <- 'brazil/beef/sei_pcs/v2.2.1/municipality_production_{year}.parquet'
# PATH_OUTPUT = 'brazil/beef/sei_pcs/v2.2.1/post_embedding/quants_post_embedding_quants_{year}_c9.parquet'
PATH_OUTPUT <- '~/documents/data/annual_metrics/beef_{year}_post_embedding_quants_v2.parquet'


# List of columns that define individual flows
FLOWS_COLS <- c(
    'STATE_OF_PRODUCTION',
    'MUNICIPALITY',
    'LOGISTICS_HUB',
    'LOGISTICS_HUB_TRASE_ID',
    'PORT_OF_EXPORT',
    'EXPORTER',
    'STATE_OF_EXPORTER',
    'IMPORTER',
    'COUNTRY',
    'VOLUME_RAW',
    'VOLUME_PRODUCT',
    'FOB',
    'HS4',
    'HS6',
    'YEAR',
    'EXPORTER_CNPJ',
    'BRANCH',
    'GEOCODE_SOURCE',
    'ZERO_DEFORESTATION_BRAZIL_BEEF'
)

#Spatial metrics normalized per ton (to be multiplied by volume)
EMBEDDING_SPATIAL_METRICS_PER_TON <- c(
    'pasture_def_back_ha_per_ton',
    'pasture_def_annualized_back_ha_per_ton',
    'pasture_def_harvest3y_ha_per_ton',
    'pasture_def_harvest3y_ha_5y_amort_per_ton',
    'pasture_def_harvest5y_ha_per_ton',
    'pasture_def_harvest5y_ha_5y_amort_per_ton',
    'cattle_def_back_ha_5y_summed_ha_per_ton',
    'cattle_def_annualized_back_ha_5y_summed_ha_per_ton',
    'cattle_def_harvest5y_ha_5y_summed_ha_per_ton',
    'cattle_def_harvest3y_ha_5y_summed_ha_per_ton'
)

# Spatial metrics used for aggregation (not normalized per ton)
SPATIAL_METRICS_QUANTS <- gsub(
    "_per_ton",
    "",
    EMBEDDING_SPATIAL_METRICS_PER_TON
)
# SPATIAL_METRICS_QUANTS <- gsub("_PER_TON", "", SPATIAL_METRICS_QUANTS)

# Define which years use each SEI-PCS version
YEARS_V220 <- 2010:2020
YEARS_V221 <- 2021:2023
YEARS <- c(YEARS_V220, YEARS_V221)

# Load municipality-level spatial metrics from S3
# --------------------------------------------------------
quants_metrics <- read_parquet(PATH_SPATIAL_METRICS)
# quants_metrics <- s3read_using(
#     FUN = read_parquet,
#     sep = ";",
#     header = TRUE,
#     as.is = TRUE,
#     object = PATH_SPATIAL_METRICS,
#     opts = c("check_region" = TRUE),
#     bucket = "trase-storage"
# )

cw_production <- s3read_using(
    FUN = read_parquet,
    sep = ";",
    header = TRUE,
    as.is = TRUE,
    object = CW_PRODUCTION,
    opts = c("check_region" = TRUE),
    bucket = "trase-storage"
)
quants_metrics_cw <- quants_metrics |>
    left_join(cw_production, join_by(trase_id, year == YEAR))
quants_metrics_per_ton <- quants_metrics_cw |>
    transmute(
        year,
        ibge_munic,
        ibge_state,
        name,
        trase_id,
        variable = paste0(variable, "_per_ton"),
        ha = if_else(
            is.na(CW_PRODUCTION_TONS) | CW_PRODUCTION_TONS == 0,
            0,
            ha / CW_PRODUCTION_TONS
        )
    )
quants_metrics_per_ton |> pull(variable) |> unique()

quants_metrics_municipality <- quants_metrics |>
    bind_rows(quants_metrics_per_ton) |>
    pivot_wider(
        id_cols = c(trase_id, year, ibge_munic, ibge_state, name, year),
        values_from = ha,
        names_from = variable
    ) |>
    left_join(
        cw_production |>
            transmute(
                year = YEAR,
                trase_id,
                CW_PRODUCTION_TONS,
                CW_PRODUCTION_TONS_5_YR
            )
    )
# as_tibble() %>%
# filter(level == "municipality")

#########

# Helper functions
# --------------------------------------------------------

# Standardize municipality and state codes using TRASE ID pattern
fix_state_and_municipality_trase_ids <- function(df) {
    municipality <- df$MUNICIPALITY

    is_unknown_municipality <- municipality == "BR-XXXXXXX"
    is_known_municipality <- str_detect(municipality, "^BR-\\d{7}$")
    is_aggregated_municipality <- str_detect(
        municipality,
        "^BR-\\d{2}-AGGREGATED$"
    )

    if (
        !all(
            is_unknown_municipality |
                is_known_municipality |
                is_aggregated_municipality
        )
    ) {
        stop("The MUNICIPALITY column contains unexpected values.")
    }

    df <- df %>%
        mutate(StateCode = str_sub(MUNICIPALITY, 1, 5))

    df$MUNICIPALITY[is_aggregated_municipality] <- "BR-XXXXXXX"

    return(df)
}

# Filter production data for a specific year
get_year_production <- function(df, year) {
    df %>%
        filter(YEAR == !!year) %>%
        mutate(
            Prod = CW_PRODUCTION_TONS,
            StateCode = str_sub(TRASE_ID, 1, 5)
        )
}

# Aggregate volume by municipality
get_volume_by_municipality <- function(df, metric_name) {
    metric_sym <- rlang::sym(metric_name)
    df %>%
        group_by(YEAR, StateCode, MUNICIPALITY) %>%
        summarise(
            !!metric_sym := sum(VOLUME_RAW_T, na.rm = TRUE),
            .groups = "drop"
        )
}

# Main embedding loop per year
# --------------------------------------------------------

for (year in YEARS) {
    cat(">>> Processing year:", year, "\n")

    # Load SEI-PCS flows
    sei_year <- s3read_using(
        FUN = read_delim,
        delim = ";",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE,
        object = glue(PATH_SEIPCS_V221),
        opts = c("check_region" = TRUE),
        bucket = "trase-storage"
    ) %>%
        mutate(VOLUME_RAW_T = VOLUME_RAW / 1000)

    # Ensure schema compatibility by adding missing columns
    if (!"ZERO_DEFORESTATION_BRAZIL_BEEF" %in% names(sei_year)) {
        sei_year <- sei_year %>%
            mutate(ZERO_DEFORESTATION_BRAZIL_BEEF = 'UNKNOWN')
    }

    # Fix TRASE ID structure and treat aggregated municipalities as unknown
    sei_year <- fix_state_and_municipality_trase_ids(sei_year)

    # Split flows into known and unknown municipalities
    known_flows <- sei_year %>% filter(MUNICIPALITY != "BR-XXXXXXX")
    unknown_flows <- sei_year %>% filter(MUNICIPALITY == "BR-XXXXXXX")

    # Step 1: Get production data by municipality/state
    production_municipality <- get_year_production(
        quants_metrics_municipality |> mutate(TRASE_ID = trase_id, YEAR = year),
        year
    )
    production_state <- production_municipality %>%
        group_by(year, ibge_state) %>%
        summarise(
            Prod = sum(Prod, na.rm = TRUE),
            across(all_of(SPATIAL_METRICS_QUANTS), ~ sum(.x, na.rm = TRUE))
        )

    # Step 2: Get export volumes by municipality/state
    known_volume_mun_state <- get_volume_by_municipality(
        known_flows,
        'KnownExport'
    )
    unknown_volume_mun_state <- get_volume_by_municipality(
        unknown_flows,
        'UnknownExport'
    )

    # Step 3: Merge metrics and exports at municipality level
    municipality_metrics <- production_municipality |>
        left_join(
            known_volume_mun_state,
            join_by(YEAR, TRASE_ID == MUNICIPALITY, StateCode)
        )

    # Step 4: Separate partially and fully unknown exports
    unknown_export_state <- unknown_volume_mun_state %>%
        filter(StateCode != "BR-XX")

    fully_unknown_export <- unknown_volume_mun_state %>%
        filter(StateCode == "BR-XX")

    # Step 5: Calculate export shares by type (known, unknown, fully unknown)
    municipality_shares <- municipality_metrics %>%
        mutate(KnownExport = replace_na(KnownExport, 0)) %>%
        mutate(
            ExportKnownShare = pmin(KnownExport / Prod, 1),
            ExportRemaining = pmax(Prod - KnownExport, 0)
        )

    # Step 5.1: Calculate export shares by state
    state_shares <- production_state |>
        rename(YEAR = year) |>
        mutate(StateCode = paste0("BR-", ibge_state)) %>%
        left_join(
            known_volume_mun_state %>%
                group_by(YEAR, StateCode) %>%
                summarise(KnownExport = sum(KnownExport, na.rm = TRUE)),
            join_by(YEAR, StateCode)
        ) %>%
        left_join(
            unknown_export_state,
            join_by(YEAR, StateCode)
        ) %>%
        mutate(UnknownExport = replace_na(UnknownExport, 0)) %>%
        mutate(ExportKnownShare = pmin(KnownExport / Prod, 1)) %>%
        mutate(
            ExportUnknownShare = pmin(
                UnknownExport / Prod,
                1 - ExportKnownShare
            )
        )

    # Step 6: Calculate flow-level shares within each category
    known_flows <- known_flows %>%
        group_by(YEAR, MUNICIPALITY) %>%
        mutate(
            FlowKnownShare = VOLUME_RAW_T / sum(VOLUME_RAW_T, na.rm = TRUE)
        ) %>%
        ungroup()

    flows_unknown_state_known <- unknown_flows %>%
        filter(StateCode != "BR-XX") %>%
        group_by(YEAR, StateCode) %>%
        mutate(
            FlowUnknownShare = VOLUME_RAW_T / sum(VOLUME_RAW_T, na.rm = TRUE)
        ) %>%
        ungroup()

    flows_fully_unknown <- unknown_flows %>%
        filter(StateCode == "BR-XX") %>%
        mutate(
            FlowFullyUnknownShare = VOLUME_RAW_T /
                sum(VOLUME_RAW_T, na.rm = TRUE)
        )

    # Step 7: Calculate total volumes and shares for diagnostics
    total_production <- sum(municipality_metrics$Prod, na.rm = TRUE)
    total_known_flows <- sum(known_flows$VOLUME_RAW_T, na.rm = TRUE)
    total_unknown_state_known <- sum(
        flows_unknown_state_known$VOLUME_RAW_T,
        na.rm = TRUE
    )
    total_flows_fully_unknown <- sum(
        flows_fully_unknown$VOLUME_RAW_T,
        na.rm = TRUE
    )
    total_domestic <- total_production -
        (total_known_flows +
            total_unknown_state_known +
            total_flows_fully_unknown)

    print(total_domestic)
    print(total_production)

    known_flows_perc <- total_known_flows / total_production
    unknown_state_known_perc <- total_unknown_state_known / total_production
    flows_fully_unknown_perc <- total_flows_fully_unknown / total_production

    # Step 8: Embed spatial metrics into known flows
    known_flows <- known_flows |>
        left_join(
            municipality_shares |>
                ungroup() |>
                select(
                    YEAR,
                    TRASE_ID,
                    all_of(SPATIAL_METRICS_QUANTS),
                    ExportKnownShare
                ),
            join_by(YEAR, MUNICIPALITY == TRASE_ID)
        ) %>%
        mutate(
            across(
                all_of(SPATIAL_METRICS_QUANTS),
                ~ .x * ExportKnownShare * FlowKnownShare
            )
        )

    # Step 9: Embed spatial metrics into unknown flows (state known)
    flows_unknown_state_known <- flows_unknown_state_known %>%
        left_join(
            state_shares %>%
                select(
                    YEAR,
                    StateCode,
                    all_of(SPATIAL_METRICS_QUANTS),
                    ExportUnknownShare
                ),
            join_by(YEAR, StateCode)
        ) %>%
        mutate(
            across(
                all_of(SPATIAL_METRICS_QUANTS),
                ~ .x * ExportUnknownShare * FlowUnknownShare
            )
        )

    # Step 10: Embed spatial metrics into fully unknown flows
    # Compute available metrics to embed based on flow percentages
    total_by_metric <- municipality_shares %>%
        group_by(YEAR) %>%
        summarise(
            across(all_of(SPATIAL_METRICS_QUANTS), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop"
        )

    total_embedded_known <- known_flows %>%
        group_by(YEAR) %>%
        summarise(
            across(all_of(SPATIAL_METRICS_QUANTS), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop"
        )

    total_embedded_unknown <- flows_unknown_state_known %>%
        group_by(YEAR) %>%
        summarise(
            across(all_of(SPATIAL_METRICS_QUANTS), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop"
        )

    total_expected_fully_unknown <- total_by_metric %>%
        mutate(across(
            all_of(SPATIAL_METRICS_QUANTS),
            ~ .x * flows_fully_unknown_perc
        ))

    # If total_embedded_known is empty, set it to 0s with same structure
    if (nrow(total_by_metric) == 0) {
        total_by_metric <- tibble(
            YEAR = year,
            !!!setNames(
                as.list(rep(0, length(SPATIAL_METRICS_QUANTS))),
                SPATIAL_METRICS_QUANTS
            )
        )
        total_embedded_known <- total_by_metric
        total_embedded_unknown <- total_by_metric
        total_expected_fully_unknown <- total_by_metric
    }

    # Calculate metrics available to fully unknown flows
    left_metrics <- total_by_metric
    left_metrics[SPATIAL_METRICS_QUANTS] <- pmax(
        left_metrics[SPATIAL_METRICS_QUANTS] -
            total_embedded_known[SPATIAL_METRICS_QUANTS],
        0
    )

    left_metrics[SPATIAL_METRICS_QUANTS] <- pmax(
        left_metrics[SPATIAL_METRICS_QUANTS] -
            total_embedded_unknown[SPATIAL_METRICS_QUANTS],
        0
    )

    metrics_fully_unknown_to_embed <- left_metrics
    metrics_fully_unknown_to_embed[SPATIAL_METRICS_QUANTS] <- Map(
        pmin,
        left_metrics[SPATIAL_METRICS_QUANTS],
        total_expected_fully_unknown[SPATIAL_METRICS_QUANTS]
    )

    flows_fully_unknown <- flows_fully_unknown %>%
        left_join(metrics_fully_unknown_to_embed, join_by(YEAR)) %>%
        mutate(
            across(
                all_of(SPATIAL_METRICS_QUANTS),
                ~ .x * FlowFullyUnknownShare
            )
        )

    total_embedded_fully_unknown <- flows_fully_unknown %>%
        group_by(YEAR) %>%
        summarise(across(all_of(SPATIAL_METRICS_QUANTS), sum, na.rm = TRUE))

    # Step 11: Embed spatial metrics into domestic consumption flows
    domestic_embedded_metrics <- pmax(
        left_metrics[SPATIAL_METRICS_QUANTS] -
            total_embedded_fully_unknown[SPATIAL_METRICS_QUANTS],
        0
    )

    domestic_embedded_flows <- domestic_embedded_metrics %>%
        mutate(
            STATE_OF_PRODUCTION = "UNKNOWN STATE",
            MUNICIPALITY = "BR-XXXXXXX",
            LOGISTICS_HUB = "BR-BEEF-SLAUGHTERHOUSE-UNKNOWN",
            LOGISTICS_HUB_TRASE_ID = "BR-XXXXXXX",
            PORT_OF_EXPORT = "DOMESTIC PROCESSING AND CONSUMPTION",
            EXPORTER = "DOMESTIC PROCESSING AND CONSUMPTION",
            STATE_OF_EXPORTER = "DOMESTIC PROCESSING AND CONSUMPTION",
            IMPORTER = "DOMESTIC",
            COUNTRY = "BRAZIL",
            VOLUME_RAW = total_domestic * 1000,
            VOLUME_PRODUCT = NA_real_,
            FOB = 0,
            HS4 = "XXXX",
            HS6 = "XXXXXX",
            YEAR = year,
            EXPORTER_CNPJ = "DOMESTIC PROCESSING AND CONSUMPTION",
            BRANCH = "DOMESTIC",
            GEOCODE_SOURCE = "UNKNOWN",
            ZERO_DEFORESTATION_BRAZIL_BEEF = 'UNKNOWN'
        )

    # Step 12: Combine all flows into one output table
    all_flows <- bind_rows(
        known_flows,
        flows_unknown_state_known,
        flows_fully_unknown,
        domestic_embedded_flows
    ) %>%
        select(
            all_of(FLOWS_COLS),
            all_of(SPATIAL_METRICS_QUANTS),
            ExportKnownShare,
            ExportUnknownShare,
            FlowKnownShare,
            FlowUnknownShare,
            FlowFullyUnknownShare
        )

    # Step 13: Export result to S3
    write_parquet(all_flows, glue(PATH_OUTPUT))

    # s3write_using(
    #     all_flows,
    #     object = glue(PATH_OUTPUT),
    #     bucket = "trase-storage",
    #     FUN = write_parquet,
    #     opts = c("check_region" = T, multipart = TRUE)
    # )
}

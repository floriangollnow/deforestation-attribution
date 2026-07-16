library(tidyverse)
library(geofacet)
library(sf)
library(zoo)
library(aws.signature)
library(aws.s3)
library(googledrive)
library(arrow)
options(scipen = 999)
drive_auth()

###########################
# CW production
##########################
cw_production <- s3read_using(
  FUN = read_parquet,
  sep = ";",
  header = TRUE,
  as.is = TRUE,
  object = "s3://trase-storage/brazil/production/statistics/anualpec/gold/cattle_production_4_annual_and_5year_2025-10-02.parquet",
  opts = c("check_region" = TRUE),
  bucket = "trase-storage"
)

###############################
## loading GEE assets
###############################

drive_download(
  "hidden_pasture_deforestation_glad_mb_orig_q4_backward_v2_2025_fix.csv",
  "~/documents/data/annual_metrics/hidden_pasture_deforestation_glad_mb_orig_q4_backward_v2_2025_fix.csv",
  overwrite = T
)
hidden_def_orig <- read_csv(
  "~/documents/data/annual_metrics/hidden_pasture_deforestation_glad_mb_orig_q4_backward_v2_2025_fix.csv"
)
#hidden_def_orig #<- hidden_def_orig |> as_tibble() |> mutate(pasture_def_5y_annualized_back= )

## new assets
drive_download(
  "pasture_def_5yr_windowSize_glad_mb_orig_q4_v2_2025_fix.csv",
  "~/documents/data/annual_metrics/pasture_def_5yr_windowSize_glad_mb_orig_q4_v2_2025_fix.csv",
  overwrite = T
)
hidden_def_5yr <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_5yr_windowSize_glad_mb_orig_q4_v2_2025_fix.csv"
)

drive_download(
  "pasture_def_3yr_windowSize_glad_mb_orig_q4_v2_2025_fix.csv",
  "~/documents/data/annual_metrics/pasture_def_3yr_windowSize_glad_mb_orig_q4_v2_2025_fix.csv",
  overwrite = T
)
hidden_def_3yr <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_3yr_windowSize_glad_mb_orig_q4_v2_2025_fix.csv"
)

drive_download(
  "pasture_def_3yr_windowSize_glad_mb8_orig_q4_v2_2025_fix.csv",
  "~/documents/data/annual_metrics/pasture_def_3yr_windowSize_glad_mb8_orig_q4_v2_2025_fix.csv",
  overwrite = T
)
hidden_def_3yr_mb8 <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_3yr_windowSize_glad_mb8_orig_q4_v2_2025_fix.csv"
)

# drive_download(
#   "pasture_def_5yr_windowSize_glad_mb_orig_q4_forest_v2_2025_fix.csv",
#   "~/documents/data/annual_metrics/pasture_def_5yr_windowSize_glad_mb_orig_q4_forest_v2_2025_fix.csv",
#   overwrite = T
# )
# hidden_def_6yr_forest <- read_csv(
#   "~/downloads/pasture_def_6yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv"
# )
# drive_download(
#   "pasture_def_6yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   "~/downloads/pasture_def_6yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   overwrite = T
# )
hidden_def_5yr_forest <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_6yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv"
)

# drive_download(
#   "pasture_def_4yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv",
#   "~/downloads/pasture_def_4yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_forest <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_4yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv"
)
# drive_download(
#   "pasture_def_4yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   "~/downloads/pasture_def_4yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_forest <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_4yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv"
)

drive_download(
  "pasture_def_gpw_v1_5yr_gpw_v1_cultivated_only_fix.csv",
  "~/documents/data/annual_metrics/pasture_def_gpw_v1_5yr_gpw_v1_cultivated_only_fix.csv",
  overwrite = T
)
hidden_def_3yr_gpw <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_gpw_v1_5yr_gpw_v1_cultivated_only_fix.csv"
)


#
# drive_download(
#   "pasture_def_3yr_windowSize_glad_mb_orig_q4_GFC_v2_2025.csv",
#   "~/downloads/pasture_def_3yr_windowSize_glad_mb_orig_q4_GFC_v2_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_gfc <- read_csv(
  "~/documents/data/annual_metrics/pasture_def_3yr_windowSize_glad_mb_orig_q4_GFC_v2_2025.csv"
)
deduce <- read_csv(
  "~/documents/data/annual_metrics/export_1773327070032_7097dfca-000000000000.csv"
)
deduce <- deduce |>
  filter(Commodity == "Cattle meat")
deduce_br <- deduce |>
  filter(Commodity == "Cattle meat") |>
  group_by(Year) |>
  summarize(ha = sum(`Deforestation attribution_ unamortized _ha_`)) |>
  mutate(variable = "deduce_unarmotized")

## state dictionary
state_names <- matrix(
  c(
    11,
    "Rondônia",
    "RO",
    12,
    "Acre",
    "AC",
    13,
    "Amazonas",
    "AM",
    14,
    "Roraima",
    "RR",
    15,
    "Pará",
    "PA",
    16,
    "Amapá",
    "AP",
    17,
    "Tocantins",
    "TO",
    21,
    "Maranhão",
    "MA",
    22,
    "Piauí",
    "PI",
    23,
    "Ceará",
    "CE",
    24,
    "Rio Grande do Norte",
    "RN",
    25,
    "Paraíba",
    "PB",
    26,
    "Pernambuco",
    "PE",
    27,
    "Alagoas",
    "AL",
    28,
    "Sergipe",
    "SE",
    29,
    "Bahia",
    "BA",
    31,
    "Minas Gerais",
    "MG",
    32,
    "Espírito Santo",
    "ES",
    33,
    "Rio de Janeiro",
    "RJ",
    35,
    "São Paulo",
    "SP",
    41,
    "Paraná",
    "PR",
    42,
    "Santa Catarina",
    "SC",
    43,
    "Rio Grande do Sul",
    "RS",
    50,
    "Mato Grosso do Sul",
    "MS",
    51,
    "Mato Grosso",
    "MT",
    52,
    "Goiás",
    "GO",
    53,
    "Distrito Federal",
    "DF"
  ),
  byrow = TRUE,
  ncol = 3
)

# Optional: Adding column names for better readability
colnames(state_names) <- c("code", "name", "abbreviation")
state_names <- as_tibble(state_names)

# add to deduce:
deduce <- deduce |>
  mutate(
    abbreviation = str_extract(`Producer subregion`, "(?<=\\()[A-Z]{2}(?=\\))")
  )

deduce <- deduce |> left_join(state_names)


#################################
# to long
#################################
hidden_def_orig_l <- hidden_def_orig |>
  as_tibble() |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "_back")
  ) |>
  filter(variable == "pasture_def_back")

hidden_def_orig_l_ann <- hidden_def_orig_l |>
  mutate(ha = ha / 5, variable = "pasture_def_annualized_back")
hidden_def_orig_l <- hidden_def_orig_l |> bind_rows(hidden_def_orig_l_ann)


hidden_def_5yr_l <- hidden_def_5yr |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "5y")
  )

hidden_def_3yr_l <- hidden_def_3yr |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y")
  )


hidden_def_5yr_forest_l <- hidden_def_5yr_forest |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "5y_forest")
  )

hidden_def_3yr_forest_l <- hidden_def_3yr_forest |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y_forest")
  )

hidden_def_3yr_gpw_l <- hidden_def_3yr_gpw |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y_gpw")
  )
hidden_def_3yr_mb8_l <- hidden_def_3yr_mb8 |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y_mb8")
  )

hidden_def_3yr_gfc_l <- hidden_def_3yr_gfc |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y_gfc")
  )

# combine for analysise

hidden_def_new <- hidden_def_5yr_l |>
  mutate(
    ibge_munic = as.character(ibge_munic),
    ibge_state = as.character(ibge_state)
  ) |>
  bind_rows(
    hidden_def_3yr_l |>
      mutate(
        ibge_munic = as.character(ibge_munic),
        ibge_state = as.character(ibge_state)
      )
  ) |>
  bind_rows(
    hidden_def_5yr_forest_l |>
      mutate(
        ibge_munic = as.character(ibge_munic),
        ibge_state = as.character(ibge_state)
      )
  ) |>
  bind_rows(
    hidden_def_3yr_forest_l |>
      mutate(
        ibge_munic = as.character(ibge_munic),
        ibge_state = as.character(ibge_state)
      )
  ) |>
  bind_rows(
    hidden_def_3yr_gpw_l |>
      mutate(
        ibge_munic = as.character(ibge_munic),
        ibge_state = as.character(ibge_state)
      )
  ) |>
  bind_rows(
    hidden_def_3yr_mb8_l |>
      mutate(
        ibge_munic = as.character(ibge_munic),
        ibge_state = as.character(ibge_state)
      )
  ) |>
  bind_rows(
    hidden_def_3yr_gfc_l |>
      mutate(
        ibge_munic = as.character(ibge_munic),
        ibge_state = as.character(ibge_state)
      )
  ) |>
  bind_rows(
    hidden_def_orig_l |>
      mutate(
        ibge_munic = as.character(ibge_munic),
        ibge_state = as.character(ibge_state)
      )
  )

## add cw production data
# cw_production_l <- cw_production |>
#   select(-CW_PRODUCTION_TONS_5_YR) |>
#   pivot_longer(
#     cols = c(CW_PRODUCTION_TONS),
#     values_to = "value",
#     names_to = "variable"
#   )
cw_production <- cw_production |>
  rename(year = YEAR) |>
  mutate(
    ibge_munic = str_sub(trase_id, start = 4),
    ibge_state = str_sub(state_trase_id, start = 4)
  ) |>
  select(-c(state_trase_id, STATE))

# hidden_def_new <- hidden_def_new |> rename(value=ha) |>
#   bind_rows(
#     cw_production_l
#   )

# deforestaion for beef is 5 year deforestation dividied by 5 year production tons

hidden_def_new_means_sums <- hidden_def_new |>
  ungroup() |>
  group_by(ibge_munic, ibge_state, name, trase_id, variable) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    ha_5y_amort = rollmean(
      ha,
      k = 5,
      align = "right",
      fill = NA
    ),
    ha_5y_summed = rollsum(
      ha,
      k = 5,
      align = "right",
      fill = NA
    )
  )

# cw_production_mean_sum <- hidden_def_new_means |>
#   filter(variable == "CW_PRODUCTION_TONS")
# hidden_def_new_means_sum <- hidden_def_new_means |>
#   filter(variable != "CW_PRODUCTION_TONS")

hidden_def_new_means_sum_l <- hidden_def_new_means_sums |>
  pivot_longer(
    cols = c(ha, ha_5y_amort, ha_5y_summed),
    names_to = "type",
    values_to = "ha1"
  ) |>
  mutate(
    variable = paste0(variable, "_", type)
  ) |>
  rename(ha = ha1) |>
  select(-type)


cattle <- hidden_def_new_means_sum_l |>
  filter(str_ends(variable, "summed")) |>
  left_join(cw_production, join_by(trase_id, year == YEAR))
cattle_def_per_ton <- cattle |>
  mutate(ha_per_ton = ha / CW_PRODUCTION_TONS_5_YR) |>
  filter(year >= 2010)
cattle_def_per_ton_cleaned <- cattle_def_per_ton |>
  mutate(
    variable = paste0(gsub("pasture", "cattle", variable), "_per_ton"),
    #ha_per_ton = cattle_def_per_ton
  )

# from ton to def - using annual tons
cattle_def <- cattle_def_per_ton_cleaned |>
  mutate(cattle_def = ha_per_ton * CW_PRODUCTION_TONS)


cattle_def_cleaned <- cattle_def |>
  mutate(
    variable = gsub("per_ton", "ha", variable),
    ha = cattle_def
  ) |>
  select(
    -c(
      CW_PRODUCTION_TONS,
      CW_PRODUCTION_TONS_5_YR,
      cattle_def,
      #cattle_def_per_ton,
      ha_per_ton
    )
  )

hidden_def_new_means_sum_l <- hidden_def_new_means_sum_l |>
  bind_rows(cattle_def_cleaned)


# hidden_def_new_means_sum_l |> filter(ibge_munic=="1505031", year==2020, variable=="pasture_def_back_ha"|variable=="pasture_def_annualized_back_ha"|variable=="pasture_def_annualized_back_ha_5y_summed") |> tail()

write_parquet(
  hidden_def_new_means_sum_l,
  "~/documents/data/annual_metrics/beef_annual_br_muni_v2.parquet"
)

###############################
## plots country levelh
###############################
# aggregate data & # add deduce
hidden_def_new_agg <- hidden_def_new_means_sum_l |>
  ungroup() |>
  group_by(year, variable) |>
  summarise(ha = sum(ha, na.rm = TRUE)) |>
  bind_rows(deduce_br |> rename(year = Year))


#hidden_def_new_agg |> filter(variable=="pasture_def_back_ha"|variable=="pasture_def_annualized_back_ha"|variable=="pasture_def_annualized_back_ha_5y_summed") |> tail()

write_parquet(
  hidden_def_new_agg,
  "~/documents/data/annual_metrics/beef_annual_br_v2.parquet"
)

#####################################################
## by state
#####################################################

hidden_def_new_state <- hidden_def_new_means_sum_l |>
  mutate(ibge_state = as.character(ibge_state)) |>
  ungroup() |>
  group_by(year, ibge_state, variable) |>
  summarise(ha = sum(ha, na.rm = TRUE)) |>
  left_join(state_names, join_by(ibge_state == code)) |>
  rename(code = ibge_state)

deduce_def_by_state <- deduce |>
  ungroup() |>
  group_by(Year, code, abbreviation) |>
  summarise(
    ha = sum(`Deforestation attribution_ unamortized _ha_`, na.rm = TRUE)
  ) |>
  mutate(variable = "deduce_unarmotized", year = Year) |>
  ungroup() |>
  select(-Year)

hidden_def_new_state_all <- hidden_def_new_state |>
  bind_rows(deduce_def_by_state)

write_parquet(
  hidden_def_new_state_all,
  "~/documents/data/annual_metrics/beef_annual_br_states_v2.parquet"
)

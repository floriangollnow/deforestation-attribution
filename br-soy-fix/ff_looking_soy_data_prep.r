# ff looking data prep

library(tidyverse)
library(geofacet)
library(sf)
#library(zoo)
library(slider)
library(aws.signature)
library(aws.s3)
library(googledrive)
library(arrow)
options(scipen = 999)
drive_auth()

###############################
## loading GEE assets
###############################
# drive_download(
#   "hidden_soy_deforestation_glad_mb_orig_q4_v2_2025.geojson",
#   "~/downloads/hidden_soy_deforestation_glad_mb_orig_q4_v2_2025.geojson",
#   overwrite = T
# )
# hidden_def_orig <- read_sf(
#   "~/downloads/hidden_soy_deforestation_glad_mb_orig_q4_v2_2025.geojson"
# )
# drive_download(
#   "hidden_soy_deforestation_glad_mb_orig_q4_backward_forest_v4_2025.csv",
#   "~/documents/data/hidden_soy_deforestation_glad_mb_orig_q4_backward_forest_v4_2025.csv",
#   overwrite = T
# )
hidden_def_forest_orig <- read_csv(
  "~/documents/data/hidden_soy_deforestation_glad_mb_orig_q4_backward_forest_v4_2025.csv"
)
# drive_download(
#   "hidden_soy_deforestation_glad_mb_orig_q4_backward_non_forest_v4_2025.csv",
#   "~/documents/data/hidden_soy_deforestation_glad_mb_orig_q4_backward_non_forest_v4_2025.csv",
#   overwrite = T
# )
hidden_def_non_forest_orig <- read_csv(
  "~/documents/data/hidden_soy_deforestation_glad_mb_orig_q4_backward_non_forest_v4_2025.csv"
)


## new assets
# drive_download(
#   "soy_def_5yr_windowSize_glad_mb_orig_q4_forest_v4_2025.csv",
#   "~/documents/data/annual_metrics/soy_def_5yr_windowSize_glad_mb_orig_q4_forest_v4_2025.csv",
#   overwrite = T
# )
hidden_def_5yr_forest <- read_csv(
  "~/documents/data/annual_metrics/soy_def_5yr_windowSize_glad_mb_orig_q4_forest_v4_2025.csv"
)
# drive_download(
#   "soy_def_5yr_windowSize_glad_mb_orig_q4_forest_v4_2025.csv",
#   "~/documents/data/annual_metrics/soy_def_5yr_windowSize_glad_mb_orig_q4_non_forest_v4_2025.csv",
#   overwrite = T
# )
hidden_def_5yr_non_forest <- read_csv(
  "~/documents/data/annual_metrics/soy_def_5yr_windowSize_glad_mb_orig_q4_non_forest_v4_2025.csv"
)

# drive_download(
#   "soy_def_3yr_windowSize_glad_mb_orig_q4_forest_v4_2025.csv",
#   "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_forest_v4_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_forest <- read_csv(
  "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_forest_v4_2025.csv"
)
# drive_download(
#   "soy_def_3yr_windowSize_glad_mb_orig_q4_non_forest_v4_2025.csv",
#   "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_non_forest_v4_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_non_forest <- read_csv(
  "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_non_forest_v4_2025.csv"
)

# # drive_download(
# #   "soy_def_6yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv",
# #   "~/downloads/soy_def_6yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv",
# #   overwrite = T
# # )
# hidden_def_6yr_forest <- read_csv(
#   "~/downloads/soy_def_6yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv"
# )
# drive_download(
#   "soy_def_6yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   "~/downloads/soy_def_6yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   overwrite = T
# )
hidden_def_5yr_forest <- read_csv(
  "~/documents/data/annual_metrics/soy_def_6yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv"
)

# # drive_download(
# #   "soy_def_4yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv",
# #   "~/downloads/soy_def_4yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv",
# #   overwrite = T
# # )
# hidden_def_3yr_forest <- read_csv(
#   "~/downloads/soy_def_4yr_windowSize_glad_mb_orig_q4_forest_v2_2025.csv"
# )
# drive_download(
#   "soy_def_3yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   "~/downloads/soy_def_3yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_forest <- read_csv(
  "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_forest_prodes_mb_v2_2025.csv"
)

## JRC forest
# drive_download(
#   "soy_def_3yr_windowSize_glad_mb_orig_q4_forest_JRC_2020_v3_v2_2025.csv",
#   "~/downloads/soy_def_3yr_windowSize_glad_mb_orig_q4_forest_JRC_2020_v3_v2_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_JRCforest <- read_csv(
  "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_forest_JRC_2020_v3_v2_2025.csv"
)


# drive_download(
#   "soy_def_3yr_windowSize_glad_mb_orig_q4_GFC_v3_2025.csv",
#   "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_GFC_v3_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_gfc <- read_csv(
  "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_mb_orig_q4_GFC_v3_2025.csv"
)

# drive_download(
#   "soy_def_3yr_windowSize_glad_orig_q4_GFC_v2_2025.csv",
#   "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_orig_q4_GFC_v2_2025.csv",
#   overwrite = T
# )
hidden_def_3yr_gfc_only <- read_csv(
  "~/documents/data/annual_metrics/soy_def_3yr_windowSize_glad_orig_q4_GFC_v2_2025.csv"
)
deduce_br_spatial <- read_csv(
  "~/documents/data/annual_metrics/Forest_loss_to_CLASSIFICATION_Brazil_PythonAPI_2024-06-01T213244.csv"
) |>
  filter(Class == 3242) |>
  pivot_longer(
    -c(CONTINENT, COUNTRY, GID_0, GID_1, GID_2, Class),
    names_to = "type",
    values_to = "deduce_spatial_br_sdef"
  ) |>
  mutate(
    year = as.numeric(str_sub(type, start = -4)),
    variable = "deduce_spatial"
  ) |>
  select(-c(CONTINENT, COUNTRY, Class, type))

#GDAM GID dictionary()
GDAM_GID <- matrix(
  c(
    "Acre",
    "12",
    "BRA.1_1",
    "Alagoas",
    "27",
    "BRA.2_1",
    "Amapá",
    "16",
    "BRA.3_1",
    "Amazonas",
    "13",
    "BRA.4_1",
    "Bahia",
    "29",
    "BRA.5_1",
    "Ceará",
    "23",
    "BRA.6_1",
    "Distrito Federal",
    "53",
    "BRA.7_1",
    "Espírito Santo",
    "32",
    "BRA.8_1",
    "Goiás",
    "52",
    "BRA.9_1",
    "Maranhão",
    "21",
    "BRA.10_1",
    "Mato Grosso",
    "51",
    "BRA.11_1",
    "Mato Grosso do Sul",
    "50",
    "BRA.12_1",
    "Minas Gerais",
    "31",
    "BRA.13_1",
    "Pará",
    "15",
    "BRA.14_1",
    "Paraíba",
    "25",
    "BRA.15_1",
    "Paraná",
    "41",
    "BRA.16_1",
    "Pernambuco",
    "26",
    "BRA.17_1",
    "Piauí",
    "22",
    "BRA.18_1",
    "Rio de Janeiro",
    "33",
    "BRA.19_1",
    "Rio Grande do Norte",
    "24",
    "BRA.20_1",
    "Rio Grande do Sul",
    "43",
    "BRA.21_1",
    "Rondônia",
    "11",
    "BRA.22_1",
    "Roraima",
    "14",
    "BRA.23_1",
    "Santa Catarina",
    "42",
    "BRA.24_1",
    "São Paulo",
    "35",
    "BRA.25_1",
    "Sergipe",
    "28",
    "BRA.26_1",
    "Tocantins",
    "17",
    "BRA.27_1"
  ),
  byrow = TRUE,
  ncol = 3
)
colnames(GDAM_GID) <- c("state", "ibge_state", "gadm_gid")
GDAM_GID <- as_tibble(GDAM_GID)
deduce_br_spatial <- deduce_br_spatial |>
  left_join(GDAM_GID, join_by(GID_1 == gadm_gid)) |>
  select(-c(GID_0, GID_1, GID_2))

deduce <- read_csv(
  "~/documents/data/annual_metrics/export_1773327070032_7097dfca-000000000000.csv"
) |>
  filter(Commodity == "Soya beans")
deduce_br <- deduce |>
  filter(Commodity == "Soya beans") |>
  group_by(Year) |>
  summarize(ha = sum(`Deforestation attribution_ unamortized _ha_`)) |>
  mutate(variable = "deduce_unarmotized")

##amortize deduce:
deduce_br_amort <- deduce_br |>
  ungroup() |>
  group_by(variable) |>
  arrange(Year, .by_group = TRUE) |>
  mutate(
    ha_5y_amort = slide_index_dbl(
      ha,
      .i = Year, # Explicitly look at the calendar year
      .f = mean,
      na.rm = TRUE, # Apply mean
      .before = 4, # Current year + 4 years prior = 5-year window
      .complete = TRUE # Returns NA if a full 5-year window doesn't exist
    )
  ) |>
  ungroup() |>
  # Filter out the NAs (the first 4 years) so you don't append empty rows
  filter(!is.na(ha_5y_amort)) |>
  mutate(
    variable = paste0(variable, "_5y_amortized"),
    ha = ha_5y_amort
  ) |>
  select(-ha_5y_amort)

# Combine back to original dataset
deduce_br <- deduce_br |> bind_rows(deduce_br_amort)

#####

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
deduce_spatial <- deduce_br_spatial |>
  left_join(state_names, join_by(ibge_state == code)) |>
  select(-state)
#################################
# to long
#################################
hidden_def_forest_orig_l <- hidden_def_forest_orig |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "_forest_back")
  )

hidden_def_non_forest_orig_l <- hidden_def_non_forest_orig |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "_non_forest_back")
  )

hidden_def_total_orig_l <- hidden_def_forest_orig_l |>
  bind_rows(hidden_def_non_forest_orig_l) |>
  pivot_wider(
    id_cols = c(year, ibge_munic, ibge_state, name, trase_id),
    names_from = "variable",
    values_from = "ha"
  ) |>
  mutate(
    soy_def_5y_back = soy_def_5y_forest_back + soy_def_5y_non_forest_back,
    soy_def_5y_annualized_back = soy_def_5y_annualized_forest_back +
      soy_def_5y_annualized_non_forest_back
  ) |>
  pivot_longer(
    -c(year, ibge_munic, ibge_state, name, trase_id),
    names_to = "variable",
    values_to = "ha"
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
hidden_def_5yr_non_forest_l <- hidden_def_5yr_non_forest |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "5y_non_forest")
  )

hidden_def_5yr_total_l <- hidden_def_5yr_forest_l |>
  bind_rows(hidden_def_5yr_non_forest_l) |>
  pivot_wider(
    id_cols = c(year, ibge_munic, ibge_state, name, trase_id),
    names_from = "variable",
    values_from = "ha"
  ) |>
  mutate(
    soy_def_def5y = soy_def_def5y_forest + soy_def_def5y_non_forest,
    soy_def_harvest5y = soy_def_harvest5y_forest + soy_def_harvest5y_non_forest
  ) |>
  pivot_longer(
    -c(year, ibge_munic, ibge_state, name, trase_id),
    names_to = "variable",
    values_to = "ha"
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

hidden_def_3yr_non_forest_l <- hidden_def_3yr_non_forest |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y_non_forest")
  )

hidden_def_3yr_total_l <- hidden_def_3yr_forest_l |>
  bind_rows(hidden_def_3yr_non_forest_l) |>
  pivot_wider(
    id_cols = c(year, ibge_munic, ibge_state, name, trase_id),
    names_from = "variable",
    values_from = "ha"
  ) |>
  mutate(
    soy_def_def3y = soy_def_def3y_forest + soy_def_def3y_non_forest,
    soy_def_harvest3y = soy_def_harvest3y_forest + soy_def_harvest3y_non_forest
  ) |>
  pivot_longer(
    -c(year, ibge_munic, ibge_state, name, trase_id),
    names_to = "variable",
    values_to = "ha"
  )


# hidden_def_5yr_forest_l <- hidden_def_5yr_forest |>
#   select(-c(`system:index`, .geo)) |>
#   pivot_longer(
#     -c(ibge_munic, ibge_state, name, trase_id),
#     names_to = 'variable',
#     values_to = 'ha'
#   ) |>
#   mutate(
#     year = as.numeric(str_sub(variable, start = -4)),
#     variable = paste0(str_sub(variable, end = -6), "5y_forest")
#   )

# hidden_def_3yr_forest_l <- hidden_def_3yr_forest |>
#   select(-c(`system:index`, .geo)) |>
#   pivot_longer(
#     -c(ibge_munic, ibge_state, name, trase_id),
#     names_to = 'variable',
#     values_to = 'ha'
#   ) |>
#   mutate(
#     year = as.numeric(str_sub(variable, start = -4)),
#     variable = paste0(str_sub(variable, end = -6), "3y_forest")
#   )

hidden_def_3yr_JRCforest_l <- hidden_def_3yr_JRCforest |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y_JRCforest")
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

hidden_def_3yr_gfc_only_l <- hidden_def_3yr_gfc_only |>
  select(-c(`system:index`, .geo)) |>
  pivot_longer(
    -c(ibge_munic, ibge_state, name, trase_id),
    names_to = 'variable',
    values_to = 'ha'
  ) |>
  mutate(
    year = as.numeric(str_sub(variable, start = -4)),
    variable = paste0(str_sub(variable, end = -6), "3y_gfc_only")
  )

# combine for analysise

hidden_def_new <- hidden_def_5yr_total_l |>
  bind_rows(hidden_def_3yr_total_l) |>
  bind_rows(hidden_def_3yr_JRCforest_l) |>
  bind_rows(hidden_def_3yr_gfc_l) |>
  bind_rows(hidden_def_3yr_gfc_only_l) |>
  bind_rows(hidden_def_total_orig_l)

# hidden_def_new_amort <- hidden_def_new |>
#   ungroup() |>
#   group_by(ibge_munic, ibge_state, name, trase_id, variable) |>
#   arrange(year, .by_group = TRUE) |>
#   mutate(
#     ha_5y_amort = rollmean(
#       ha,
#       k = 5,
#       align = "right",
#       fill = NA,
#       na.rm = TRUE
#     )
#   ) |>
#   ungroup() |>
#   mutate(variable = paste0(variable, "_5y_amortized"), ha = ha_5y_amort) |>
#   select(-ha_5y_amort)

# hidden_def_new <- hidden_def_new |> bind_rows(hidden_def_new_amort)

hidden_def_new_amort <- hidden_def_new |>
  ungroup() |>
  group_by(ibge_munic, ibge_state, name, trase_id, variable) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    ha_5y_amort = slide_index_dbl(
      ha,
      .i = year, # Explicitly look at the calendar year
      .f = mean,
      na.rm = TRUE, # Apply mean
      .before = 4, # Current year + 4 years prior = 5-year window
      .complete = TRUE # Returns NA if a full 5-year window doesn't exist
    )
  ) |>
  ungroup() |>
  # Filter out the NAs (the first 4 years) so you don't append empty rows
  filter(!is.na(ha_5y_amort)) |>
  mutate(
    variable = paste0(variable, "_5y_amortized"),
    ha = ha_5y_amort
  ) |>
  select(-ha_5y_amort)

# Combine back to original dataset
hidden_def_new <- hidden_def_new |> bind_rows(hidden_def_new_amort)

###############################
## plots municipality level
###############################

write_parquet(
  hidden_def_new,
  "~/documents/data/annual_metrics/soy_annual_br_muni_v3.parquet"
)

###############################
## plots country level
###############################
# aggregate data & # add deduce
deduce_spatial_agg <- deduce_spatial |>
  group_by(year, variable) |>
  summarise(ha = sum(deduce_spatial_br_sdef, na.rm = T))

hidden_def_new_agg <- hidden_def_new |>
  group_by(year, variable) |>
  summarise(ha = sum(ha)) |>
  bind_rows(deduce_br |> rename(year = Year)) |>
  bind_rows(deduce_spatial_agg)


write_parquet(
  hidden_def_new_agg,
  "~/documents/data/annual_metrics/soy_annual_br_v3.parquet"
)

#####################################################
## by state
#####################################################

hidden_def_new_state <- hidden_def_new |>
  mutate(ibge_state = as.character(ibge_state)) |>
  group_by(year, ibge_state, variable) |>
  summarise(ha = sum(ha)) |>
  left_join(state_names, join_by(ibge_state == code)) |>
  rename(code = ibge_state)

deduce_def_by_state <- deduce |>
  group_by(Year, code, abbreviation) |>
  summarise(ha = sum(`Deforestation attribution_ unamortized _ha_`)) |>
  mutate(variable = "deduce_unarmotized", year = Year) |>
  ungroup() |>
  select(-Year)

hidden_def_new_state_all <- hidden_def_new_state |>
  bind_rows(deduce_def_by_state)

write_parquet(
  hidden_def_new_state_all,
  "~/documents/data/annual_metrics/soy_annual_br_states_v3.parquet"
)

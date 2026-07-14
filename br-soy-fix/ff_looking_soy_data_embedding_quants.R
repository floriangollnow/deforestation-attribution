# Embedding of soy deforestation avoinding normalization of deforestation.

# this means I take the remotely sensed soy area as truth rather than the ibge reported and calculate production using IBGE reported yields.
# this ensures that the tons you are dividing by actually correspond to the physical hectares you are monitoring for deforestation.

# what if there is more export than production in once municpality?
#   - we take the export volume
# embedded total soy deforestation needs to be tha same as total soy deforestation

library(tidyverse)
library(arrow)
library(aws.s3)
library(zoo)
options(scipen = 9999)
aws.signature::use_credentials()

#-------- Step 1 --------#
#-- reading all quants --#
#------------------------#
print(paste0("reading all quants start: ", Sys.time()))
# quants_def <- s3read_using(
#     FUN = read.table,
#     sep = ";",
#     header = TRUE,
#     as.is = TRUE,
#     object = "brazil/soy/indicators/out/q4_2023/soy_deforestation_2013_2023_q3_2024_multilevel.csv",
#     opts = c("check_region" = TRUE),
#     bucket = "trase-storage"
# ) %>%
#     as_tibble() %>%
#     filter(level == "municipality") %>%
#     select(TRASE_ID, YEAR, SOY_DEF_NORM, SOY_DEF_NORM4TON, YIELD)

# quants_ghg <- s3read_using(
#     FUN = read.table,
#     sep = ";",
#     header = TRUE,
#     as.is = TRUE,
#     object = "brazil/soy/indicators/out/q4_2023/GHG_soy_deforestation_2013_2023_q3_2024_multilevel.csv",
#     opts = c("check_region" = TRUE),
#     bucket = "trase-storage"
# ) %>%
#     as_tibble() %>%
#     filter(level == "municipality") %>%
#     select(
#         TRASE_ID,
#         YEAR,
#         SOY_GrossDefGHG_NORM_tCO2eq,
#         SOY_NetDefGHG_NORM_tCO2eq,
#         SOY_GrossDefGHG_NORM_tCO2eq_perSoyTon,
#         SOY_NetDefGHG_NORM_tCO2eq_perSoyTon
#     )

# # combining quants
# quants <- quants_def %>% left_join(quants_ghg)

quants <- read_parquet(
    "~/documents/data/annual_metrics/soy_annual_br_muni.parquet"
) |>
    pivot_wider(
        id_cols = c(ibge_munic, ibge_state, name, trase_id, year),
        names_from = variable,
        values_from = ha
    )
quants <- quants |>
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
    )


IBGE_production <- s3read_using(
    FUN = read.table,
    sep = ";",
    header = TRUE,
    as.is = TRUE,
    object = "brazil/soy/indicators/out/q4_2023/soy_production_IBGE_2003_2023_multilevel.csv",
    opts = c("check_region" = TRUE),
    bucket = "trase-storage"
) %>%
    as_tibble() %>%
    filter(level == "municipality") %>%
    select(TRASE_ID, YEAR, HA, TN) |>
    mutate(yield = TN / HA)


####################
# calculate production based on soy area (RS) and yield (IBGE)

quants_prod <- quants |>
    left_join(IBGE_production, join_by(trase_id == TRASE_ID, year == YEAR))
quants_prod <- quants_prod |>
    ungroup() |>
    group_by(trase_id) |>
    mutate(mean_yield_muni = mean(yield, na.rm = TRUE)) |>
    ungroup() |>
    group_by(ibge_state) |>
    mutate(mean_yield_state = mean(yield, na.rm = TRUE)) |>
    ungroup() |>
    replace_na(list(yield = 0)) |>
    mutate(
        yield_filled = case_when(
            (yield == 0 & mean_yield_muni != 0) ~ mean_yield_muni,
            (yield == 0 & mean_yield_muni == 0) ~ mean_yield_state,
            .default = yield
        )
    )

quants_prod <- quants_prod |> mutate(rs_tons = yield_filled * soy_back)
summary(quants_prod)

quants_prod_qa_by_state <- quants_prod |>
    group_by(year, ibge_state) |>
    summarise(
        IBGE_TN = sum(TN, na.rm = T),
        RS_TN = sum(rs_tons, na.rm = T),
        soy_ha = sum(soy_back, na.rm = T)
    )

#View(quants_prod_qa_by_state)

## per ton defo
quants_prod_per_ton <- quants_prod |>
    transmute(
        trase_id,
        year,
        rs_tons,
        across(
            soy_def_def5y:soy_def_harvest3y_5y_amort,
            ~ if_else(
                rs_tons == 0 | is.na(rs_tons) | is.na(.x), # The "Safety Net" conditions
                0, # Return 0 if any condition above is true
                .x / rs_tons # Otherwise, do the math
            ), # The math formula
            .names = "{.col}_per_ton" # Appends '_per_ton' to column names
        )
    )


# View(quants)
#-------- Step 2 -------#
#-- reading all flows --#
#-----------------------#
print(paste0("reading flows start: ", Sys.time()))
# read all v2.6.1 and v2.6.0
years261 <- 2021:2022
years260 <- 2013:2020
# v2.6.1
for (i in years261) {
    print(paste0("reading ", i, ": ", Sys.time()))
    tmp <- s3read_using(
        read_parquet,
        show_col_types = FALSE,
        as.is = TRUE,
        object = paste0(
            "brazil/soy/sei_pcs/v2.6.1/with_exporter/SEIPCS_BRAZIL_SOY_",
            i,
            "_WITH_EXPORTER.parquet"
        ),
        opts = c("check_region" = TRUE),
        bucket = "trase-storage"
    )
    assign(paste0("seipcs_", i), tmp)
}
# which(duplicated(seipcs_2021) == TRUE)
# v2.6.0
for (i in years260) {
    print(paste0("reading ", i, ": ", Sys.time()))
    tmp <- s3read_using(
        read_parquet,
        show_col_types = FALSE,
        as.is = TRUE,
        object = paste0(
            "brazil/soy/sei_pcs/v2.6.0/with_exporter/SEIPCS_BRAZIL_SOY_",
            i,
            "_WITH_EXPORTER.parquet"
        ),
        opts = c("check_region" = TRUE),
        bucket = "trase-storage"
    )
    assign(paste0("seipcs_", i), tmp)
}
# seipcs_2013 %>%
#     duplicated() %>%
#     which()
# seipcs_2014 %>%
#     duplicated() %>%
#     which()
# seipcs_2015 %>%
#     duplicated() %>%
#     which()
# seipcs_2016 %>%
#     duplicated() %>%
#     which()
# seipcs_2017 %>%
#     duplicated() %>%
#     which()
# seipcs_2018 %>%
#     duplicated() %>%
#     which()
# seipcs_2019 %>%
#     duplicated() %>%
#     which()
# seipcs_2020 %>%
#     duplicated() %>%
#     which()
# seipcs_2021 %>%
#     duplicated() %>%
#     which()
# seipcs_2022 %>%
#     duplicated() %>%
#     which()
##
#---- Step 3 ---#
#-- embedding known flows--#
#---------------#
print(paste0("embedding known flows", Sys.time()))
# seipcs
years <- c(years260, years261)
for (i in years) {
    print(paste0("embedding ", i, ": ", Sys.time()))
    # get data
    known_tmp <- get(paste0("seipcs_", i)) %>%
        filter(LVL6_TRASE_ID_PROD != "BR-XXXXXXX")
    # join quants
    known_tmp <- known_tmp %>%
        left_join(
            quants_prod_per_ton %>% filter(year == i),
            join_by(YEAR == year, LVL6_TRASE_ID_PROD == trase_id)
        )
    # embedd data
    known_tmp <- known_tmp %>%
        mutate(across(
            ends_with("_per_ton"),
            ~ .x * (VOLUME_RAW / 1000),
            .names = "{gsub('_per_ton', '_exp', .col)}"
        ))
    assign(paste0("known_", i), known_tmp)
}
#---- Step 3 ---#
#-- embedding unknown flows--#
#---------------#
# here we calculate the difference between embedded quants and original quants and assigne the left over quants to the unknown flows
for (i in years) {
    print(paste0("embedding unknonw", i, ": ", Sys.time()))
    known_tmp <- get(paste0("known_", i))
    unknown_tmp <- get(paste0("seipcs_", i)) %>%
        filter(LVL6_TRASE_ID_PROD == "BR-XXXXXXX")
    # aggregate all unknown
    unknown_tmp_agg <-
        unknown_tmp %>%
        group_by(YEAR) %>%
        summarise(VOLUME_RAW = sum(VOLUME_RAW, na.rm = TRUE))
    # aggregate all known
    known_flows <- known_tmp %>%
        ungroup() %>%
        group_by(YEAR) %>%
        summarize(
            # Dynamically target and sum all exposure columns
            across(ends_with("_exp"), ~ sum(.x, na.rm = TRUE))
        )

    # aggregate all original
    total_quants <- quants %>%
        filter(year == i) %>%
        ungroup() %>%
        group_by(year) %>%
        summarise(across(starts_with("soy"), ~ sum(.x, na.rm = TRUE)))

    # total_area <- IBGE_production %>%
    #     filter(YEAR == i) %>%
    #     ungroup() %>%
    #     group_by(YEAR) %>%
    #     summarise(SOY_AREA = sum(HA, na.rm = TRUE))

    # what is the difference between known flows and original quants
    # 1. Reshape totals to long format
    total_long <- total_quants %>%
        pivot_longer(-year, names_to = "metric", values_to = "total_val")

    # 2. Reshape exposure to long format and strip the "_exp" suffix
    known_long <- known_flows %>%
        rename(year = YEAR) %>%
        pivot_longer(-year, names_to = "metric", values_to = "exp_val") %>%
        mutate(metric = str_remove(metric, "_exp$"))

    # 3. Join them side-by-side and subtract
    quants_difference_long <- total_long %>%
        inner_join(known_long, by = c("year", "metric")) %>%
        mutate(difference = total_val - exp_val)

    quants_difference_wide <- quants_difference_long |>
        pivot_wider(
            id_cols = year,
            names_from = metric,
            values_from = difference
        ) |>
        rename(YEAR = year)

    # calulate quant per to for unknown flows
    diff_seipcs_quants <- quants_difference_wide %>% left_join(unknown_tmp_agg)
    diff_seipcs_quants <- diff_seipcs_quants %>%
        mutate(
            across(
                starts_with("soy"),
                ~ .x / (VOLUME_RAW / 1000),
                .names = "{.col}_per_ton"
            ),
            LVL6_TRASE_ID_PROD = "BR-XXXXXXX"
        ) %>%
        select(-VOLUME_RAW)

    # join quants
    unknown_tmp <- unknown_tmp %>%
        left_join(
            diff_seipcs_quants %>% filter(YEAR == i),
            join_by(YEAR, LVL6_TRASE_ID_PROD)
        )
    # embedd unknown
    unknown_tmp <- unknown_tmp %>%
        mutate(across(ends_with("per_ton"), ~ .x * (VOLUME_RAW / 1000)))

    assign(paste0("unknown_", i), unknown_tmp)
}


#------------ Step 4 -----------#
#--Combine known and unknown--#
#-------------------------------#

for (i in years) {
    known_flows <- get(paste0("known_", i))
    unknown_flows <- get(paste0("unknown_", i))
    full_tmp <- known_flows %>% bind_rows(unknown_flows)
    assign(paste0("full_", i), full_tmp)
}

#------------ Step 4 -----------#
#--Looking for NAs in the data--#
#-------------------------------#
print(paste0("check NAs start: ", Sys.time()))
for (i in years) {
    print(paste0("check NAs ", i, " all Good?: "))
    Na_new <- nrow(
        get(paste0("full_", i)) %>%
            filter(
                is.na(soy_back) |
                    is.na(soy_def_harvest5y_exp) |
                    is.na(soy_def_harvest3y_exp)
            )
    )
    Na_prod <- nrow(get(paste0("full_", i)) %>% filter(is.na(VOLUME_RAW)))
    print(Na_new == Na_prod)
}

#------------ Step 4 -----------#
#--Check exposure sums and number of rows--#
#-------------------------------#
for (i in years) {
    print(paste0("check nrow ", i, " all Good?: "))
    Nrow_original <- nrow(get(paste0("seipcs_", i)))
    Nrow_new <- nrow(get(paste0("full_", i)))
    print(Nrow_original == Nrow_new)
}
# for (i in years) {
#     print(paste0("check perc exposure ", i, " close to 100%?: "))
#     SUM_def_exp_original <- quants %>%
#         filter(YEAR == i) %>%
#         ungroup() %>%
#         summarize(
#             SOY_DEF_NORM = sum(SOY_DEF_NORM, na.rm = TRUE),
#             SOY_GrossDefGHG_NORM_tCO2eq = sum(
#                 SOY_GrossDefGHG_NORM_tCO2eq,
#                 na.rm = TRUE
#             ),
#             SOY_NetDefGHG_NORM_tCO2eq = sum(
#                 SOY_NetDefGHG_NORM_tCO2eq,
#                 na.rm = TRUE
#             )
#         )
#     SUM_IBGE_prod_original <- IBGE_production %>%
#         filter(YEAR == i) %>%
#         ungroup() %>%
#         summarize(IBGE_TN = sum(TN, na.rm = TRUE))
#     SUM_def_exp_embedded <- get(paste0("full_", i)) %>%
#         ungroup() %>%
#         summarize(
#             SOY_DEF_NORM = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE),
#             SOY_GROSS_GHG_EMISSIONS = sum(
#                 SOY_GROSS_GHG_EMISSIONS_5A1L_EXPOSURE,
#                 na.rm = TRUE
#             ),
#             SOY_NET_GHG_EMISSIONS = sum(
#                 SOY_NET_GHG_EMISSIONS_5A1L_EXPOSURE,
#                 na.rm = TRUE
#             ),
#             SEI_TN = sum(VOLUME_RAW) / 1000
#         )
#     perc_def <- (SUM_def_exp_embedded %>% pull(SOY_DEF_NORM)) /
#         (SUM_def_exp_original %>% pull(SOY_DEF_NORM)) *
#         100
#     perc_gemmiss <- (SUM_def_exp_embedded %>% pull(SOY_GROSS_GHG_EMISSIONS)) /
#         (SUM_def_exp_original %>% pull(SOY_GrossDefGHG_NORM_tCO2eq)) *
#         100
#     perc_nemmiss <- (SUM_def_exp_embedded %>% pull(SOY_NET_GHG_EMISSIONS)) /
#         (SUM_def_exp_original %>% pull(SOY_NetDefGHG_NORM_tCO2eq)) *
#         100
#     perc_pro <- (SUM_def_exp_embedded %>% pull(SEI_TN)) /
#         (SUM_IBGE_prod_original %>% pull(IBGE_TN)) *
#         100

#     print(paste0("Year - ", i, " Percent def exposure ", round(perc_def), "%"))
#     print(paste0(
#         "Year - ",
#         i,
#         " Percent gross emisions exposure ",
#         round(perc_gemmiss),
#         "%"
#     ))
#     print(paste0(
#         "Year - ",
#         i,
#         " Percent net emisions exposure ",
#         round(perc_nemmiss),
#         "%"
#     ))
#     print(paste0("Year - ", i, " Percent production ", round(perc_pro), "%"))
#     print("")
# }

#------------ Step 5 -----------#
#-- write data --#
#-------------------------------#
print(paste0("writing embedded flows start: ", Sys.time()))
for (i in years) {
    print(paste0("writing ", i))
    write_parquet(
        get(paste0("full_", i)),
        paste0(
            "~/documents/data/annual_metrics/soy_",
            i,
            "_post_embedding_quants.parquet"
        )
    )

    # s3write_using(
    #     get(paste0("full_", i)),
    #     object = paste0(
    #         "brazil/soy/sei_pcs/v2.6.1/post_embedding/SEIPCS_BRAZIL_SOY_",
    #         i,
    #         "_post_embedding_quants.parquet"
    #     ),
    #     bucket = "trase-storage",
    #     FUN = write_parquet,
    #     opts = c("check_region" = T, multipart = TRUE)
    # )
}

#------------ Step 6 -----------#
#-- ## test --#
#-------------------------------#
# print(paste0("testing embedded flows start: ", Sys.time()))

# full_2013 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol_kton = sum(VOLUME_PRODUCT, na.rm = TRUE) / 1000,
#         soy_def_exp_kha = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE) /
#             1000,
#         soy_gross_ghg_exp_kt = sum(
#             SOY_GROSS_GHG_EMISSIONS_5A1L_EXPOSURE,
#             na.rm = TRUE
#         ) /
#             1000,
#         soy_net_ghg_exp_kt = sum(
#             SOY_NET_GHG_EMISSIONS_5A1L_EXPOSURE,
#             na.rm = TRUE
#         ) /
#             1000
#     ) %>%
#     arrange(desc(soy_def_exp_kha)) %>%
#     print()

# full_2013 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol_kton = sum(VOLUME_PRODUCT, na.rm = TRUE) / 1000,
#         soy_def_exp_kha = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE) /
#             1000,
#         soy_gross_ghg_exp_kt = sum(
#             SOY_GROSS_GHG_EMISSIONS_5A1L_EXPOSURE,
#             na.rm = TRUE
#         ) /
#             1000,
#         soy_net_ghg_exp_kt = sum(
#             SOY_NET_GHG_EMISSIONS_5A1L_EXPOSURE,
#             na.rm = TRUE
#         ) /
#             1000
#     ) %>%
#     arrange(desc(product_vol_kton)) %>%
#     print()

# names(full_2018)
# names(full_2019)
# names(full_2020)
# full_2019 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE),
#         soy_gross_ghg_exp_kt = sum(
#             SOY_GROSS_GHG_EMISSIONS_5A1L_EXPOSURE,
#             na.rm = TRUE
#         ) /
#             1000,
#         soy_net_ghg_exp_kt = sum(
#             SOY_NET_GHG_EMISSIONS_5A1L_EXPOSURE,
#             na.rm = TRUE
#         ) /
#             1000
#     ) %>%
#     arrange(desc(product_vol)) %>%
#     print()
# full_2019 %>%
#     ungroup() %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     print()

# full_2020 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     arrange(desc(soy_def_exp)) %>%
#     print()
# full_2020 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     arrange(desc(product_vol)) %>%
#     print()
# full_2020 %>%
#     ungroup() %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     print()

# full_2021 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     arrange(desc(soy_def_exp)) %>%
#     print()
# full_2021 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     arrange(desc(product_vol)) %>%
#     print()
# full_2021 %>%
#     ungroup() %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     print()

# full_2022 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     arrange(desc(soy_def_exp)) %>%
#     print()
# full_2022 %>%
#     group_by(EXPORTER_GROUP) %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     arrange(desc(product_vol)) %>%
#     print()
# full_2022 %>%
#     ungroup() %>%
#     summarise(
#         product_vol = sum(VOLUME_RAW, na.rm = TRUE),
#         soy_def_exp = sum(SOY_DEFORESTATION_5A1L_EXPOSURE, na.rm = TRUE)
#     ) %>%
#     print()

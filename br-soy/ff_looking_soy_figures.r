library(tidyverse)
library(slider)
library(arrow)
library(geofacet)
library(scales)
library(sf)
library(zoo)


soy_br <- read_parquet("~/documents/data/annual_metrics/soy_annual_br.parquet")
soy_states <- read_parquet(
  "~/documents/data/annual_metrics/soy_annual_br_states.parquet"
)
soy_supply_chain_2022_v2 <- read_parquet(
  "~/documents/data/annual_metrics/soy_2022_post_embedding_quants_v2.parquet"
)


##########################################
# country level
##########################################

# How different is the
# - 5 year sum
# - 5 year annualzised
# - 5 year deduce spatial attribution to harvest
# - 3 year deduce spatial attribution to harvest
# - deudce spatial and statistical attribution to deforestation event

ggplot(
  soy_br |>
    filter(year >= 2010 & year <= 2024) |>
    filter(
      variable == "soy_def_5y_back" |
        variable == "soy_def_5y_annualized_back" |
        variable == "soy_def_harvest5y" |
        variable == "soy_def_harvest3y" #|
      #variable == "deduce_unarmotized" |
      #variable == "soy_def_def3y_gfc"
    ) |>
    mutate(
      variable = case_when(
        variable == "soy_def_5y_back" ~ "Trase-5y-sum",
        variable == "soy_def_5y_annualized_back" ~ "Trase-5y-annualized",
        variable == "soy_def_harvest5y" ~ "Soy-def-@harvest_5y",
        variable == "soy_def_harvest3y" ~ "Soy-def-@harvest_3y",
        #variable == "deduce_unarmotized" ~ "deduce-annual",
        #variable == "soy_def_def3y_gfc" ~ "deduce_spatial",
        .default = variable
      )
    ),
  aes(year, ha / 10000, color = fct_reorder(variable, ha, .desc = T))
) +
  geom_line(lwd = 1.5) +
  #geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    title = "Soy deforestation: comparing attribution methods (annual)",
    y = "in kha",
    x = "Year"
  ) +
  theme_minimal() +
  labs(color = NULL) +
  theme(legend.position = "bottom") #+
# guides(color = guide_legend(nrow = 2))

# 5 year sum double counts annual deforestation
# 5 year annualized undercounts annual deforestation because conversion to soy commonly does not happen in the first year after deforestation. dividing by 5 hence leads to undercounting
# 3 or 5 year attribution window, are quite similar, but expectedly 5 year is higher

# same using 5 year amotization

soy_br_5ymean <- soy_br |>
  filter(variable == "soy_def_harvest5y" | variable == "soy_def_harvest3y") |>
  ungroup() |>
  group_by(variable) |>
  arrange(year, .by_group = TRUE) |>
  mutate(amortized_5 = rollmean(ha, k = 5, align = "right", fill = NA))

soy_br_5ymean <- soy_br_5ymean |>
  transmute(
    year = year,
    variable = paste0(variable, "_5y_amort"),
    ha = amortized_5
  )
soy_br_amort <- soy_br |> bind_rows(soy_br_5ymean)


ggplot(
  soy_br_amort |>
    filter(year >= 2013 & year <= 2024) |>
    filter(
      variable == "soy_def_5y_back" |
        variable == "soy_def_5y_annualized_back" |
        variable == "soy_def_harvest5y_5y_amort" |
        variable == "soy_def_harvest3y_5y_amort" #|
      #variable == "deduce_unarmotized" |
      #variable == "soy_def_def3y_gfc"
    ) |>
    mutate(
      variable = case_when(
        variable == "soy_def_5y_back" ~ "Trase-5y-sum",
        variable == "soy_def_5y_annualized_back" ~ "Trase-5y-annualized",
        variable == "soy_def_harvest5y_5y_amort" ~ "Soy-def-@harvest_5y_amort",
        variable == "soy_def_harvest3y_5y_amort" ~ "Soy-def-@harvest_3y_amort",
        #variable == "deduce_unarmotized" ~ "deduce-annual",
        #variable == "soy_def_def3y_gfc" ~ "deduce_spatial",
        .default = variable
      )
    ),
  aes(year, ha / 10000, color = fct_reorder(variable, ha, .desc = T))
) +
  geom_line(lwd = 1.5) +
  #geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    title = "Soy deforestation: comparing attribution methods (5 y amortized)",
    y = "in kha",
    x = "Year"
  ) +
  theme_minimal() +
  labs(colour = NULL) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))

# how much percent difference between 5 year annualized and 3 and 5 year forward looking amortized
# how much percent difference between 3 year and 5 year forward looking

soy_br_amort_wide <- soy_br_amort |>
  pivot_wider(id_cols = year, names_from = variable, values_from = ha)

soy_br_amort_perc <- soy_br_amort_wide |>
  filter(year >= 2013) |>
  transmute(
    year = year,
    diff_perc_5y_Trase_ann_5y_ff_amort = ((soy_def_5y_annualized_back -
      soy_def_harvest5y_5y_amort) /
      soy_def_harvest5y_5y_amort) *
      100,
    diff_perc_5y_Trase_ann_3y_ff_amort = ((soy_def_5y_annualized_back -
      soy_def_harvest3y_5y_amort) /
      soy_def_harvest3y_5y_amort) *
      100,
    diff_perc_5y_ff_3y_ff_amort = ((soy_def_harvest5y_5y_amort -
      soy_def_harvest3y_5y_amort) /
      soy_def_harvest3y_5y_amort) *
      100
  )
soy_br_amort_perc_l <- soy_br_amort_perc |>
  pivot_longer(cols = -year, names_to = "variable", values_to = "percent")


ggplot(soy_br_amort_perc_l, aes(year, percent)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ fct_reorder(variable, percent)) +
  labs(title = "Difference between attribution methods in percent") +
  scale_x_continuous(breaks = breaks_pretty()) +
  theme_minimal()

# Annualization of Trase 5 year sum would miss between 20 and 80% of soy deforestation annually, depending on year and if a 3 or 5 year attribution window is assumed.
# A 5 year ff looking window would map around 22-30% more soy deforestation than a 3y ff looking window

##########################################
# What are the differences at state level? Are patterns the same?
##########################################

# same using 5 year amotization

soy_states_5ymean <- soy_states |>
  filter(variable == "soy_def_harvest5y" | variable == "soy_def_harvest3y") |>
  ungroup() |>
  group_by(variable, code, name, abbreviation) |>
  arrange(year, .by_group = TRUE) |>
  mutate(amortized_5 = rollmean(ha, k = 5, align = "right", fill = NA))

soy_states_5ymean <- soy_states_5ymean |>
  transmute(
    year = year,
    code = code,
    name = name,
    abbreviation = abbreviation,
    variable = paste0(variable, "_5y_amort"),
    ha = amortized_5
  )
soy_states_amort <- soy_states |> bind_rows(soy_states_5ymean)


ggplot(
  soy_states_amort |>
    filter(year >= 2013) |>
    filter(
      variable == "soy_def_5y_back" |
        variable == "soy_def_5y_annualized_back" |
        variable == "soy_def_harvest5y_5y_amort" |
        variable == "soy_def_harvest3y_5y_amort"
    ) |>
    mutate(
      variable = case_when(
        variable == "soy_def_5y_back" ~ "Trase-5y-sum",
        variable == "soy_def_5y_annualized_back" ~ "Trase-5y-annualized",
        variable == "soy_def_harvest5y_5y_amort" ~ "Soy-def-@harvest_5y_amort",
        variable == "soy_def_harvest5y_5y_amort" ~ "Soy-def-@harvest_5y_amort",
        .default = variable
      )
    ),
  aes(year - 2000, ha / 10000, color = fct_reorder(variable, ha, .desc = T))
) +
  geom_line() +
  scale_x_continuous(breaks = breaks_pretty()) +
  labs(colour = NULL) +
  facet_geo(~abbreviation, grid = "br_states_grid1") +
  #facet_wrap(~abbreviation)+
  theme_minimal() +
  theme(legend.position = "bottom") #+
#guides(color = guide_legend(nrow = 2))

#We see that RS has very high 5 year sum deforestation. This is not as accentuated in annual metric
ggplot(
  soy_states_amort |>
    filter(year >= 2013) |>
    filter(
      #variable == "soy_def_5y_back" |
      variable == "soy_def_5y_annualized_back" |
        variable == "soy_def_harvest5y_5y_amort" |
        variable == "soy_def_harvest3y_5y_amort"
    ) |>
    mutate(
      variable = case_when(
        #variable == "soy_def_5y_back" ~ "Trase-5y-sum",
        variable == "soy_def_5y_annualized_back" ~ "Trase-5y-annualized",
        variable == "soy_def_harvest5y_5y_amort" ~ "Soy-def-@harvest_5y_amort",
        variable == "soy_def_harvest5y_5y_amort" ~ "Soy-def-@harvest_5y_amort",
        .default = variable
      )
    ),
  aes(year - 2000, ha / 10000, color = fct_reorder(variable, ha, .desc = T))
) +
  geom_line() +
  scale_x_continuous(breaks = breaks_pretty()) +
  labs(colour = NULL) +
  #facet_geo(~abbreviation, grid = "br_states_grid1") +
  facet_wrap(~abbreviation) +
  theme_minimal() +
  theme(legend.position = "bottom") #+
#guides(color = guide_legend(nrow = 2))

#non-amortized
ggplot(
  soy_states_amort |>
    filter(year >= 2013) |>
    filter(
      variable == "soy_def_5y_back" |
        variable == "soy_def_5y_annualized_back" |
        variable == "soy_def_harvest5y" |
        variable == "soy_def_harvest3y"
    ) |>
    mutate(
      variable = case_when(
        variable == "soy_def_5y_back" ~ "Trase-5y-sum",
        variable == "soy_def_5y_annualized_back" ~ "Trase-5y-annualized",
        variable == "soy_def_harvest5y" ~ "Soy-def-@harvest_5y",
        variable == "soy_def_harvest3y" ~ "Soy-def-@harvest_3y",
        .default = variable
      )
    ),
  aes(year - 2000, ha / 10000, color = fct_reorder(variable, ha, .desc = T))
) +
  geom_line() +
  scale_x_continuous(breaks = breaks_pretty()) +
  labs(colour = NULL) +
  #facet_geo(~abbreviation, grid = "br_states_grid1") +
  facet_wrap(~abbreviation) +
  theme_minimal() +
  theme(legend.position = "bottom") #+
#guides(color = guide_legend(nrow = 2))

ggplot(
  soy_states_amort |>
    filter(year >= 2013) |>
    filter(
      #variable == "soy_def_5y_back" |
      variable == "soy_def_5y_annualized_back" |
        variable == "soy_def_harvest5y" |
        variable == "soy_def_harvest3y"
    ) |>
    mutate(
      variable = case_when(
        #variable == "soy_def_5y_back" ~ "Trase-5y-sum",
        variable == "soy_def_5y_annualized_back" ~ "Trase-5y-annualized",
        variable == "soy_def_harvest5y" ~ "Soy-def-@harvest_5y",
        variable == "soy_def_harvest3y" ~ "Soy-def-@harvest_3y",
        .default = variable
      )
    ),
  aes(year - 2000, ha / 10000, color = fct_reorder(variable, ha, .desc = T))
) +
  geom_line() +
  scale_x_continuous(breaks = breaks_pretty()) +
  labs(colour = NULL) +
  #facet_geo(~abbreviation, grid = "br_states_grid1") +
  facet_wrap(~abbreviation) +
  theme_minimal() +
  theme(legend.position = "bottom") #+
#guides(color = guide_legend(nrow = 2))

# main question:
# do we want to show amortized, non-amortized values?

# 10 traders with highest exposure in 2022
soy_supply_chain_2022_v2_exp <- soy_supply_chain_2022 |>
  group_by(EXPORTER_GROUP) |>
  summarize(across(ends_with("_exp"), ~ sum(.x, na.rm = T)))

soy_supply_chain_2022_v2_reg <- soy_supply_chain_2022_v2 |>
  group_by(LVL6_NAME_PROD) |>
  summarize(across(ends_with("_exp"), ~ sum(.x, na.rm = T)))
print("Trase 5 year sum deforestation")
soy_supply_chain_2022_v2_exp |>
  arrange(desc(soy_def_5y_back_exp)) |>
  select(EXPORTER_GROUP, soy_def_5y_back_exp) |>
  filter(
    EXPORTER_GROUP != "PROCESSED DOMESTICALLY",
    EXPORTER_GROUP != "UNKNOWN"
  ) |>
  top_n(10) |>
  print()

print("Trase ff 5 year amorized")
soy_supply_chain_2022_v2_exp |>
  arrange(desc(soy_def_harvest5y_5y_amort_exp)) |>
  select(EXPORTER_GROUP, soy_def_harvest5y_5y_amort_exp) |>
  filter(
    EXPORTER_GROUP != "PROCESSED DOMESTICALLY",
    EXPORTER_GROUP != "UNKNOWN"
  ) |>
  top_n(10) |>
  print()

print("Trase ff 3 year amorized")
soy_supply_chain_2022_v2_exp |>
  arrange(desc(soy_def_harvest3y_5y_amort_exp)) |>
  select(EXPORTER_GROUP, soy_def_harvest3y_5y_amort_exp) |>
  filter(
    EXPORTER_GROUP != "PROCESSED DOMESTICALLY",
    EXPORTER_GROUP != "UNKNOWN"
  ) |>
  top_n(10) |>
  print()

print("Trase ff 3 year ")
soy_supply_chain_2022_v2_exp |>
  arrange(desc(soy_def_harvest3y_exp)) |>
  select(EXPORTER_GROUP, soy_def_harvest3y_exp) |>
  filter(
    EXPORTER_GROUP != "PROCESSED DOMESTICALLY",
    EXPORTER_GROUP != "UNKNOWN"
  ) |>
  top_n(10) |>
  print()


soy_supply_chain_2022_reg |>
  arrange(desc(soy_def_5y_back_exp)) |>
  select(LVL6_NAME_PROD, soy_def_5y_back_exp) |>
  filter(LVL6_NAME_PROD != "UNKNOWN") |>
  top_n(10) |>
  print()

soy_supply_chain_2022_imp |>
  arrange(desc(soy_def_5y_back_exp)) |>
  select(COUNTRY_GROUP, soy_def_5y_back_exp) |>
  filter(
    COUNTRY_GROUP != "BRAZIL",
    COUNTRY_GROUP != "UNKNOWN COUNTRY EUROPEAN UNION"
  ) |>
  top_n(10) |>
  print()


soy_supply_chain_2022_exp |>
  arrange(desc(soy_def_5y_annualized_back_exp)) |>
  select(EXPORTER_GROUP, soy_def_5y_annualized_back_exp)
soy_supply_chain_2022_exp |>
  arrange(desc(soy_def_harvest5y_5y_amort_exp)) |>
  select(EXPORTER_GROUP, soy_def_harvest5y_5y_amort_exp)
soy_supply_chain_2022_exp |>
  arrange(desc(soy_def_harvest3y_5y_amort_exp)) |>
  select(EXPORTER_GROUP, soy_def_harvest3y_5y_amort_exp)
soy_supply_chain_2022_exp |>
  arrange(desc(soy_def_harvest5y_exp)) |>
  select(EXPORTER_GROUP, soy_def_harvest5y_exp)
soy_supply_chain_2022_exp |>
  arrange(desc(soy_def_harvest3y_exp)) |>
  select(EXPORTER_GROUP, soy_def_harvest3y_exp)

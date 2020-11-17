## @knitr acs5_v18_clean

library(magrittr)

v18 <- tidycensus::load_variables(2018, "acs5", cache = TRUE)
# View(v18)

v18_clean <- v18 %>%
  tidyr::separate(
    col = name,
    into = "table",
    sep = "_",
    remove = FALSE
  ) %>%
  tidyr::separate(
    col = label,
    into = c("label_base", "label_unit", "label_primary", "label_secondary", "label_tertiary"),
    sep = "!!",
    remove = FALSE
  ) %>% 
  dplyr::mutate(
    concept = stringr::str_to_title(concept),
    concept = stringr::str_replace_all(concept, "[:space:](Or|The|Of|By|And|For|In|With)[:space:]", tolower),
    concept = stringr::str_replace(concept, "United[:space:]States", "U.S."),
    level = dplyr::case_when(
      !is.na(label_tertiary) ~ "tertiary",
      !is.na(label_secondary) ~ "secondary",
      !is.na(label_primary) ~ "primary",
      is.na(label_primary) ~ "base"
    ),
    sex = dplyr::if_else(stringr::str_detect(concept, "Sex"), TRUE, FALSE),
    age = dplyr::if_else(stringr::str_detect(concept, "Age"), TRUE, FALSE),
    race = dplyr::case_when(
      stringr::str_detect(concept, "White[:space:]Alone") ~ "White Alone",
      stringr::str_detect(concept, "African[:space:]American[:space:]Alone") ~ "Black Or African American Alone",
      stringr::str_detect(concept, "Native[:space:]Alone") ~ "American Indian And Alaska Native Alone",
      stringr::str_detect(concept, "Asian[:space:]Alone") ~ "Asian Alone",
      stringr::str_detect(concept, "Pacific[:space:]Islander[:space:]Alone") ~ "Native Hawaiian And Other Pacific Islander Alone",
      stringr::str_detect(concept, "Other[:space:]Race[:space:]Alone") ~ "Some Other Race Alone",
      stringr::str_detect(concept, "Two[:space:]Or[:space:]More") ~ "Two Or More Races"
    ),
    hispanic = dplyr::case_when(
      stringr::str_detect(concept, "Not[:space:]Hispanic") | stringr::str_detect(label, "Not[:space:]Hispanic") ~ "Not Hispanic or Latino",
      (stringr::str_detect(concept, "Hispanic")) && !stringr::str_detect(label, "Not[:space:]Hispanic") ~ "Hispanic or Latino"
    ),
    label_snake = snakecase::to_any_case(
        label,
        numerals = "asis"),
    label_snake = stringr::str_replace(label_snake, "year", "yr"),
    label_snake = stringr::str_replace(label_snake, "estimate_", ""),
    label_snake = dplyr::case_when(
      hispanic == "Hispanic or Latino" ~ stringr::str_c(label_snake, "_hispanic"),
      hispanic == "Not Hispanic or Latino" ~ stringr::str_c(label_snake, "_not_hispanic"),
      TRUE ~ label_snake
    ),
    label_snake = dplyr::case_when(
      race == "White Alone" ~ stringr::str_c(label_snake, "_white"),
      race == "Black Or African American Alone" ~ stringr::str_c(label_snake, "_black"),
      race == "American Indian And Alaska Native Alone" ~ stringr::str_c(label_snake, "_native"),
      race == "Asian Alone" ~ stringr::str_c(label_snake, "_asian"),
      race == "Native Hawaiian And Other Pacific Islander Alone" ~ stringr::str_c(label_snake, "_pacific_islander"),
      race == "Some Other Race Alone" ~ stringr::str_c(label_snake, "_other_race"),
      race == "Two Or More Races" ~ stringr::str_c(label_snake, "_two_or_more_races"),
      TRUE ~ label_snake
    ),
    label_snake = forcats::fct_inorder(label_snake)
  )

## @knitr acs5_overview_tables

overview_tables <- c("B02001",
                     "B15003",
                     "B03002",
                     "B03003",
                     "B05001",
                     "B25002",
                     "B25003",
                     "B25017",
                     "B25024",
                     "B25034",
                     "B25075",
                     "B25094",
                     "B19001",
                     "B23025",
                     "B23007")

overview_acs <- purrr::map_dfr(
  overview_tables,
  ~ tidycensus::get_acs(
    geography = "tract",
    table = .x,
    cache_table = TRUE,
    county = "Baltimore city",
    state = "MD",
    year = 2018,
    survey = "acs5"
  )
)

overview_acs <- overview_acs %>%
  janitor::clean_names("snake") %>% 
  #dplyr::select(-c(table:label_snake))
  dplyr::left_join(v18_clean, by = c("variable" = "name"))

saveRDS(overview_acs, file = "data/overview_acs.rda")

overview_acs_county <- purrr::map_dfr(
  overview_tables,
  ~ tidycensus::get_acs(
    geography = "county",
    table = .x,
    cache_table = TRUE,
    county = "Baltimore city",
    state = "MD",
    year = 2018,
    survey = "acs5"
  )
)

overview_acs_county <- overview_acs_county %>%
  janitor::clean_names("snake") %>% 
  #dplyr::select(-c(table:label_snake))
  dplyr::left_join(v18_clean, by = c("variable" = "name"))

saveRDS(overview_acs_county, file = "data/overview_acs_county.rda")

## @knitr housing_cost_acs_tables

housing_cost_tables <- c("B25063",
                         "B25074",
                         "B25087",
                         "B25106")


selected_acs <- purrr::map_dfr(
  housing_cost_tables,
  ~ tidycensus::get_acs(
    geography = "tract",
    table = .x,
    county = "Baltimore city",
    state = "MD",
    year = 2018,
    survey = "acs5"
  )
)

selected_acs_stat <- selected_acs %>%
  tidyr::pivot_wider(id_cols = GEOID, names_from = variable, values_from = estimate) %>%
  dplyr::transmute(
    geoid = GEOID,
    num_households_less_20k = B25074_002 + B25074_011,
    num_households_not_cost_burdened_less_20k = B25074_003 + B25074_004 + B25074_005 + B25074_012 + B25074_013 + B25074_014,
    num_households_cost_burdened_less_20k = B25074_006 + B25074_007 + B25074_008 + B25074_009 + B25074_015 + B25074_016 + B25074_017 + B25074_018,
    num_households_severely_cost_burdened_less_20k = B25074_009 + B25074_018,
    num_households_burden_not_computed_less_20k = B25074_010 + B25074_019,
    num_households_20k_to_34k = B25074_020,
    num_households_not_cost_burdened_20k_to_34k = B25074_021 + B25074_022 + B25074_023,
    num_households_cost_burdened_20k_to_34k = B25074_024 + B25074_025 + B25074_026 + B25074_027,
    num_households_severely_cost_burdened_20k_to_34k = B25074_027,
    num_households_burden_not_computed_20k_to_34k = B25074_028,
    num_households_35k_to_49k = B25074_029,
    num_households_not_cost_burdened_35k_to_49k = B25074_030 + B25074_031 + B25074_032,
    num_households_cost_burdened_35k_to_49k = B25074_033 + B25074_034 + B25074_035 + B25074_036,
    num_households_severely_cost_burdened_35k_to_49k = B25074_036,
    num_households_burden_not_computed_35k_to_49k = B25074_037,
    num_households_50k_to_74k = B25074_038,
    num_households_not_cost_burdened_50k_to_74k = B25074_039 + B25074_040 + B25074_041,
    num_households_cost_burdened_50k_to_74k = B25074_042 + B25074_043 + B25074_044 + B25074_045,
    num_households_severely_cost_burdened_50k_to_74k = B25074_045,
    num_households_burden_not_computed_50k_to_74k = B25074_046,
    num_households_75k_more = B25074_047 + B25074_056,
    num_households_not_cost_burdened_75k_more = B25074_048 + B25074_049 + B25074_050 + B25074_057 + B25074_058 + B25074_059,
    num_households_cost_burdened_75k_more = B25074_051 + B25074_052 + B25074_053 + B25074_054 + B25074_060 + B25074_061 + B25074_062 + B25074_063,
    num_households_severely_cost_burdened_75k_more = B25074_054 + B25074_063,
    num_households_burden_not_computed_75k_more = B25074_055 + B25074_064,
    num_owner_households = B25106_002,
    num_owner_households_less_20k = B25106_003,
    num_owner_households_not_cost_burdened_less_20k = B25106_004 + B25106_005,
    num_owner_households_cost_burdened_less_20k = B25106_006,
    num_owner_households_20k_to_34k = B25106_007,
    num_owner_households_not_cost_burdened_20k_to_34k = B25106_008 + B25106_009,
    num_owner_households_cost_burdened_20k_to_34k = B25106_010,
    num_owner_households_35k_to_49k = B25106_011,
    num_owner_households_not_cost_burdened_35k_to_49k = B25106_012 + B25106_013,
    num_owner_households_cost_burdened_35k_to_49k = B25106_014,
    num_owner_households_50k_to_74k = B25106_015,
    num_owner_households_not_cost_burdened_50k_to_74k = B25106_016 + B25106_017,
    num_owner_households_cost_burdened_50k_to_74k = B25106_018,
    num_owner_households_75k_more = B25106_019,
    num_owner_households_not_cost_burdened_75k_more = B25106_020 + B25106_021,
    num_owner_households_cost_burdened_75k_more = B25106_022,
    num_owner_households_zero_negative_income = B25106_023,
    num_renter_households = B25106_024,
    num_renter_households_less_20k = B25106_025,
    num_renter_households_not_cost_burdened_less_20k = B25106_026 + B25106_027,
    num_renter_households_cost_burdened_less_20k = B25106_028,
    num_renter_households_20k_to_34k = B25106_029,
    num_renter_households_not_cost_burdened_20k_to_34k = B25106_030 + B25106_031,
    num_renter_households_cost_burdened_20k_to_34k = B25106_032,
    num_renter_households_35k_to_49k = B25106_033,
    num_renter_households_not_cost_burdened_35k_to_49k = B25106_034 + B25106_035,
    num_renter_households_cost_burdened_35k_to_49k = B25106_036,
    num_renter_households_50k_to_74k = B25106_037,
    num_renter_households_not_cost_burdened_50k_to_74k = B25106_038 + B25106_039,
    num_renter_households_cost_burdened_50k_to_74k = B25106_040,
    num_renter_households_75k_more = B25106_041,
    num_renter_households_not_cost_burdened_75k_more = B25106_042 + B25106_043,
    num_renter_households_cost_burdened_75k_more = B25106_044,
    num_renter_households_zero_negative_income = B25106_045,
    num_renter_households_no_cash_rent = B25106_046,
    perc_renter_households_cost_burdened_less_20k = num_renter_households_cost_burdened_less_20k / num_renter_households_less_20k,
    perc_renter_households_cost_burdened_20k_to_34k = num_renter_households_cost_burdened_20k_to_34k / num_renter_households_20k_to_34k,
    perc_renter_households_cost_burdened_35k_to_49k = num_renter_households_cost_burdened_35k_to_49k / num_renter_households_35k_to_49k,
    perc_renter_households_cost_burdened_50k_to_74k = num_renter_households_cost_burdened_50k_to_74k / num_renter_households_50k_to_74k,
    perc_renter_households_cost_burdened_75k_more = num_renter_households_cost_burdened_75k_more / num_renter_households_75k_more,
    perc_owner_households_cost_burdened_less_20k = num_owner_households_cost_burdened_less_20k / num_owner_households_less_20k,
    perc_owner_households_cost_burdened_20k_to_34k = num_owner_households_cost_burdened_20k_to_34k / num_owner_households_20k_to_34k,
    perc_owner_households_cost_burdened_35k_to_49k = num_owner_households_cost_burdened_35k_to_49k / num_owner_households_35k_to_49k,
    perc_owner_households_cost_burdened_50k_to_74k = num_owner_households_cost_burdened_50k_to_74k / num_owner_households_50k_to_74k,
    perc_owner_households_cost_burdened_75k_more = num_owner_households_cost_burdened_75k_more / num_owner_households_75k_more,
    perc_households_cost_burdened_less_20k = num_households_cost_burdened_less_20k / (num_households_not_cost_burdened_less_20k + num_households_cost_burdened_less_20k),
    perc_households_cost_burdened_20k_to_34k = num_households_cost_burdened_20k_to_34k / (num_households_not_cost_burdened_20k_to_34k + num_households_cost_burdened_20k_to_34k),
    perc_households_cost_burdened_35k_to_49k = num_households_cost_burdened_35k_to_49k / (num_households_not_cost_burdened_35k_to_49k + num_households_cost_burdened_35k_to_49k),
    perc_households_cost_burdened_50k_to_74k = num_households_cost_burdened_50k_to_74k / (num_households_not_cost_burdened_50k_to_74k + num_households_cost_burdened_50k_to_74k),
    perc_households_cost_burdened_75k_more = num_households_cost_burdened_75k_more / (num_households_not_cost_burdened_75k_more + num_households_cost_burdened_75k_more),
    perc_households_severely_cost_burdened_less_20k = num_households_severely_cost_burdened_less_20k / (num_households_not_cost_burdened_less_20k + num_households_cost_burdened_less_20k),
    perc_households_severely_cost_burdened_20k_to_34k = num_households_severely_cost_burdened_20k_to_34k / (num_households_not_cost_burdened_20k_to_34k + num_households_cost_burdened_20k_to_34k),
    perc_households_severely_cost_burdened_35k_to_49k = num_households_severely_cost_burdened_35k_to_49k / (num_households_not_cost_burdened_35k_to_49k + num_households_cost_burdened_35k_to_49k),
    perc_households_severely_cost_burdened_50k_to_74k = num_households_severely_cost_burdened_50k_to_74k / (num_households_not_cost_burdened_50k_to_74k + num_households_cost_burdened_50k_to_74k),
    perc_households_severely_cost_burdened_75k_more = num_households_severely_cost_burdened_75k_more / (num_households_not_cost_burdened_75k_more + num_households_cost_burdened_75k_more),
    num_rental_units_cash_rent = B25063_002,
    num_rental_units_monthly_cost_less_200 = B25063_003 + B25063_004 + B25063_005,
    num_rental_units_monthly_cost_200_to_399 = B25063_006 + B25063_007 + B25063_008 + B25063_009,
    num_rental_units_monthly_cost_400_to_599 = B25063_010 + B25063_011 + B25063_012 + B25063_013,
    num_rental_units_monthly_cost_600_to_799 = B25063_014 + B25063_015 + B25063_016 + B25063_017,
    num_rental_units_monthly_cost_800_to_999 = B25063_018 + B25063_019,
    num_rental_units_monthly_cost_1000_to_1499 = B25063_020 + B25063_021,
    num_rental_units_monthly_cost_1500_more = B25063_022 + B25063_023 + B25063_024 + B25063_025 + B25063_026,
    num_rental_units_no_cash_rent = B25063_027,
    num_owner_mortgage_units = B25087_002,
    num_owner_mortgage_units_monthly_cost_less_200 = B25087_003,
    num_owner_mortgage_units_monthly_cost_200_to_399 = B25087_004 + B25087_005,
    num_owner_mortgage_units_monthly_cost_400_to_599 = B25087_006 + B25087_007,
    num_owner_mortgage_units_monthly_cost_600_to_799 = B25087_008 + B25087_009,
    num_owner_mortgage_units_monthly_cost_800_to_999 = B25087_010 + B25087_011,
    num_owner_mortgage_units_monthly_cost_1000_to_1499 = B25087_012 + B25087_013,
    num_owner_mortgage_units_monthly_cost_1500_more = B25087_014 + B25087_015 + B25087_016 + B25087_017 + B25087_018 + B25087_019,
    num_owner_no_mortgage_units = B25087_020,
    num_owner_no_mortgage_units_monthly_cost_less_200 = B25087_021 + B25087_022 + B25087_023,
    num_owner_no_mortgage_units_monthly_cost_200_to_399 = B25087_024 + B25087_025 + B25087_026 + B25087_027,
    num_owner_no_mortgage_units_monthly_cost_400_to_599 = B25087_028 + B25087_029,
    num_owner_no_mortgage_units_monthly_cost_600_to_799 = B25087_030 + B25087_031,
    num_owner_no_mortgage_units_monthly_cost_800_to_999 = B25087_032 + B25087_033,
    num_owner_no_mortgage_units_monthly_cost_1000_to_1499 = B25087_034 + B25087_035 + B25087_036 + B25087_037 + B25087_038,
    num_owner_no_mortgage_units_monthly_cost_1500_more = B25087_039
  ) %>% 
  dplyr::mutate(
    dplyr::across(
      tidyr::starts_with("perc"),
      ~ as.numeric(.x * 100)
      )
    )



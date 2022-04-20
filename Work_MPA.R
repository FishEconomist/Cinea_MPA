#---- projet MPA Europe 2022-2023 ----
# 
# Loading geographical info + filtering and simplifying
#
# Coder : Sébastien Metz
# Coding January 2022 - March 2022
#

#---- Loading usefull functions and lilbraries ----
#
#

remove(list = ls(all.names = TRUE))

# Specific housekeeping functions
source("R/Housekeeping.R")

detachAllPackages()

package.list <-
  c(
    "tidyverse",
    "readr",
    "readxl",
    "scales",
    "lubridate",
    "sf"
  )

attachPackages(package.list)
source("R/Format.R")

#---- Load data ----
load("Input/Global_Natura2000.Rdata")
load("Input/Global_CDDA.Rdata")

CountryCodes <- 
  funCharAsFactor(
    read_excel(
      "Input/CountryCodes.xlsx",
      col_names = TRUE,
      sheet = "CountryCodes") %>%
      filter(EU_work == 1)
  ) %>%
  mutate(CountryFile = str_replace(Country, " ", "_"))

#---- Prepare data ----

Global_Natura2000_marine_init <- Global_Natura2000_marine
Global_CDDA_marine_init <- Global_CDDA_marine
Global_CDDA_marine_test_init <- Global_CDDA_marine_test

CountryCodesBase <- 
  CountryCodes %>% 
  select(Lot, CountryFile, Order) %>%
  arrange(Order) %>%
  select(-Order)

varAreaGroup <- 
  c("0-1 km2",
    "1-3 km2",
    "3-5 km2",
    "5-10 km2",
    "over 10 km2")

Global_Natura2000_marine <- 
  dplyr::left_join(
    Global_Natura2000_marine_init,
    CountryCodes,
    by = c("COUNTRY_CODE" = "CountryCode")) %>%
  mutate(AreaGroup = case_when(
    AREAHA <= 100 ~ "0-1 km2",
    AREAHA <= 300 ~ "1-3 km2",
    AREAHA <= 500 ~ "3-5 km2",
    AREAHA <= 1000 ~ "5-10 km2",
    AREAHA > 1000 ~ "over 10 km2",
    TRUE ~ "no size reported"
  ))

Global_CDDA_marine <- 
  dplyr::left_join(
    Global_CDDA_marine_init,
    CountryCodes,
    by = c("cddaCountryCode" = "CountryCode")) %>%
  mutate(AreaGroup = case_when(
    siteArea <= 100 ~ "0-1 km2",
    siteArea <= 300 ~ "1-3 km2",
    siteArea <= 500 ~ "3-5 km2",
    siteArea <= 1000 ~ "5-10 km2",
    siteArea > 1000 ~ "over 10 km2",
    TRUE ~ "no size reported"
  ))

Global_CDDA_marine_test <- 
  dplyr::left_join(
    Global_CDDA_marine_test_init,
    CountryCodes,
    by = c("cddaCountryCode" = "CountryCode")) %>%
  mutate(AreaGroup = case_when(
    siteArea <= 100 ~ "0-1 km2",
    siteArea <= 300 ~ "1-3 km2",
    siteArea <= 500 ~ "3-5 km2",
    siteArea <= 1000 ~ "5-10 km2",
    siteArea > 1000 ~ "over 10 km2",
    TRUE ~ "no size reported"
  ))

#---- MPA Size range ----

Global_Natura2000_marine %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(N = n()) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "N") %>%
  arrange(Order) %>%
  select(-Order) %>%
  print()


CDDA_SizeRange <- Global_CDDA_marine %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(N = n()) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "N") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()


#---- MPA Totals ----

Top_stat_Natura2000 <- 
  Global_Natura2000_marine %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_N2000 = n(),
            TotArea_N2000 = round(sum(AREAHA, na.rm = TRUE)/100),
            MeanArea_N2000 = round(mean(AREAHA, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA <- 
  Global_CDDA_marine %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_CDDA = n(),
            TotArea_CDDA = round(sum(siteArea, na.rm = TRUE)/100),
            MeanArea_CDDA = round(mean(siteArea, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA_test <- 
  Global_CDDA_marine_test %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_CDDA_r = n(),
            TotArea_CDDA_r = round(sum(siteArea, na.rm = TRUE)/100),
            MeanArea_CDDA_r = round(mean(siteArea, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat <- 
  dplyr::left_join(
    dplyr::left_join(
      CountryCodesBase,
      Top_stat_Natura2000,
      by = c("Lot", "CountryFile")
    ),
    Top_stat_CDDA,
    by = c("Lot", "CountryFile")
  ) %>%
  print()

Top_stat_CDDA_Comp <- 
  dplyr::left_join(
    dplyr::left_join(
      CountryCodesBase,
      Top_stat_CDDA,
      by = c("Lot", "CountryFile")
    ),
    Top_stat_CDDA_test,
    by = c("Lot", "CountryFile")
  ) %>%
  print()

#---- MPA Totals for over X km2----

#### CHANGE X HERE - X in hectares
X <- 500
####
Top_stat_Natura2000_X <- 
  Global_Natura2000_marine %>%
  filter(AREAHA >= X) %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_N2000 = n(),
            TotArea_N2000 = round(sum(AREAHA, na.rm = TRUE)/100),
            MeanArea_N2000 = round(mean(AREAHA, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA_X <- 
  Global_CDDA_marine %>%
  filter(siteArea >= X) %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_CDDA = n(),
            TotArea_CDDA = round(sum(siteArea, na.rm = TRUE)/100),
            MeanArea_CDDA = round(mean(siteArea, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA_test_X <- 
  Global_CDDA_marine_test %>%
  filter(siteArea >= X) %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_CDDA_r = n(),
            TotArea_CDDA_r = round(sum(siteArea, na.rm = TRUE)/100),
            MeanArea_CDDA_r = round(mean(siteArea, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_Natura2000_Comp_X <- 
  dplyr::left_join(
    dplyr::left_join(
      CountryCodesBase,
      Top_stat_Natura2000,
      by = c("Lot", "CountryFile")
    ),
    Top_stat_Natura2000_X,
    by = c("Lot", "CountryFile"),
    suffix = c("", ".R")
  ) %>%
  print()

Top_stat_CDDA_Comp_X <- 
  dplyr::left_join(
    dplyr::left_join(
      CountryCodesBase,
      Top_stat_CDDA,
      by = c("Lot", "CountryFile")
    ),
    Top_stat_CDDA_X,
    by = c("Lot", "CountryFile"),
    suffix = c("", ".R")
  ) %>%
  print()


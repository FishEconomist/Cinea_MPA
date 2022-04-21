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
    "sf",
    "units"
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

EU_land <- 
  st_read(
    dsn = "Input/EEA_Coastline_Polygon_Shape/EEA_Coastline_20170228.shp") %>% 
  mutate(Id = 1)

#---- Prepare data ----

Global_Natura2000_marine_init <- 
  Global_Natura2000_marine
Global_CDDA_marine_init <- 
  Global_CDDA_marine %>%
  filter(cddaRegionCode %notin% c("TF"))
Global_CDDA_marine_test_init <- 
  Global_CDDA_marine_test %>%
  filter(cddaRegionCode %notin% c("TF"))


Global_Natura2000_marine_sf$GISarea <-
  st_area(Global_Natura2000_marine_sf) / 1e4

Global_CDDA_marine_sf$GISarea <-
  st_area(Global_CDDA_marine_sf) / 1e4

test_Natura2000_sf <- 
  st_intersection(Global_Natura2000_marine_sf,
                  EU_land)

test_Natura2000_sf$GISarea_land <- 
  st_area(test_Natura2000_sf) / 1e4

test_CDDA_sf <- 
  st_intersection(Global_CDDA_marine_sf,
                  EU_land)

test_CDDA_sf$GISarea_land <- 
  st_area(test_CDDA_sf) / 1e4

landArea_Natura2000 <- 
  test_Natura2000_sf %>%
  st_drop_geometry() %>%
  group_by(SITECODE) %>%
  summarise(GISarea_land = sum(GISarea_land, na.rm = TRUE))

landArea_CDDA <- 
  test_CDDA_sf %>%
  st_drop_geometry() %>%
  group_by(cddaId) %>%
  summarise(GISarea_land = sum(GISarea_land, na.rm = TRUE))

CountryCodesBase <- 
  CountryCodes %>% 
  select(Lot, CountryFile, Order) %>%
  arrange(Order) %>%
  select(-Order)

varAreaGroup <- 
  c("0-1 km2",
    "1-2 km2",
    "2-3 km2",
    "1-3 km2",
    "3-4 km2",
    "4-5 km2",
    "3-5 km2",
    "5-10 km2",
    "over 10 km2")

Global_marine_Natura2000 <- 
  dplyr::left_join(
    dplyr::left_join(
      dplyr::left_join(
        Global_Natura2000_marine_init,
        CountryCodes,
        by = c("COUNTRY_CODE" = "CountryCode")),
      Global_Natura2000_marine_sf %>%
        st_drop_geometry() %>%
        select(SITECODE, GISarea),
      by = "SITECODE"),
    landArea_Natura2000,
    by = "SITECODE") %>%
  drop_units() %>%
  mutate(
    GISarea_land = ifelse(is.na(GISarea_land), 0, GISarea_land),
    MarineShare = 1 - GISarea_land / GISarea,
    MarineArea <- MarineShare * AREAHA, 
    AreaGroup = case_when(
      AREAHA <= 100 ~ "0-1 km2",
      AREAHA <= 200 ~ "1-2 km2",
      AREAHA <= 300 ~ "2-3 km2",
      AREAHA <= 400 ~ "3-4 km2",
      AREAHA <= 500 ~ "3-5 km2",
      AREAHA <= 1000 ~ "5-10 km2",
      AREAHA > 1000 ~ "over 10 km2",
      TRUE ~ "no size reported"
    ),
    MarineShareGroup = case_when(
      MarineShare <= 0.25 ~ "<25%",
      MarineShare <= 0.5 ~ "25%-50%",
      MarineShare <= 0.75 ~ "50%-75%",
      MarineShare <= 1 ~ ">75%",
      TRUE ~ "Others"
    ))

# dplyr::left_join(
# Global_CDDA_marine_init,
# CountryCodes,
# by = c("cddaCountryCode" = "CountryCode"))

Global_marine_CDDA <- 
  dplyr::left_join(
    dplyr::left_join(
      dplyr::left_join(
        Global_CDDA_marine_init,
        CountryCodes,
        by = c("cddaCountryCode" = "CountryCode")),
      Global_CDDA_marine_sf %>%
        st_drop_geometry() %>%
        select(cddaId, GISarea),
      by = "cddaId"),
    landArea_CDDA,
    by = "cddaId") %>%
  drop_units() %>%
  mutate(
    GISarea_land = ifelse(is.na(GISarea_land), 0, GISarea_land),
    MarineShare = 1 - GISarea_land / GISarea,
    MarineArea <- MarineShare * siteArea, 
    MarineShareGroup = case_when(
      MarineShare <= 0.25 ~ "<25%",
      MarineShare <= 0.5 ~ "25%-50%",
      MarineShare <= 0.75 ~ "50%-75%",
      MarineShare <= 1 ~ ">75%",
      TRUE ~ "Others"
    ))

Global_marine_CDDA <- 
Global_marine_CDDA %>%
  mutate(AreaGroup = case_when(
    siteArea <= 100 ~ "0-1 km2",
    siteArea <= 200 ~ "1-2 km2",
    siteArea <= 300 ~ "2-3 km2",
    siteArea <= 400 ~ "3-4 km2",
    siteArea <= 500 ~ "3-5 km2",
    siteArea <= 1000 ~ "5-10 km2",
    siteArea > 1000 ~ "over 10 km2",
    TRUE ~ "no size reported"))  

Global_marine_CDDA_test <- 
  dplyr::left_join(
    Global_CDDA_marine_test_init,
    CountryCodes,
    by = c("cddaCountryCode" = "CountryCode")) %>%
  mutate(AreaGroup = case_when(
    siteArea <= 100 ~ "0-1 km2",
    siteArea <= 200 ~ "1-2 km2",
    siteArea <= 300 ~ "2-3 km2",
    siteArea <= 400 ~ "3-4 km2",
    siteArea <= 500 ~ "3-5 km2",
    siteArea <= 1000 ~ "5-10 km2",
    siteArea > 1000 ~ "over 10 km2",
    TRUE ~ "no size reported"),
    marineAreaPercGroup =  marineAreaPercentage %/% 10 * 10
  )


#---- MPA Size range ----

SizeRange_N_Natura2000 <- Global_marine_Natura2000 %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(N = n()) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "N") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

SizeRange_N_CDDA <- Global_marine_CDDA %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(N = n()) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "N") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

SizeRange_N_CDDA_test <- Global_marine_CDDA_test %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(N = n()) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "N") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

SizeRange_km_Natura2000 <- Global_marine_Natura2000 %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(Area = sum(AREAHA, na.rm = TRUE)) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "Area") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

SizeRange_km_CDDA <- Global_marine_CDDA %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(Area = sum(siteArea, na.rm = TRUE)) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "Area") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

SizeRange_km_CDDA_test <- Global_marine_CDDA_test %>%
  group_by(Lot, CountryFile, Order, AreaGroup) %>%
  summarise(Area = sum(siteArea, na.rm = TRUE)) %>%
  arrange(match(AreaGroup, varAreaGroup)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "Area") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

#---- MPA Totals ----

Top_stat_Natura2000 <- 
  Global_marine_Natura2000 %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_N2000 = n(),
            TotArea_N2000 = round(sum(AREAHA, na.rm = TRUE)/100),
            MeanArea_N2000 = round(mean(AREAHA, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA <- 
  Global_marine_CDDA %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_CDDA = n(),
            TotArea_CDDA = round(sum(siteArea, na.rm = TRUE)/100),
            MeanArea_CDDA = round(mean(siteArea, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA_test <- 
  Global_marine_CDDA_test %>%
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

size_vs_marine_Natura2000 <- 
  Global_marine_Natura2000 %>%
  group_by(MarineShareGroup, AreaGroup) %>%
  summarise(N = n()) %>%
  # arrange(match(iucnCategory, variucnCategory)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "N") %>%
  print()

size_vs_marine_CDDA <- 
  Global_marine_CDDA %>%
  group_by(MarineShareGroup, AreaGroup) %>%
  summarise(N = n()) %>%
  # arrange(match(iucnCategory, variucnCategory)) %>%
  pivot_wider(names_from = "AreaGroup",
              values_from = "N") %>%
  print()

#---- MPA Totals for over X km2----

#### CHANGE X HERE - X in hectares
X <- 500
####
Top_stat_Natura2000_X <- 
  Global_marine_Natura2000 %>%
  filter(AREAHA >= X) %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_N2000 = n(),
            TotArea_N2000 = round(sum(AREAHA, na.rm = TRUE)/100),
            MeanArea_N2000 = round(mean(AREAHA, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA_X <- 
  Global_marine_CDDA %>%
  filter(siteArea >= X) %>%
  group_by(Lot, CountryFile, Order) %>%
  summarise(N_CDDA = n(),
            TotArea_CDDA = round(sum(siteArea, na.rm = TRUE)/100),
            MeanArea_CDDA = round(mean(siteArea, na.rm = TRUE)/100)) %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order)

Top_stat_CDDA_test_X <- 
  Global_marine_CDDA_test %>%
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

Top_stat_CDDA_Comp_test_X <- 
  dplyr::left_join(
    dplyr::left_join(
      CountryCodesBase,
      Top_stat_CDDA_test,
      by = c("Lot", "CountryFile")
    ),
    Top_stat_CDDA_test_X,
    by = c("Lot", "CountryFile"),
    suffix = c("", ".R")
  ) %>%
  print()

#---- IUCN ----


variucnCategory <- c("Ia", "Ib", "II", "III", "IV", "V", "VI",
                     "notAssigned", "notReported", "notApplicable")

IUCN_CDDA <- 
  Global_marine_CDDA %>%
  group_by(Lot, CountryFile, Order, iucnCategory) %>%
  summarise(N = n()) %>%
  arrange(match(iucnCategory, variucnCategory)) %>%
  pivot_wider(names_from = "iucnCategory",
              values_from = "N") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

IUCN_CDDA_test <- 
  Global_marine_CDDA_test %>%
  group_by(Lot, CountryFile, Order, iucnCategory) %>%
  summarise(N = n()) %>%
  arrange(match(iucnCategory, variucnCategory)) %>%
  pivot_wider(names_from = "iucnCategory",
              values_from = "N") %>%
  arrange(Order) %>%
  ungroup() %>%
  select(-Order) %>%
  print()

marine_IUCN <- 
  Global_marine_CDDA %>%
  group_by(iucnCategory, MarineShareGroup) %>%
  summarise(N = n()) %>%
  arrange(match(iucnCategory, variucnCategory)) %>%
  pivot_wider(names_from = "iucnCategory",
              values_from = "N") %>%
  print()

size_IUCN <- 
  Global_marine_CDDA %>%
  group_by(iucnCategory, AreaGroup) %>%
  summarise(N = n()) %>%
  arrange(match(iucnCategory, variucnCategory)) %>%
  pivot_wider(names_from = "iucnCategory",
              values_from = "N") %>%
  print()

limited_Global_marine_Natura2000_sf <- 
  right_join(Global_Natura2000_marine_sf, 
            Global_marine_Natura2000 
            %>% filter(AREAHA >= 500)
            %>% select(SITECODE))

st_write(limited_Global_marine_Natura2000_sf,
         "Output/Global_Natura2000_5k.gpkg", 
         "Natura2000 marine", 
         append = FALSE)

limited_Global_marine_CDDA_sf <- 
  right_join(Global_CDDA_marine_sf,
             Global_marine_CDDA
            %>% filter(siteArea >= 500)
            %>% select(cddaId))

st_write(limited_Global_marine_CDDA_sf,
         "Output/Global_CDDA_5k.gpkg", 
         "CDDA marine", 
         append = FALSE)

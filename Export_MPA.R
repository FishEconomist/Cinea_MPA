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
    "units",
    "rmapshaper",
    "maptools",
    "xlsx"
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

# EU_land <- 
#   st_read(
#     dsn = "Input/EEA_Coastline_Polygon_Shape/EEA_Coastline_20170228.shp") %>% 
#   mutate(Id = 1)

#---- Prepare data ----
Global_marine_CDDA_init_sf <- 
  Global_marine_CDDA_sf
Global_marine_Natura2000_init <- 
  Global_marine_Natura2000
Global_marine_CDDA_init <- 
  Global_marine_CDDA %>%
  filter(cddaRegionCode %notin% c("TF"))
Global_marine_CDDA_test_init <- 
  Global_marine_CDDA_test %>%
  filter(cddaRegionCode %notin% c("TF"))

Global_marine_Natura2000_sf$GISarea <-
  st_area(Global_marine_Natura2000_sf) / 1e4

Global_land_Natura2000_sf$GISarea_land <- 
  st_area(Global_land_Natura2000_sf) / 1e4

Global_only_marine_Natura2000_sf$GISarea_marine <- 
  st_area(Global_only_marine_Natura2000_sf) / 1e4

Global_marine_CDDA_init_sf$GISarea <-
  st_area(Global_marine_CDDA_init_sf) / 1e4

Global_land_CDDA_sf$GISarea_land <- 
  st_area(Global_land_CDDA_sf) / 1e4

Global_only_marine_CDDA_sf$GISarea_marine <- 
  st_area(Global_only_marine_CDDA_sf) / 1e4

landArea_Natura2000 <- 
  Global_land_Natura2000_sf %>%
  st_drop_geometry() %>%
  group_by(SITECODE) %>%
  summarise(GISarea_land = sum(GISarea_land, na.rm = TRUE))

marineArea_Natura2000 <- 
  Global_only_marine_Natura2000_sf %>%
  st_drop_geometry() %>%
  group_by(SITECODE) %>%
  summarise(GISarea_marine = sum(GISarea_marine, na.rm = TRUE))

landArea_CDDA <- 
  Global_land_CDDA_sf %>%
  st_drop_geometry() %>%
  group_by(cddaId) %>%
  summarise(GISarea_land = sum(GISarea_land, na.rm = TRUE))

marineArea_CDDA <- 
  Global_only_marine_CDDA_sf %>%
  st_drop_geometry() %>%
  group_by(cddaId) %>%
  summarise(GISarea_marine = sum(GISarea_marine, na.rm = TRUE))

CountryCodesBase <- 
  CountryCodes %>% 
  select(Lot, CountryFile, Order) %>%
  arrange(Order) %>%
  select(-Order)

varAreaGroup <- 
  c("0-1 km2",
    "1-2 km2",
    "2-3 km2",
    # "1-3 km2",
    "3-4 km2",
    "4-5 km2",
    # "3-5 km2",
    "5-10 km2",
    "over 10 km2")

Global_marine_Natura2000 <- 
  dplyr::left_join(
    dplyr::left_join(
      dplyr::left_join(
        dplyr::left_join(
          Global_marine_Natura2000_init,
          CountryCodes,
          by = c("COUNTRY_CODE" = "CountryCode")),
        Global_marine_Natura2000_sf %>%
          st_drop_geometry() %>%
          select(SITECODE, GISarea),
        by = "SITECODE"),
      landArea_Natura2000,
      by = "SITECODE"),
    marineArea_Natura2000,
    by = "SITECODE") %>%
  drop_units() %>%
  mutate(
    GISarea_land = ifelse(is.na(GISarea_land), 0, GISarea_land),
    MarineShare = 1 - GISarea_land / GISarea,
    MarineArea = MarineShare * AREAHA, 
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
    MarineShareGroup = trunc(MarineShare * 10) *10
  ) 

Global_marine_Natura2000_redux <- 
  Global_marine_Natura2000  %>%
  select(SITECODE,
         MarineShare,
         MarineArea)

Global_marine_Natura2000_sf <- 
  left_join(
    Global_marine_Natura2000_sf,
    Global_marine_Natura2000_redux)

Global_marine_CDDA <- 
  dplyr::left_join(
    dplyr::left_join(
      dplyr::left_join(
        dplyr::left_join(
          Global_marine_CDDA_init,
          CountryCodes,
          by = c("cddaCountryCode" = "CountryCode")),
        Global_marine_CDDA_init_sf %>%
          st_drop_geometry() %>%
          select(cddaId, GISarea),
        by = "cddaId"),
      landArea_CDDA,
      by = "cddaId"),
    marineArea_CDDA,
    by = "cddaId")%>%
  drop_units() %>%
  mutate(
    GISarea_land = ifelse(is.na(GISarea_land), 0, GISarea_land),
    MarineShare = 1 - GISarea_land / GISarea,
    MarineArea = MarineShare * siteArea, 
    MarineShareGroup = trunc(MarineShare * 10) *10
  ) %>%
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
    Global_marine_CDDA_test_init,
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
    MarineArea = marineAreaPercentage * siteArea  / 100, 
    MarineShare = marineAreaPercentage,
    MarineShareGroup =  marineAreaPercentage %/% 10 * 10
  ) %>%
  filter(cddaCountryCode == "FI")


Global_marine_CDDA <- 
  bind_rows(
    Global_marine_CDDA,
    Global_marine_CDDA_test) 

Global_marine_CDDA_redux <- 
  Global_marine_CDDA %>%
  select(cddaId,
         MarineShare,
         MarineArea)

Global_marine_CDDA_test_sf_FI <- 
  Global_marine_CDDA_test_sf %>%
  filter(cddaCountryCode == "FI")

Global_marine_CDDA_sf <- 
  bind_rows(
    Global_marine_CDDA_init_sf,
    Global_marine_CDDA_test_sf_FI) %>%
  left_join(Global_marine_CDDA_redux)

LimitPercentage <- 95
LimiteArea <- 500


CDDA_select <- 
  Global_marine_CDDA %>%
  filter(
    MarineArea >= LimiteArea | 
      MarineShare >= LimitPercentage) %>%
  select(-c(CountryFile,
            GISarea_land,
            GISarea,
            GISarea_marine,
            EU_work,
            Lot,
            Order,
            MarineShareGroup,
            AreaGroup))

CDDA_select_sf <- 
  Global_marine_CDDA_sf %>%
  filter(
    MarineArea >= LimiteArea | 
      MarineShare >= LimitPercentage)

Natura2000_select <- 
  Global_marine_Natura2000 %>%
  filter(
    MarineArea >= LimiteArea | 
      MarineShare >= LimitPercentage) %>%
  select(-c(CountryFile,
            GISarea_land,
            GISarea,
            GISarea_marine,
            EU_work,
            Lot,
            Order,
            MarineShareGroup,
            AreaGroup))

Natura2000_select_sf <- 
  Global_marine_Natura2000_sf %>%
  filter(
    MarineArea >= LimiteArea | 
      MarineShare >= LimitPercentage)
 

save(CDDA_select,
     CDDA_select_sf,
     Natura2000_select,
     Natura2000_select_sf,
     file = "Output/Selection/Selection_MPAs.Rdata")

st_write(CDDA_select_sf,
         "Output/Selection/CDDA_selection.gpkg", 
         "CDDA geometry",
         append = FALSE)

st_write(CDDA_select_sf,
         "Output/Selection/CDDA_selection.gpkg", 
         "CDDA data",
         append = FALSE)

st_write(Natura2000_select_sf,
         "Output/Selection/Natura2000.gpkg", 
         "Natura2000 geometry",
         append = FALSE)

st_write(Natura2000_select_sf,
         "Output/Selection/Natura2000.gpkg", 
         "Natura2000 data",
         append = FALSE)


i <- "NL"

start_time <- Sys.time()
print(start_time)

for(i in CountryCodes$CountryCode){
    
    Code <- CountryCodes %>%
      filter(CountryCode == i) %>%
      select(CountryFile) %>%
      as.list()
    
    CDDA_country <-
      CDDA_select %>% 
      filter(cddaCountryCode == i)
    
    CDDA_country_sf <-
      CDDA_select_sf %>% 
      filter(cddaCountryCode == i)
    
    Natura2000_country <-
      Natura2000_select %>% 
      filter(COUNTRY_CODE == i)
    
    Natura2000_country_sf <-
      Natura2000_select_sf %>% 
      filter(COUNTRY_CODE == i)
    
    
    dir.create(file.path(getwd(), 
                         paste0("Output/Selection/", 
                                Code[1])),
               showWarnings = FALSE)  
    
    write.xlsx2(CDDA_country, 
                file = paste0("Output/Selection/",
                              Code[1],
                              "/CDDA_", 
                              Code[1],
                              ".xlsx"), 
                sheetName = "CDDA Marine",
                col.names = TRUE, 
                row.names = FALSE, 
                append = FALSE)
    
    write.xlsx2(Natura2000_country, 
                file = paste0("Output/Selection/",
                              Code[1],
                              "/Natura2000_", 
                              Code[1],
                              ".xlsx"), 
                sheetName = "Natura2000 Marine",
                col.names = TRUE, 
                row.names = FALSE, 
                append = FALSE)
    
    save(CDDA_country,
         CDDA_country_sf,
         Natura2000_country,
         Natura2000_country_sf,
         file = 
           paste0("Output/Selection/",
                  Code[1],
                  "/Selection_",
                  Code[1],
                  ".Rdata"))
    
    st_write(CDDA_country_sf,
             paste0("Output/Selection/",
                    Code[1],
                    "/CDDA_",
                    Code[1],
                    ".gpkg"), 
             "CDDA geometry",
             append = FALSE)
    
    st_write(CDDA_country,
             paste0("Output/Selection/",
                    Code[1],
                    "/CDDA_",
                    Code[1],
                    ".gpkg"), 
             "CDDA data",
             append = TRUE)
    
    st_write(Natura2000_country_sf,
             paste0("Output/Selection/",
                    Code[1],
                    "/Natura2000",
                    Code[1],
                    ".gpkg"), 
             "Natura2000 geometry",
             append = FALSE)
    
    st_write(Natura2000_country,
             paste0("Output/Selection/",
                    Code[1],
                    "/Natura2000",
                    Code[1],
                    ".gpkg"), 
             "Natura2000 data",
             append = TRUE)
    
    end_time <- Sys.time()    
    print(end_time)
    print(end_time - start_time)
    print(paste(i, "OK"))
    
  }
  


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
    "xlsx",
    "scales",
    "lubridate",
    "sf",
    "rmapshaper"
  )

attachPackages(package.list)
source("R/Format.R")

CountryCodes <- 
  funCharAsFactor(
    read_excel(
      "Input/CountryCodes.xlsx",
      col_names = TRUE,
      sheet = "CountryCodes") %>%
      filter(EU_work == 1)
    ) %>%
  mutate(CountryFile = str_replace(Country, " ", "_"))

CountryCodesBase <- 
  CountryCodes %>% 
  select(CountryCode) %>%
  as.list()

EU_land <- 
  st_read(
    dsn = "Input/EEA_Coastline_Polygon_Shape/EEA_Coastline_20170228.shp") %>% 
  mutate(Id = 1)


  st_layers("Input/CDDA_2021_rework.gpkg")
  
  CDDA_sf <-
    funCharAsFactor(
      st_read(
        dsn = "Input/CDDA_2021_v01_public.gpkg",
        layer = "ProtectedSite")) %>%
    st_transform(3035) 

  CDDA_redux_sf <- 
    funCharAsFactor(
      st_read(
        dsn = "Input/CDDA_2021_rework.gpkg",
        layer = "CDDA_2021_rework")) %>%
    st_transform(3035)
  
  st_crs(CDDA_sf)
  
  CDDA_DesignationType <- 
    funCharAsFactor(
      st_read(
        dsn = "Input/CDDA_2021_v01_public.gpkg",
        layer = "DesignationType"))
  
  CDDA_DesignatedArea <- 
    funCharAsFactor(
      st_read(
        dsn = "Input/CDDA_2021_v01_public.gpkg",
        layer = "DesignatedArea"))
  
  save(CDDA_sf,
       CDDA_DesignationType, 
       CDDA_DesignatedArea,
       file = "Output/CDDA.Rdata")
  
  start_time <- Sys.time()
  print(start_time)
  
  for(i in CountryCodes$CountryCode){
    
    if(i %in% c("BE", "DK", "EE", "FI")){
      
      Code <- CountryCodes %>%
        filter(CountryCode == i) %>%
        select(CountryFile) %>%
        as.list()
      
      CDDA_country_sf <-
        CDDA_sf %>% 
        filter(cddaCountryCode == i)
      
      CDDA_redux_country_sf <-
        CDDA_redux_sf %>% 
        filter(cddaCountryCode == i & geomType == "polygon")
      
      if(i == "FI"){
        marine_CDDA_sf <-
          marine_CDDA_sf %>%
          filter(FALSE)
        marine_CDDA <-
          marine_CDDA %>%
          filter(FALSE)
      } else {
        marine_CDDA_sf <-
          st_join(
            CDDA_redux_country_sf,
            EU_land,
            join = st_covered_by) %>%
          filter(is.na(Id)) %>%
          select(-Id, Shape_Leng, Shape_Area)
        
        marine_CDDA <- 
          left_join(
            marine_CDDA_sf %>% 
              st_drop_geometry() %>%
              select(cddaId),
            CDDA_DesignatedArea,
            by = "cddaId")
        
      }
              
      marine_CDDA_test <- 
        CDDA_DesignatedArea %>%
        filter(cddaCountryCode == i &
                 majorEcosystemType != "terrestrial")
      
      marine_CDDA_test_sf <- 
        left_join(
          marine_CDDA_test %>% select(cddaId),
          CDDA_sf,
          by = "cddaId"
        )

      dir.create(file.path(getwd(), 
                           paste0("Output/",Code[1])),
                 showWarnings = FALSE)  
      
      write.xlsx2(marine_CDDA, 
                  file = paste0("Output/", 
                                Code[1],
                                "/CDDA_", 
                                Code[1],
                                ".xlsx"), 
                  sheetName = "CDDA Marine",
                  col.names = TRUE, 
                  row.names = FALSE, 
                  append = FALSE)
      
      
      save(marine_CDDA,
           marine_CDDA_sf,
           marine_CDDA_test,
           marine_CDDA_test_sf,
           file = 
             paste0("Output/", 
                    Code[1],
                    "/CDDA_",
                    Code[1],
                    ".Rdata"))
      
      st_write(marine_CDDA_sf,
               paste0("Output/", 
                      Code[1],
                      "/CDDA_",
                      Code[1],
                      ".gpkg"), 
               "CDDA",
               append = FALSE)
      
      st_write(CDDA_redux_country_sf,
               paste0("Output/", 
                      Code[1],
                      "/CDDA_redux_",
                      Code[1],
                      ".gpkg"), 
               "CDDA redux",
               append = FALSE)
      
      
      
    }
    
    end_time <- Sys.time()    
    print(end_time)
    print(end_time - start_time)
    print(paste(i, "OK"))
  }


Natura2000_Bioregion <- 
  st_read(
    dsn = "Input/Natura2000_end2021.gpkg",
    layer = "BIOREGION",
    stringsAsFactors = TRUE)

Natura2000_DesignationStatus <-
  st_read(
    dsn = "Input/Natura2000_end2021.gpkg",
    layer = "DESIGNATIONSTATUS",
    stringsAsFactors = TRUE)

Natura2000_DirectiveSpecies <-
  st_read(
    dsn = "Input/Natura2000_end2021.gpkg",
    layer = "DIRECTIVESPECIES",
    stringsAsFactors = TRUE)

Natura2000_HabitatClass <-
  st_read(
    dsn = "Input/Natura2000_end2021.gpkg",
    layer = "HABITATCLASS",
    stringsAsFactors = TRUE)

Natura2000_Habitats <-
  st_read(
    dsn = "Input/Natura2000_end2021.gpkg",
    layer = "HABITATS",
    stringsAsFactors = TRUE)

Natura2000_sf <-
  st_read(
    dsn = "Input/Natura2000_end2021.gpkg",
    layer = "NaturaSite_polygon",
    stringsAsFactors = TRUE) %>%
  rename(COUNTRY_CODE = MS)

Natura2000_redux_sf <-
  st_read(
    dsn = "Input/Natura_2021_rework.gpkg",
    layer = "CDDA_2021_rework",
    stringsAsFactors = TRUE) %>%
  rename(COUNTRY_CODE = MS) %>%
  select(-id)

sites_Natura2000  <-
  st_read(
    dsn = "Input/Natura2000_end2021.gpkg",
    layer = "NATURA2000SITES",
    stringsAsFactors = TRUE) 

marine_Natura2000 <- 
  sites_Natura2000  %>%
  filter(MARINE_AREA_PERCENTAGE > 0)

sites_Natura2000_MarineIndex <-
  sites_Natura2000 %>%
  select(SITECODE,
         MARINE_AREA_PERCENTAGE)

marine_Natura2000_sf <- 
  dplyr::left_join(
    Natura2000_sf,
    sites_Natura2000_MarineIndex) %>%
  filter(MARINE_AREA_PERCENTAGE > 0)

marine_Natura2000 <- 
  sites_Natura2000 %>%
  filter(MARINE_AREA_PERCENTAGE > 0)

marine_Natura2000_test <- 
  marine_Natura2000 %>%
  group_by(COUNTRY_CODE) %>%
  summarise(N = n())

sites_Natura2000_test <- 
  sites_Natura2000 %>%
  group_by(COUNTRY_CODE) %>%
  summarise(N = n())

start_time <- Sys.time()
print(start_time)

for(i in CountryCodes$CountryCode){
  
  Code <- CountryCodes %>%
    filter(CountryCode == i) %>%
    select(CountryFile) %>%
    as.list()
  
  country_Natura2000_sf <- 
    Natura2000_sf %>%
    filter(COUNTRY_CODE == i)
  
  country_Natura2000 <- 
    sites_Natura2000 %>%
    filter(COUNTRY_CODE == i)
  
  marine_Natura2000_sf <-
    st_join(
      country_Natura2000_sf,
      EU_land,
      join = st_covered_by) %>%
    filter(is.na(Id)) %>%
    select(-Id, Shape_Leng, Shape_Area)

  marine_Natura2000 <- 
    left_join(
      marine_Natura2000_sf %>% 
        st_drop_geometry() %>%
        select(SITECODE),
      country_Natura2000,
      by = "SITECODE"
    )
  
  dir.create(file.path(getwd(), 
                       paste0("Output/",Code[1])),
             showWarnings = FALSE)  
  
  write.xlsx2(marine_Natura2000, 
              file = paste0("Output/", 
                            Code[1],
                            "/Natura2000_", 
                            Code[1],
                            ".xlsx"), 
              sheetName = "Natura 2000 Marine",
              col.names = TRUE, 
              row.names = FALSE, 
              append = FALSE)
  
  st_write(marine_Natura2000_sf,
           paste0("Output/", 
                  Code[1],
                  "/Natura2000_",
                  Code[1],
                  ".gpkg"), 
           "Natura2000 Marine",
           append = FALSE)
  
  st_write(marine_Natura2000,
           paste0("Output/", 
                  Code[1],
                  "/Natura2000_",
                  Code[1],
                  ".gpkg"), 
           "Natura2000 Marine Informations", 
           append = TRUE)
  
  st_layers(paste0("Output/", 
                   Code[1],
                   "/Natura2000_",
                   Code[1],
                   ".gpkg"))
  
  save(country_Natura2000,
       country_Natura2000_sf, 
       marine_Natura2000_sf,
       marine_Natura2000,
       file = paste0("Output/", 
                     Code[1],
                     "/Natura2000_", 
                     Code[1],
                     ".Rdata")
       )
  
  end_time <- Sys.time()    
  print(end_time)
  print(end_time - start_time)
  print(paste(i, "OK"))
}

start_time <- Sys.time()
print(start_time)
for(i in CountryCodes$CountryCode){
  
  Code <- CountryCodes %>%
    filter(CountryCode == i) %>%
    select(CountryFile) %>%
    as.list()
  
  load(paste0("Output/", 
              Code[1],
              "/CDDA_",
              Code[1],
              ".Rdata"))
  
  load(paste0("Output/", 
              Code[1],
              "/Natura2000_",
              Code[1],
              ".Rdata"))
  
  if(i == "BE"){
    Global_marine_CDDA <- marine_CDDA
    Global_marine_CDDA_sf <- marine_CDDA_sf %>%
      select(-metadata_beginLifeSpanVersion)
    Global_marine_CDDA_test <- marine_CDDA_test
    Global_marine_CDDA_test_sf <- marine_CDDA_test_sf %>%
      select(-metadata_beginLifeSpanVersion)
    Global_marine_Natura2000 <- marine_Natura2000
    Global_marine_Natura2000_sf <- marine_Natura2000_sf 
  } else {
    Global_marine_CDDA <- 
      dplyr::bind_rows(
        Global_marine_CDDA,
        marine_CDDA )
    Global_marine_CDDA_sf <- 
      rbind(
        Global_marine_CDDA_sf,
        marine_CDDA_sf  %>%
          select(-metadata_beginLifeSpanVersion))
    Global_marine_CDDA_test <- 
      dplyr::bind_rows(
        Global_marine_CDDA_test,
        marine_CDDA_test )
    Global_marine_CDDA_test_sf <- 
      rbind(
        Global_marine_CDDA_test_sf,
        marine_CDDA_test_sf  %>%
          select(-metadata_beginLifeSpanVersion))
    Global_marine_Natura2000 <- 
      dplyr::bind_rows(
        Global_marine_Natura2000,
        marine_Natura2000 )
    Global_marine_Natura2000_sf <- 
      rbind(
        Global_marine_Natura2000_sf,
        marine_Natura2000_sf)
  }
  
  end_time <- Sys.time()    
  print(end_time)
  print(end_time - start_time)
  print(paste(i, "OK"))
}

Global_land_CDDA_sf <- 
  st_intersection(Global_marine_CDDA_sf,
                  EU_land)

Global_only_marine_CDDA_sf <- 
  rmapshaper::ms_erase(
    Global_marine_CDDA_sf,
    EU_land)

save(Global_marine_CDDA,
     Global_marine_CDDA_sf,
     Global_land_CDDA_sf,
     Global_only_marine_CDDA_sf,
     Global_marine_CDDA_test,
     Global_marine_CDDA_test_sf,
     file = "Input/Global_CDDA.Rdata"
)

load("Input/Global_CDDA.Rdata")

st_write(Global_marine_CDDA_sf,
         "Output/Global_CDDA.gpkg", 
         "CDDA marine subset", 
         append = TRUE)

st_write(Global_marine_CDDA_test_sf,
         "Output/Global_CDDA.gpkg", 
         "CDDA marine from ratio", 
         append = TRUE)

Global_land_Natura2000_sf <- 
  st_intersection(Global_marine_Natura2000_sf,
                  EU_land)

Global_only_marine_Natura2000_sf <- 
  rmapshaper::ms_erase(
    Global_marine_Natura2000_sf,
    EU_land)

save(Global_marine_Natura2000,
     Global_marine_Natura2000_sf, 
     Global_land_Natura2000_sf,
     Global_only_marine_Natura2000_sf,
     file = "Input/Global_Natura2000.Rdata"
)

st_write(Global_marine_Natura2000_sf,
         "Output/Global_Natura2000.gpkg", 
         "Natura2000 marine subset", 
         append = TRUE)


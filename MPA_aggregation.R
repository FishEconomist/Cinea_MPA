#---- project MPA Europe 2022-2023 ----
# 
# Loading MPA info 
#
# Coder : Sébastien Metz
# Coding August 2022 - ...
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
source("R/MPAscripts.R")

lstCodes <- 
  as.list(funCharAsFactor(
    read_excel(
      "Input/CountryCodes.xlsx",
      col_names = TRUE,
      sheet = "CountryCodes") %>%
      filter(EU_work == 1)
  ) %>%
  select(Country))

Origin_CDD_sf <-
  funCharAsFactor(
    st_read(
      dsn = "Output/Selection/CDDA_selection.gpkg",
      layer = "CDDA")) %>%
  st_transform(3035) %>%
  rename(SiteCode = cddaId,
         CountryCode = cddaCountryCode,
         RegionCode = cddaRegionCode,
         SiteName = siteName) %>%
  mutate(Origin = "CDDA") %>%
  select(-c(Shape_Leng, Shape_Area), -starts_with("id"))


Origin_CDD <- 
  funCharAsFactor(
    read_excel(
      "Input/CountryCodes.xlsx",
      col_names = TRUE,
      sheet = "CountryCodes") %>%
      filter(EU_work == 1)
  ) %>%
  mutate(CountryFile = str_replace(Country, " ", "_"))

k <- 0
for(i in lstCodes$Country){
  k <- k + 1
  
 # i <- "Denmark"
  
  print(i)
  
  load(
    paste0(
      "Output/Selection/", i, "/Selection_", i, ".Rdata"))
  
  Natura2000_country_sf <- 
    Natura2000_country_sf %>%
    # select(-c(id, Shape_Leng, Shape_Area)) %>%
    rename(SiteCode = SITECODE,
           CountryCode = COUNTRY_CODE,
           SiteType = SITETYPE,
           SiteName = SITENAME) %>%
    mutate(Origin = "Natura2000",
           GISarea = as.numeric(GISarea))
  
  Natura2000_country <- 
    Natura2000_country %>%
    rename(SiteCode = SITECODE,
           CountryCode = COUNTRY_CODE,
           SiteType = SITETYPE,
           SiteName = SITENAME)  %>%
    mutate(Origin = "Natura2000")
  
  Country <- 
    Natura2000_country %>%
    group_by(CountryCode) %>%
    summarise(N = n())
  
  CountryKeep <-
    as.character(Country$CountryCode[1])
  
  
  CDDA_country_sf <- 
    Origin_CDD_sf %>%
    filter(CountryCode == CountryKeep[1])
  
  CDDA_tmp <- 
    read_excel(
      path = paste0("Input/Origin/", i, "/CDDA_", i, ".xlsx"),
      sheet = "CDDA Marine",
      col_names = TRUE,
      # skip = 3,
      na = c("", " ", "n/a")
    )
  
  if(dim(CDDA_tmp)[1] >0) { 
    CDDA_country <- 
      CDDA_tmp }
  
  CDDA_country <- 
    CDDA_country %>%
      rename(SiteCode = cddaId,
             CountryCode = cddaCountryCode,
             RegionCode = cddaRegionCode,
             SiteName = siteName)  %>%
      mutate(Origin = "CDDA")
  
  tmpBaseNatura2000 <- 
    read_excel(
      path = paste0("Input/Excels/", i, "_QC.xlsm"),
      sheet = "Natura2000_Input file",
      col_names = TRUE,
      skip = 3,
      na = c("", " ", "n/a")
    ) %>%
    mutate(
      SiteCode = as.factor(SiteCode),
      CountryCode = CountryKeep[1],
      CountryName = i,
      Base = "Natura2000") %>%
    funCharAsFactorExt() %>%
    filter(!is.na(SiteCode))
  
  tmpSelectNatura2000 <- 
    left_join(
      tmpBaseNatura2000 %>%
        select(SiteCode),
      Natura2000_country,
      by = c("SiteCode" = "SiteCode"))
    
  tmpSelectNatura2000_sf <- 
    left_join(
      tmpBaseNatura2000 %>%
        select(SiteCode),
      Natura2000_country_sf,
      by = c("SiteCode" = "SiteCode")) %>%
    mutate(Base = "Natura2000")
  
  tmpBaseCDDA <- 
    read_excel(
      path = paste0("Input/Excels/", i, "_QC.xlsm"),
      sheet = "CDDA_input file",
      col_names = TRUE,
      skip = 3,
      na = c("", " ", "n/a")
    ) %>%
    mutate(
      SiteCode = as.factor(SiteCode),
      CountryCode = CountryKeep[1],
      CountryName = i,
      Base = "CDDA") %>%
    funCharAsFactorExt() %>%
    filter(!is.na(SiteCode)) 
  
  tmpSelectCDDA <- 
    left_join(
      tmpBaseCDDA %>%
        select(SiteCode),
      CDDA_country %>%
        mutate(SiteCode = as.factor(SiteCode)),
      by = c("SiteCode" = "SiteCode"))
  
  tmpSelectCDDA_sf <- 
    left_join(
      tmpBaseCDDA %>%
        select(SiteCode),
      CDDA_country_sf %>%
        mutate(SiteCode = as.factor(SiteCode)),
      by = c("SiteCode" = "SiteCode")) %>%
    mutate(CountryCode = CountryKeep[1])
  
  tmpBaseAdditional <- 
    read_excel(
      path = paste0("Input/Excels/", i, "_QC.xlsm"),
      sheet = "Additional MPAs_input file",
      col_names = TRUE,
      skip = 3,
      na = c("", " ", "n/a")
    ) %>%
    mutate(
      SiteCode = as.factor(SiteCode),
      CountryCode = CountryKeep[1],
      CountryName = i,
      Base = "Additional") %>%
    # rename(CountryCode = Country) %>%
    funCharAsFactorExt()  %>%
    filter(!is.na(SiteCode))

  tmpBaseAgregg <- 
    bind_rows(
      tmpBaseNatura2000,
      tmpBaseCDDA,
      tmpBaseAdditional
    )
  print(paste0("tmpBaseAgregg ", 
               dim(tmpBaseAgregg)[1],
               " - ", 
               dim(tmpBaseAgregg)[2]))
  
  tmpBaseDep <- 
    bind_rows(
      tmpSelectNatura2000,
      tmpSelectCDDA
    )
  print(paste0("tmpBaseDep ", 
               dim(tmpBaseDep)[1],
               " - ", 
               dim(tmpBaseDep)[2]))
  
  tmpBaseDep_sf <- 
    bind_rows(
      tmpSelectNatura2000_sf,
      tmpSelectCDDA_sf
    ) 
  
  print(paste0("tmpBaseDep_sf ",
               dim(tmpBaseDep_sf)[1],
               " - ", 
               dim(tmpBaseDep_sf)[2]))
  
  if(k == 1){
    BaseMPAInfo <- tmpBaseAgregg
    BaseMPAInit <- tmpBaseDep
    BaseMPAInit_sf <- tmpBaseDep_sf
    
  } else {
    BaseMPAInfo <- 
      bind_rows(
        BaseMPAInfo,
        tmpBaseAgregg
      )
    BaseMPAInit <- 
      bind_rows(
        BaseMPAInit,
        tmpBaseDep
      )
    BaseMPAInit_sf <- 
      bind_rows(
        BaseMPAInit_sf,
        tmpBaseDep_sf
      )
    
  }
  # rm(list = ls(pattern = "tmp"))
  # rm(list = ls(pattern = "CDDA"))
  # rm(list = ls(pattern = "Natura"))
  # rm(list = ls(pattern = "Country"))

}
rm(i, k)

BaseMPAInfo <- 
  BaseMPAInfo %>%
  select(-starts_with("Additional")) %>%
  mutate(Relevant = 
           as.factor(
             substr(as.character(Relevant), 1, 3))) %>%
  mutate(across(everything(), ~as.factor(as.character(.)))) %>%
  relocate("SiteCode", "SiteName", "CountryCode", "CountryName", "Base")

BaseMPAInit <- 
  BaseMPAInit %>%
  relocate("SiteCode", "SiteName", "CountryCode", "Origin")

BaseMPAInit_sf <- 
  BaseMPAInit_sf %>%
  relocate("SiteCode", "SiteName", "CountryCode", "Origin")

VarList <- 
  c("IUCNType",
    "OtherCommercial",
    "OtherHumanThreats",
    "TypeDataUsed",
    "ComGearLevel1",
    "ComGearLevel2",
    "RecGearLevel1",
    "RecGearLevel2",
    "EnforcementKind",
    "StakeholdersInvolved",
    "DesigStakeholdersWhich",
    "MeasStakeholdersWhich",
    "ObjectivesConsequences",
    "HabitatsLev1",
    "HabitatsLev2",
    "EcosystLev1",
    "EcosystLev2",
    "EcosystLev3",
    "FishStocks")

for(i in VarList){
  Name <- paste0("BaseMult_", i)
  assign(
    Name,
    ExtractMultipleColumn(VarName = i)
    )
}

BaseMult_NotRelevant <- 
  ExtractMultipleColumn(VarName = "NotRelevant",
                        relevantInit = "no")

save(list = ls(pattern = "Base"),
     file = "Input/RelevantMPA.Rdata")


st_write(BaseMPAInit_sf,
         "Input/RelevantMPA.gpkg", 
         "MPA polygons",
         append = FALSE)

st_write(BaseMPAInit,
         "Input/RelevantMPA.gpkg", 
         "Information from EEA database",
         append = TRUE)

st_write(BaseMPAInfo,
         "Input/RelevantMPA.gpkg", 
         "Information gathered during the project",
         append = TRUE)

write.xlsx(data.frame(lapply(BaseMPAInfo, as.character), 
                      stringsAsFactors=FALSE), 
            file = "Input/RelevantMPA.xlsx", 
            sheetName = "Project database",
            col.names = TRUE, 
            row.names = FALSE, 
            append = FALSE)

write.xlsx(data.frame(lapply(BaseMPAInit, as.character), 
                      stringsAsFactors=FALSE),
            file = "Input/RelevantMPA.xlsx", 
            sheetName = "EEA database",
            col.names = TRUE, 
            row.names = FALSE, 
            append = TRUE)

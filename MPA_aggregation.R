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
lstCountries <- c("Estonia", "Ireland", "Netherlands")
# lstCountries <- "Netherlands"
k <- 0
for(i in lstCountries){
  k <- k + 1
  
  print(i)
  
  load(
    paste0(
      "Output/Selection/", i, "/Selection_", i, ".Rdata"))
  
  tmpBaseNatura2000 <- 
    read_excel(
      path = paste0("Input/Excels/", i, "_QC.xlsm"),
      sheet = "Natura2000_Input file",
      col_names = TRUE,
      skip = 3
    ) %>%
    mutate(
      SiteCode = as.factor(SiteCode),
      Base = "Natura2000") %>%
    funCharAsFactorExt() %>%
    filter(Relevant == "yes")
  
  tmpSelectNatura2000 <- 
    left_join(
      tmpBaseNatura2000 %>%
        select(SiteCode),
      Natura2000_country,
      by = c("SiteCode" = "SITECODE"))
   
  tmpSelectNatura2000_sf <- 
    left_join(
      tmpBaseNatura2000 %>%
        select(SiteCode),
      Natura2000_country_sf,
      by = c("SiteCode" = "SITECODE"))
  
  tmpBaseCDDA <- 
    read_excel(
      path = paste0("Input/Excels/", i, "_QC.xlsm"),
      sheet = "CDDA_input file",
      col_names = TRUE,
      skip = 3
    ) %>%
    mutate(
      SiteCode = as.factor(SiteCode),
      Base = "CDDA") %>%
    funCharAsFactorExt() %>%
    filter(Relevant == "yes")
  
  tmpSelectCDDA <- 
    left_join(
      tmpBaseCDDA %>%
        select(SiteCode),
      CDDA_country %>%
        mutate(cddaId = as.factor(cddaId)),
      by = c("SiteCode" = "cddaId"))
  
  tmpSelectCDDA_sf <- 
    left_join(
      tmpBaseCDDA %>%
        select(SiteCode),
      CDDA_country_sf %>%
        mutate(cddaId = as.factor(cddaId)),
      by = c("SiteCode" = "cddaId"))
  
  tmpBaseAdditional <- 
    read_excel(
      path = paste0("Input/Excels/", i, "_QC.xlsm"),
      sheet = "Additional MPAs_input file",
      col_names = TRUE,
      skip = 3
    ) %>%
    mutate(
      SiteCode = as.factor(SiteCode),
      Base = "Additional") %>%
    funCharAsFactorExt() %>%
    filter(Relevant == "yes")
  
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
  rm(list = ls(pattern = "tmp"))
  rm(list = ls(pattern = "CDDA"))
  rm(list = ls(pattern = "Natura"))
  
}
rm(i, k)

save(list = ls(pattern = "Base"),
     )
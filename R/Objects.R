#---- Objects for CINEA MPAs----
# 
# Code written by: 
#         Sébastien Metz - Sakana Consultants
# Initial Code: July 2022
#


#---- Class gginS4 -----
# allow ggplot in S4 objects

setOldClass(c("gg", "ggplot"))
setClass("gginS4",
         contains = "ggplot",
         slots = c(graph = "ggplot"))

#---- Definition of object objExtract ----
setClass(
  "objExtract",
  slots = c(
    countries = "character",
    baseMPA = "character",
    relevant = "character",
    VarNames = "character",
    tblCountMPAs = "tbl"
  ))


#---- Initialisation of object objExtract ----
setMethod(
  f = "initialize" ,
  signature ="objExtract" ,
  definition = function(
    .Object,
    tblOrigin = BaseMPAInfo,
    initCountries = "*",
    initBaseMPA = "*",
    initRelevant = "yes",
    What,
    NA_what) {
    
    .Object@countries <- initCountries
    
    .Object@baseMPA <- initBaseMPA
    
    .Object@relevant <- initRelevant
    
    .Object@VarNames <- What
    
    .Object@tblCountMPAs <-
      funTabIndicators(
        tblStart = tblOrigin,
        countries = initCountries,
        baseMPA = initBaseMPA,
        relevant = initRelevant,
        VarNames = What,
        NA_what = NA_what
        )
    
    .Object@tblCountMPAs <-
      funTabIndicators(
        tblStart = tblOrigin,
        countries = initCountries,
        baseMPA = initBaseMPA,
        relevant = initRelevant,
        VarNames = What,
        NA_what = NA_what
      )
    
    return(.Object)
    
  }
)


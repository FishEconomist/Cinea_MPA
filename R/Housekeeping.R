#---- projet MPA Europe 2022-2023 ----
# 
# Scripts de Housekeeping
#
# Codeur : Sébastien Metz
# Codage effectué en janvier - 2022
#

#---- Fonction detachAllPackages ----
#
# chargement du minimum de package

detachAllPackages <- function() {
  basic.packages.blank <-  c("stats",
                             "graphics",
                             "grDevices",
                             "utils",
                             "datasets",
                             "methods",
                             "base")
  basic.packages <-
    paste("package:", basic.packages.blank, sep = "")
  
  package.list <- 
    search()[
      ifelse(unlist(gregexpr("package:",
                             search())) == 1,
             TRUE,
             FALSE)]
  
  package.list <- setdiff(package.list, basic.packages)
  
  if (length(package.list) > 0)
    for (package in package.list) {
      detach(package, character.only = TRUE)
      print(paste("package ",
                  package,
                  " detached",
                  sep = ""))
    }
}

#---- Fonction attachPackages ----
#
# chargement du minimum de package

attachPackages <- function(package.list,
                           repository = "http://cran.us.r-project.org") {
  for (package in package.list) {
    if (!require(package,
                 character.only = TRUE)) {
      install.packages(package,
                       repos = repository)
      require(package,
              character.only = TRUE)
    }
  }
}

`%notin%` <- Negate(`%in%`)
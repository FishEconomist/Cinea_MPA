


source("R/Housekeeping.R")

detachAllPackages()

package.list <-
  c("tidyverse",
    "data.table",
    "shiny",
    "shinydashboard",
    "Hmisc",
    "rlist",
    "xtable",
    "DMwR2")

attachPackages(package.list)
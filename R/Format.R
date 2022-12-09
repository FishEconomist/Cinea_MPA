#---- projet EGIS - AO5 ----
# 
# Scripts de formattage
#
# Préparation de rapport pour le projet EGIS - AO5
# Codeur : Sébastien Metz
# Codage effectué en janvier - 2022
#

#---- function CharAsFactor -----
funCharAsFactor <- 
  function(
    DF,
    origin){
  DF_Factor <- 
    DF %>% 
    mutate_at(vars(starts_with("Date")), as.Date, format = "%d/%m/%Y") %>%
    mutate(across(where(is_character), as_factor))
  
  return(DF_Factor)
}

#---- function CharAsFactorExt -----
funCharAsFactorExt <- 
  function(
    DF,
    origin){
    DF_Factor <- 
      DF %>%
      mutate_all(as_factor) %>% 
      mutate_at(vars(starts_with("Date")), as.Date, format = "%d/%m/%Y")
    
    return(DF_Factor)
  }


#---- function FirstUpper ----
funFirstUpper <- 
  function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#---- function Percent ----
funPercent <- 
  function(
    x,
    digits = 2, 
    format = "f",
    ...) {
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

#---- function FrenchPercent ----
funFrenchPercent <- 
  label_percent(
    decimal.mark = ",")

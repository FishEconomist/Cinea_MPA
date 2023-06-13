#---- project MPA Europe 2022-2023  ----
# 
# MPA specific scripts
#
# Project: MPA Europe 2022-2023 
# Coder: Sébastien Metz
# Code: January 2022 - ...
#


#---- fonction funAllVariable -----
# Extract the levels of a variable from any table
funAllVariable <- 
  function(
    tblStart,
    codeVariable
  ){
    
    checkVariable <-
      tblStart %>%
      select_at(codeVariable) %>%
      unique()
    
    listVariable <-
      as.character(checkVariable[[codeVariable]])
    
    return(listVariable)
  }


#---- function ExtractMultipleColumn -----
ExtractMultipleColumn <- 
  function(Base = BaseMPAInfo,
           VarName,
           relevantInit = "yes"){
    
    # for(i in VarName){
    #   Base <- 
    #     Base %>%
    #     mutate({{i}} := as.factor(str_replace_all(.data[[i]], ",", ";")))
    # }
    
    Top <- 
      max(str_count(pull(Base[VarName], 1), ";"),
          na.rm = TRUE)
    
    BaseValue <- 
      Base %>%
      filter(Relevant == relevantInit) %>%
      select(SiteCode, 
             CountryCode,
             CountryName,
             Base,
             Relevant,
             all_of(VarName)) %>%
      separate(VarName, 
               sep = ";",
               into = paste0("Var_", 
                             seq(1, Top+1)),
               fill = "right") %>%
      pivot_longer(starts_with("Var"),
                   names_to = "tmpCol",
                   values_to = "Value") %>%
      filter(!is.na(Value)) %>%
      select(-tmpCol) %>%
      mutate(Value = as.factor(trimws(Value))) %>%
      rename({{VarName}} := Value)
    
  }

#---- fonction funGenerateIndicators -----
# 
funTabIndicators <- 
  function(
    grouped = FALSE,
    tblStart,
    countries,
    baseMPA,
    relevant,
    VarNames,
    NA_what
    ) {
    
    if (countries[1] == "*") {
      countriesInit <- 
        funAllVariable(
          tblStart,
          "CountryName")
    } else {
      countriesInit <- countries
    }
    
    if (baseMPA[1] == "*") {
      baseMPAList <- 
        funAllVariable(
          tblStart,
          "Base")
    } else {
      baseMPAList <- baseMPA
    }
    
    if (relevant[1] == "*") {
      relevantInit <- 
        funAllVariable(
          tblStart,
          "Relevant")
    } else {
      relevantInit <- relevant
    }
    
    What <- c("CountryName", VarNames)
    
    tblFinal <-
      tblStart %>%
      filter(
        CountryName %in% countriesInit,
        Base %in% baseMPAList,
        Relevant %in% relevantInit
        ) %>%
      select(all_of(What)) %>%
      mutate(across(VarNames,  ~fct_explicit_na(., na_level=NA_what)))
    
    if(grouped){
      tblFinal <- 
        tblFinal %>%
        group_by(CountryName)
    } else {
      tblFinal <- 
        tblFinal %>%
        group_by_at(What)
    }  
      
    tblFinal <- 
      tblFinal %>%
      summarise(N = n(),
                .groups = "drop") %>%
      pivot_wider(
        names_from = "CountryName",
        values_from = "N"
      ) %>%
      mutate(AllMS = rowSums(across(where(is.numeric)), na.rm = TRUE))
    
    return(tblFinal)
    
    
  }


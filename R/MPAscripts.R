#---- project MPA Europe 2022-2023  ----
# 
# MPA specific scripts
#
# Project: MPA Europe 2022-2023 
# Coder: Sébastien Metz
# Code: January 2022 - ...
#

#---- function ExtractMultipleColumn -----
ExtractMultipleColumn <- 
  function(Base = BaseMPAInfo,
           VarName){
    
    Top <- 
      max(str_count(pull(Base[VarName], 1), ";"),
          na.rm = TRUE)
    
    BaseValue <- 
      Base %>%
      select(SiteCode, VarName) %>%
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
      mutate(Value = as.factor(trimws(Value)))
    
  }
# 
# BaseMPAIUCN2 <- 
#   BaseMPAInfo %>%
#   select(SiteCode, IUCNType) %>%
#   separate(IUCNType, 
#            sep = ";",
#            into = paste0("IUCN_", 
#                          seq(1, Top+1)),
#            fill = "right") %>%
#   pivot_longer(starts_with("IUCN"),
#                names_to = "tmpCol",
#                values_to = "IUCNType") %>%
#   filter(!is.na(IUCNType)) %>%
#   select(-tmpCol) %>%
#   mutate(IUCNType = as.factor(trimws(IUCNType)))
# 
# 
# levels(BaseMPAIUCN$IUCNType)
# 
# save(list = ls(pattern = "Base"),
#      file = "Input/RelevantMPA.Rdata")

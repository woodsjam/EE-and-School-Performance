EEMs <-read.csv("https://docs.google.com/spreadsheets/d/15X3x4KCEx2-_JYLjq3gXhqS33EKBscW_LLPO-MggDFk/pub?gid=1734037341&single=true&output=csv")

library(dplyr)
library(knitr)

# Convert cost savings to numeric
EEMs %>%
  mutate(
    ElectricitySave = as.numeric(gsub( "[$,]", "",as.character(Energy.Cost.Savings_Electricity))),
    NGCostSave = as.numeric(gsub( "[$,]", "",as.character(Energy.Cost.Savings_Natural.Gas))),
    DieselCostSave = as.numeric(gsub( "[$,]", "",as.character(Energy.Cost.Savings_Diesel))),
    PropaneCostSave = as.numeric(gsub( "[$,]", "",as.character(Energy.Cost.Savings_Propane))), 
    OilCostSave = as.numeric(gsub( "[$,]", "",as.character(Energy.Cost.Savings_..5.Oil))),
    SteamCostSave = as.numeric(gsub( "[$,]", "",as.character(Energy.Cost.Savings_Steam)))
         ) -> EEMs

EEMs %>%
  rowwise() %>%
  mutate(TotalCostSave =  sum(ElectricitySave, NGCostSave, DieselCostSave, PropaneCostSave, OilCostSave, SteamCostSave, na.rm = TRUE)) -> EEMs


## Categorize
## 
EEMs %>%
  select(MeasureTypeDescription:AuditMeasureUserSuppliedDescription) %>%
  unique() %>%
  View()

  
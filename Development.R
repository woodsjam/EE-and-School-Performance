
#Read in data
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

# These are handcoded classifications of the EEMs into placebo and educationally important.
Categorize <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQTPacqSITvg9cwb2FqO8heISBbaXyDrzjSTOeYeoLWB0PdnUXYuXokE_TTej6fwemkAakIxuxZukBx/pub?gid=1802982604&single=true&output=csv")

# Merge in categorization and make sure there are no NAs

EEMs<-EEMs %>%
  left_join(Categorize, suffix = c("",".KILL")  ) %>%
  select( - contains(".KILL")) %>%
  mutate(Placebo = if_else(is.na(Placebo), FALSE, TRUE)) %>%
  mutate(Effective = if_else(is.na(Effective), FALSE, TRUE))

#Get the year definitions lined up with test score and other data

EEMs<-EEMs %>%
  mutate(Year = if_else(as.numeric(ImplementedMonth) > 5, as.numeric(ImplementedYear), as.numeric(ImplementedYear) + 1))


# Spread the kind of EEM into types

EEMs %>%
  mutate(   
    Controls =if_else(MeasureClassDetailDescription == "Controls", TRUE, FALSE),
    FixtureMod =if_else(MeasureClassDetailDescription == "Fixture Modification", TRUE, FALSE),
    Boiler =if_else(MeasureClassDetailDescription == "Boiler Equipment", TRUE, FALSE),
    Lamp =if_else(MeasureClassDetailDescription == "Lamp Modification", TRUE, FALSE),  
    Distribution  =if_else(MeasureClassDetailDescription == "Distribution System", TRUE, FALSE),          
    Chiller  = if_else(MeasureClassDetailDescription == "Chiller/AC Equipment", TRUE, FALSE),         
    Insulation  = if_else(MeasureClassDetailDescription == "Insulation", TRUE, FALSE),                   
    Windows  = if_else(MeasureClassDetailDescription == "Windows", TRUE, FALSE),                      
    Other  = if_else(MeasureClassDetailDescription == "Other", TRUE, FALSE),                        
    Maintenance  = if_else(MeasureClassDetailDescription == "Maintenance", TRUE, FALSE),                  
    Flow  = if_else(MeasureClassDetailDescription == "Flow Issues", TRUE, FALSE),                  
    Doors  = if_else(MeasureClassDetailDescription == "Doors", TRUE, FALSE),                        
    HeatRecovery = if_else(MeasureClassDetailDescription == "Heat Recovery Options", TRUE, FALSE)     
    ) -> EEMs

## Compress by site/year with any true yielding true, or
## 
## 

EEMs %>%
  group_by(FacilityODEId, Year) %>%
  summarise(Controls  = any(Controls),
            FixtureMod = any(FixtureMod),
            Boiler = any(Boiler),
            Lamp = any(Lamp),
            Distribution = any(Distribution),
            Chiller = any(Chiller),
            Insulation = any(Insulation),
            Windows = any(Windows),                               
            Other = any(Other),
            Maintenance = any(Maintenance),                     
            Flow = any(Flow),
            Doors = any(Doors),  
            HeatRecovery = any(HeatRecovery),
            Placebo = any(Placebo),
            Effective = any(Effective),
            TotalCostSave = sum(TotalCostSave)
  ) -> EEMSummary

  

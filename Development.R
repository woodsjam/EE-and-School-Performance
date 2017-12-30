DistrictFinance <- read.csv("https://s3-us-west-2.amazonaws.com/woodsjeeschools/ActualExpendituresFundFunction30.CSV") 

library(stringr)
library(dplyr)

#Convert to starting of fiscal year.  Remember that the year may not match with the test scores.
DistrictFinance$YearBeginning <- as.numeric(str_sub(as.character(DistrictFinance$SchlYr),-4) )

DistrictFinance$YearEnding <-  DistrictFinance$YearBeginning +1

DistrictFinance %>%
  count(FundDesc,FuncDesc) %>%
  View()


DistrictFinance %>% 
  filter( FuncDesc == 'Building Acquisition; Construction; and Improvement Services' | FuncDesc == 'Facilities Acquisition and Construction' | FuncDesc == 'Site Acquisition and Development Services') %>% 
  mutate(ExpType = 'Capital') -> CapitalExpenditures

DistrictFinance %>% 
  filter( FuncDesc == 'Operation and Maintenance of Plant Services' ) %>%
  mutate( ExpType = 'Operations' ) -> PlantOperations

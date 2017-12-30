Schools16 <- read.csv("http://www.ode.state.or.us/data/reportcard/media/17/RCmediaSchoolsAggregate.csv")

Schools14 <- read.csv("http://www.ode.state.or.us/data/reportcard/media/15/RCmediaSchoolsAggregate.csv")
# make the school identifiers uniform witth 16 name
# 

Schools13 <- read.csv("http://www.ode.state.or.us/data/reportcard/media/14/RCmediaSchoolsAggregate.csv")

library(dplyr)


###
Schools13 %>% 
  select( DistrictID = District.ID, SchoolID = School.ID,
    Reading.Pct.Met_10 = Reading.Pct.Met.2009.10,
    Math.Pct.Met_10 = Math.Pct.Met.2009.10,  
    Writing.Pct.Met_10 = Writing.Pct.Met.2009.10,  
    Science.Pct.Met_10 = Science.Pct.Met.2009.10,
  ) -> Schools13

Schools14 %>%
  select(DistrictID = District.ID, SchoolID = School.ID,
                        
         Reading.Pct.Met_11 = Reading.Pct.Met.2010.11,               
         Reading.Pct.Met_12 = Reading.Pct.Met.2011.12,               
         Reading.Pct.Met_13 = Reading.Pct.Met.2012.13,
         Reading.Pct.Met_14 = Reading.Pct.Met.2013.14,
         
         Math.Pct.Met_11 = Math.Pct.Met.2010.11,                  
         Math.Pct.Met_12 = Math.Pct.Met.2011.12,                  
         Math.Pct.Met_13 = Math.Pct.Met.2012.13,
         Math.Pct.Met_14 = Math.Pct.Met.2013.14,

         Writing.Pct.Met_11 = Writing.Pct.Met.2010.11,               
         Writing.Pct.Met_12= Writing.Pct.Met.2011.12,               
         Writing.Pct.Met_13 = Writing.Pct.Met.2012.13,        
         Writing.Pct.Met_14 = Writing.Pct.Met.2013.14,
         
                        
         Science.Pct.Met_11 = Science.Pct.Met.2010.11,               
         Science.Pct.Met_12 = Science.Pct.Met.2011.12,               
         Science.Pct.Met_13 = Science.Pct.Met.2012.13,
         Science.Pct.Met_14 = Science.Pct.Met.2013.14
) -> Schools14


Schools16 %>%
  select(DistrictID = District.ID, District = District.Name, SchoolID = School.ID, School = School.Name, Grades.Offered, School.Type,
         ELA.Pct.Met_15 = ELA.Pct.Met.2014.15,                      
         ELA.Pct.Met_16 = ELA.Pct.Met.2015.16, 
         Math.Pct.Met_15 = Math.Pct.Met.2014.15,                     
         Math.Pct.Met_16 = Math.Pct.Met.2015.16,  
         Science.Pct.Met_15= Science.Pct.Met.2014.15,                  
         Science.Pct.Met_16 = Science.Pct.Met.2015.16         
) -> Schools16        

full_join(Schools14, Schools16, by = c("DistrictID", "SchoolID"), suffix = c("", ".KILL")) %>%
  full_join(Schools13, by = c("DistrictID", "SchoolID"), suffix = c("", ".KILL")) %>%
  select(-contains(".KILL")) -> TestScores

remove(Schools14, Schools16, Schools13)

library(dplyr)
library(tidyr)

#Percent pass are top coded at 95% and bottom coded at 5%

TestScores %>% 
  gather(Test, value, contains("met")) %>%
  separate(Test, into = c("Test", "Year"),sep = "_") %>%
  mutate(value = as.numeric(case_when(
    value == ">95"  ~ "95",
    value == "<5" ~ "5",
    value == "*" ~ "NA",
    TRUE ~ value
    ))) %>%
select(DistrictID:School.Type, Year, Test, value)%>% 
  spread(Test, value) %>%
  mutate(Year =  as.numeric(Year) + 2000) -> TestScores
  


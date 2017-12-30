
# Enrollment Reports
# 

library(readxl)

# Read in from local.  Error in http read so resorted to this.

Membership10 <- read_excel("MembershipReport/fallmembershipreport_20092010.xls", sheet = "School")

Membership11 <- read_excel("MembershipReport/fallmembershipreport_20102011.xls", sheet = "School (Final)")

Membership12 <- read_excel("MembershipReport/fallmembershipreport_20112012.xls", sheet = "School (Final)")

Membership13 <- read_excel("MembershipReport/fallmembershipreport_20122013.xls", sheet = "School (Final)")

Membership14 <- read_excel("MembershipReport/fallmembershipreport_20132014.xls", sheet = "School (1314)")

Membership15 <- read_excel("MembershipReport/fallmembershipreport_20142015.xls", sheet = "School (1415)")

Membership16 <- read_excel("MembershipReport/fallmembershipreport_20152016.xlsx", sheet = "School (15-16)")

Membership17 <- read_excel("MembershipReport/fallmembershipreport_20162017.xlsx", sheet = "School (16-17)")

#Add year indicator to each dataframe
#
Membership10$Year <- 2010
Membership11$Year <- 2011
Membership12$Year <- 2012
Membership13$Year <- 2013
Membership14$Year <- 2014
Membership15$Year <- 2015
Membership16$Year <- 2016
Membership17$Year <- 2017

# Create common colum names for  data we need
# 

CommonNames1217 <-  c("DistrictID"              
                      ,"District"                                       
                      ,"SchoolID"                
                      ,"School"                                         
                      ,"PrevEnrollment"                       
                      ,"Enrollment"                       
                      ,"Male"                                  
                      ,"PercentMale"                             
                      ,"Female"                                 
                      ,"PercentFemale"                            
                      ,"AmericanIndianAlaskaNative"         
                      ,"PercentAmericanIndianAlaskaNative"    
                      ,"Asian"                                  
                      ,"PercentAsian"                             
                      ,"NativeHawaiianPacificIslander"      
                      ,"PercentNativeHawaiianPacificIslander"
                      ,"AfricanAmerican"                 
                      ,"PercentAfricanAmerican"           
                      ,"Hispanic"                       
                      ,"PercentHispanic"                  
                      ,"White"                                  
                      ,"PercentWhite"                            
                      ,"Multiracial"                            
                      ,"PercentMultiracial"                       
                      ,"Kindergarten"                           
                      ,"GradeOne"                              
                      ,"GradeTwo"                              
                      ,"GradeThree"                            
                      ,"GradeFour"                             
                      ,"GradeFive"                             
                      ,"GradeSix"                              
                      ,"GradeSeven"                            
                      ,"GradeEight"                            
                      ,"GradeNine"                             
                      ,"GradeTen"                              
                      ,"GradeEleven"                           
                      ,"GradeTwelve"                           
                      ,"Year") 



names(Membership12) <- CommonNames1217
names(Membership13) <- CommonNames1217
names(Membership14) <- CommonNames1217
names(Membership15) <- CommonNames1217
names(Membership16) <- CommonNames1217
names(Membership17) <- CommonNames1217

# Only deal with counts not percents
# 
Membership12 %>% select(-contains("Percent")) -> Membership12
Membership13 %>% select(-contains("Percent")) -> Membership13
Membership14 %>% select(-contains("Percent")) -> Membership14
Membership15 %>% select(-contains("Percent")) -> Membership15
Membership16 %>% select(-contains("Percent")) -> Membership16
Membership17 %>% select(-contains("Percent")) -> Membership17

Membership10 %>% select(-contains("%")) -> Membership10
Membership11 %>% select(-contains("%")) -> Membership11



Membership10 %>%
  rename(
    DistrictID = "Attnd\nDistInstID",
    SchoolID = "Attnd\nSchlInstID",  
    Enrollment = "2009-10\nTotal Enrollment",                 
    Male = "2009-10 Male",                                    
    Female ="2009-10\nFemale",                                                      
    AmericanIndianAlaskaNative="2009-10\nAmerican Indian/Alaskan Native\n(Non-Hispanic)",                  
    
    AfricanAmerican= "2009-10\nBlack/African American\n(Non-Hispanic)",
    Hispanic= "2009-10\nHispanic/Latino",                                                 
    White = "2009-10\nWhite\n(Non-Hispanic)",                                           
    Multiracial = "2009-10\nMultiracial\n(Non-Hispanic)",
    
    Kindergarten = "2009-10 Kindergarten",                                                     
    GradeOne = "2009-10 Grade One",                                                        
    GradeTwo = "2009-10 Grade Two",                                                        
    GradeThree = "2009-10 Grade Three",                                                      
    GradeFour = "2009-10 Grade Four",                                                       
    GradeFive = "2009-10 Grade Five",                                                       
    GradeSix = "2009-10 Grade Six",                                                        
    GradeSeven = "2009-10 Grade Seven",                                                      
    GradeEight = "2009-10 Grade Eight",                                                      
    GradeNine = "2009-10 Grade Nine",                                                       
    GradeTen = "2009-10 Grade Ten",                                                        
    GradeEleven = "2009-10 Grade Eleven",                                                     
    GradeTwelve = "2009-10 Grade Twelve",                                                     
    NativeHawaiianPacificIslander = `For Future Reference\n2009-10\nPacific Islander\n(Non-Hispanic)`,
    
    Asian = `For Future Reference\n2009-10\nAsian\n(Non-Hispanic)`
  ) %>%
  select( -contains("2009-10")) -> Membership10

Membership10$PrevEnrollment <- NA

Membership11 %>% 
  rename( DistrictID = "Attending\nDistrict\nInstID",                                                                                       
          SchoolID = "Attending\nSchool\nInstID",                                                     
          PrevEnrollment = "2009-10\nTotal Enrollment",                                                     
          Enrollment = "2010-11\nTotal Enrollment",                                                     
          Male = "2010-11 Male",                                                                  
          Female = "2010-11 Female",                                                                
          AmericanIndianAlaskaNative = "2010-11\nAmerican Indian/Alaskan Native\n(Non-Hispanic)",                       
          
          AfricanAmerican = "2010-11\nBlack/African American\n(Non-Hispanic)",                              
          Hispanic = "2010-11\nHispanic/Latino",                                                      
          White = "2010-11\nWhite\n(Non-Hispanic)",                                                
          Multiracial = "2010-11\nMultiracial\n(Non-Hispanic)",                                          
          Kindergarten = "2010-11\nKindergarten",                                                         
          GradeOne = "2010-11\nGrade One",                                                            
          GradeTwo = "2010-11\nGrade Two",                                                            
          GradeThree = "2010-11\nGrade Three",                                                          
          GradeFour = "2010-11\nGrade Four",                                                           
          GradeFive = "2010-11\nGrade Five",                                                           
          GradeSix = "2010-11\nGrade Six",                                                            
          GradeSeven = "2010-11\nGrade Seven",                                                          
          GradeEight = "2010-11\nGrade Eight",                                                          
          GradeNine = "2010-11\nGrade Nine",                                                           
          GradeTen = "2010-11\nGrade Ten",                                                            
          GradeEleven = "2010-11\nGrade Eleven",                                                         
          GradeTwelve = "2010-11\nGrade Twelve",                                   
          Asian = "For Future Reference\n2010-11\nAsian only\n(Non-Hispanic)", 
          
          NativeHawaiianPacificIslander  = "2010-11\nAsian/Pacific Islander\n(Non-Hispanic)",            
          Year  = "Year" 
  ) %>%   
  select( -contains("2010-11")) -> Membership11

# Enrollment and PrevEnrollment contains notes.  Set those to NA
Membership10$Enrollment <- as.numeric(Membership10$Enrollment)
Membership11$Enrollment <- as.numeric(Membership11$Enrollment)
Membership12$Enrollment <- as.numeric(Membership12$Enrollment)
Membership13$Enrollment <- as.numeric(Membership13$Enrollment)
Membership14$Enrollment <- as.numeric(Membership14$Enrollment)
Membership15$Enrollment <- as.numeric(Membership15$Enrollment)
Membership16$Enrollment <- as.numeric(Membership16$Enrollment)
Membership17$Enrollment <- as.numeric(Membership17$Enrollment)

Membership10$PrevEnrollment <- as.numeric(Membership10$PrevEnrollment)
Membership11$PrevEnrollment <- as.numeric(Membership11$PrevEnrollment)
Membership12$PrevEnrollment <- as.numeric(Membership12$PrevEnrollment)
Membership13$PrevEnrollment <- as.numeric(Membership13$PrevEnrollment)
Membership14$PrevEnrollment <- as.numeric(Membership14$PrevEnrollment)
Membership15$PrevEnrollment <- as.numeric(Membership15$PrevEnrollment)
Membership16$PrevEnrollment <- as.numeric(Membership16$PrevEnrollment)
Membership17$PrevEnrollment <- as.numeric(Membership17$PrevEnrollment)

# Join them
Membership <- bind_rows(Membership10, Membership11, Membership12, Membership13, Membership14, Membership15, Membership16, Membership17) 

# Clean space
remove(Membership10, Membership11, Membership12, Membership13, Membership14, Membership15, Membership16, Membership17, CommonNames1217)

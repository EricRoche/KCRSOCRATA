#Satisfaction Data

#Get 311 Survey Data
library(RSocrata)
Survey <- read.socrata("https://data.kcmo.org/311/311-Call-Center-Service-Requests-Survey-Data/hfbv-um74")

#Data Cleaning
Survey$CREATION.DATE <- as.POSIXct(Survey$CREATION.DATE)
Survey$CLOSED.DATE <- as.POSIXct(Survey$CLOSED.DATE)
Survey$Very.Satisfied <- ifelse(Survey$QUALITY.OF.SERVICE == "5", 1, 0)
Survey$Satisfied <- ifelse(Survey$QUALITY.OF.SERVICE == "4", 1, 0)


#Roll Data Together
library(dplyr)
SurveyStats <- Survey %>% 
  filter(STATUS == "RESOL" | STATUS == "OPEN") %>%
  group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
  summarise(
    Mean.Quality.Of.Service = mean(QUALITY.OF.SERVICE),
    Median.Quality.Of.Service = median(QUALITY.OF.SERVICE),
    Number.of.Very.Satisfied.Respones = count()
    
    )

    
    


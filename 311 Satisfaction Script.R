#Satisfaction Data

#Get 311 Survey Data
library(RSocrata)
Survey <- read.socrata("https://data.kcmo.org/311/311-Call-Center-Service-Requests-Survey-Data/hfbv-um74")

#Data Cleaning
Survey$CREATION.DATE <- as.POSIXct(Survey$CREATION.DATE)
Survey$CLOSED.DATE <- as.POSIXct(Survey$CLOSED.DATE)
summarise(Survey, Total.Responses.For.Quality.Of.Service = count(is.na(QUALITY.OF.SERVICE)))
 tasummarise(Survey, Total.Responses.For.Timeliness.Of.Service = n(),na.rm=TRUE)

#Creating Bivariate Variables
Survey$Very.Satisfied.With.Quality.Of.Service <- ifelse(Survey$QUALITY.OF.SERVICE == "5", 1, 0)
Survey$Satisfied.With.Quality.Of.Service <- ifelse(Survey$QUALITY.OF.SERVICE == "4", 1, 0)
Survey$Neutral.With.Quality.Of.Service <- ifelse(Survey$QUALITY.OF.SERVICE == "3", 1, 0)
Survey$Dissatisfied.With.Quality.Of.Service <- ifelse(Survey$QUALITY.OF.SERVICE == "2", 1, 0)
Survey$VeryDissatisfied.With.Quality.Of.Service <- ifelse(Survey$QUALITY.OF.SERVICE == "1", 1, 0)
Survey$Very.Satisfied.With.Timeliness.Of.Service <- ifelse(Survey$TIMELINESS.OF.SERVICE == "5", 1, 0)
Survey$Satisfied.With.Timeliness.Of.Service <- ifelse(Survey$TIMELINESS.OF.SERVICE == "4", 1, 0)
Survey$Neutral.With.Timeliness.Of.Service <- ifelse(Survey$TIMELINESS.OF.SERVICE == "3", 1, 0)
Survey$Dissatisfied.With.Timeliness.Of.Service <- ifelse(Survey$TIMELINESS.OF.SERVICE == "2", 1, 0)
Survey$VeryDissatisfied.With.Timeliness.Of.Service <- ifelse(Survey$TIMELINESS.OF.SERVICE == "1", 1, 0)
Survey$Very.Satisfied.With.Customer.Service <- ifelse(Survey$CUSTOMER.SERVICE == "5", 1, 0)
Survey$Satisfied.With.Customer.Service <- ifelse(Survey$CUSTOMER.SERVICE == "4", 1, 0)
Survey$Neutral.With.Customer.Service <- ifelse(Survey$CUSTOMER.SERVICE == "3", 1, 0)
Survey$Dissatisfied.With.Customer.Service <- ifelse(Survey$CUSTOMER.SERVICE == "2", 1, 0)
Survey$VeryDissatisfied.With.Customer.Service <- ifelse(Survey$CUSTOMER.SERVICE == "1", 1, 0)

#Creating Bivariate Variables but with less discrete categories. 
#These are useful for checking if someone was satisfied, neutral, or not without being overly complicated.
Survey$Very.Satisfied.And.Satisfied.With.Quality.Of.Service <- ifelse(Survey$QUALITY.OF.SERVICE == "5" | Survey$QUALITY.OF.SERVICE == "4", 1, 0)
Survey$Very.Dissatisfied.And.Dissatisfied.With.Quality.Of.Service <- ifelse(Survey$QUALITY.OF.SERVICE == "2" | Survey$QUALITY.OF.SERVICE == "1", 1, 0)

#Roll Data Together
library(dplyr)
SurveyStats <- Survey %>% 
  filter(STATUS == "RESOL" | STATUS == "OPEN") %>%
  group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
  summarise(
    Mean.Quality.Of.Service = mean(QUALITY.OF.SERVICE, na.rm=T),
    Median.Quality.Of.Service = median(QUALITY.OF.SERVICE, na.rm=T),
    Number.Of.Responses.Satisfied.With.Quality.Of.Service = sum(QUALITY.OF.SERVICE == 3 | QUALITY.OF.SERVICE == 4 | QUALITY.OF.SERVICE == 5, na.rm=T),
    Number.Of.Responses.Dissatisfied.With.Quality.Of.Service = sum(QUALITY.OF.SERVICE == 1 | QUALITY.OF.SERVICE == 2, na.rm=T),
    Total.Quality.Of.Service.Survey.Responses = sum(Number.Of.Responses.Satisfied.With.Quality.Of.Service + Number.Of.Responses.Dissatisfied.With.Quality.Of.Service),
    Number.Of.Responses.Satisfied.With.Timeliness.Of.Service = sum(TIMELINESS.OF.SERVICE == 3 | TIMELINESS.OF.SERVICE == 4 | TIMELINESS.OF.SERVICE == 5, na.rm=T),
    Number.Of.Responses.Dissatisfied.With.Timeliness.Of.Service = sum(TIMELINESS.OF.SERVICE == 1 | TIMELINESS.OF.SERVICE == 2, na.rm=T),
    Total.Timeliness.Of.Service.Survey.Responses = sum(Number.Of.Responses.Satisfied.With.Timeliness.Of.Service + Number.Of.Responses.Dissatisfied.With.Timeliness.Of.Service),
    Number.Of.Responses.Satisfied.With.Customer.Service = sum(CUSTOMER.SERVICE == 3 | CUSTOMER.SERVICE == 4 | CUSTOMER.SERVICE == 5, na.rm=T),
    Number.Of.Responses.Dissatisfied.With.Customer.Service = sum(CUSTOMER.SERVICE == 1 | CUSTOMER.SERVICE == 2, na.rm=T),
    Total.Customer.Service.Survey.Resonses = sum(Number.Of.Responses.Satisfied.With.Customer.Service + Number.Of.Responses.Dissatisfied.With.Customer.Service),
    Percent.Satisfied.With.Quality.Of.Service = (Number.Of.Responses.Satisfied.With.Quality.Of.Service/Total.Quality.Of.Service.Survey.Responses),
    Percent.Dissatisfied.With.Quality.Of.Service = (Number.Of.Responses.Dissatisfied.With.Quality.Of.Service/Total.Quality.Of.Service.Survey.Responses),
    Percent.Satisfied.With.Timeliness.Of.Service = (Number.Of.Responses.Satisfied.With.Timeliness.Of.Service/Total.Timeliness.Of.Service.Survey.Responses),
    Percent.Dissatisfied.With.Timeliness.Of.Service = (Number.Of.Responses.Satisfied.With.Timeliness.Of.Service/Total.Timeliness.Of.Service.Survey.Responses),
    Percent.Satisfied.With.Customer.Service = (Number.Of.Responses.Satisfied.With.Customer.Service/Total.Customer.Service.Survey.Resonses),
    Percent.Dissatisfied.With.Customer.Service = (Number.Of.Responses.Dissatisfied.With.Customer.Service/Total.Customer.Service.Survey.Resonses))


#Null values are causing an error. 
http://stackoverflow.com/questions/26665319/removing-na-in-dplyr-pipe
http://stackoverflow.com/questions/1923273/counting-the-number-of-elements-with-the-values-of-x-in-a-vector
    
    


#Satisfaction Data

#Get 311 Survey Data
library(RSocrata)
Survey <- read.socrata("https://data.kcmo.org/311/311-Call-Center-Service-Requests-Survey-Data/hfbv-um74")

#Data Cleaning
Survey$CREATION.DATE <- as.POSIXct(Survey$CREATION.DATE)
Survey$CLOSED.DATE <- as.POSIXct(Survey$CLOSED.DATE)

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
    Percent.Dissatisfied.With.Customer.Service = (Number.Of.Responses.Dissatisfied.With.Customer.Service/Total.Customer.Service.Survey.Resonses)) %>%
  mutate(UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))

#Drop unncessary columns that would otherwise be duplicated after the data frames merge
Drop.Columns <- c("DEPARTMENT", "WORK.GROUP", "CREATION.YEAR", "CREATION.MONTH")
SurveyStats <- SurveyStats[,!names(SurveyStats) %in% Drop.Columns]

#Merge Datasets
Stats <- merge(x = Stats, y = SurveyStats, by = "UniqueID", all.x = TRUE)

write.csv(Stats, file = "311Stats.csv")
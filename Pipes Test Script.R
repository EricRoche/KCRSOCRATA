#P1 includes all data

Stats <- Data.311 %>% 
  filter(STATUS == "RESOL" | STATUS == "OPEN") %>%
  group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
  summarise(
    Total.Number.Of.Cases.Created = length(CASE.ID),
    Number.Of.Cases.Remaining.Open = sum(Number.Of.Open.Cases),
    Number.Of.Cases.Exceeding.Timeframe = sum(EXCEEDED.EST.TIMEFRAME),
    Percent.Of.Cases.Exceeding.Timeframe = (Number.Of.Cases.Exceeding.Timeframe/Total.Number.Of.Cases.Created)*100,
    Mean.Days.To.Close.Of.Closed.Cases = mean(DAYS.TO.CLOSE, na.rm=T),
    Median.Days.To.Close.Closed.Cases = (median(DAYS.TO.CLOSE, na.rm =T)*1),
    Standard.Deviation = sd(DAYS.TO.CLOSE, na.rm=TRUE),
    Above.One.Standard.Deviation = (Standard.Deviation + Mean.Days.To.Close.Of.Closed.Cases),
    Above.Two.Standard.Deviations = ((Standard.Deviation*2) + Mean.Days.To.Close.Of.Closed.Cases),
    Above.Three.Standard.Deviations = ((Standard.Deviation*3) + Mean.Days.To.Close.Of.Closed.Cases),
    Below.One.Standard.Deviation = (Mean.Days.To.Close.Of.Closed.Cases - Standard.Deviation),
    Below.Two.Standard.Deviations = (Mean.Days.To.Close.Of.Closed.Cases - (Standard.Deviation*2)),
    Below.Three.Standard.Deviations = (Mean.Days.To.Close.Of.Closed.Cases - (Standard.Deviation*3))) %>%
  mutate(UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))
  

#Had an issue where I needed to map the data out by timeline and the function needed the last day of the month to properly space things
#This code generates a dataframe of the last days of the month
Date.End.Month <- seq(as.Date("2005-02-01"),length=240,by="months")-1
Date.End.Month <- as.data.frame(Date.End.Month)
Date.End.Month <- format(Date.End.Month$Date.End.Month, "%Y/%m/%d")
Date.End.Month <- as.data.frame(Date.End.Month)
Date.End.Month <- mutate(Date.End.Month, Month.Year =  paste((year(Date.End.Month)), month(Date.End.Month),sep = "."))
Stats <- mutate(Stats , Month.Year =  paste(CREATION.YEAR, CREATION.MONTH, sep = "."))
Stats <- full_join(Stats, Date.End.Month, by = "Month.Year" )
Stats$Date.End.Month <- as.POSIXct(Stats$Date.End.Month)
Stats <- filter(Stats, !is.na(DEPARTMENT))

#Add in Fiscal Year
FiscalYears <- seq(as.POSIXct("2000-05-01"), length=35, by="year")
Stats$Creation.Fiscal.Year <- (2001:2025)[ findInterval(Stats$Date.End.Month, FiscalYears)]

#Overwrite negative standard deviations with zeroes because it doesn't make sense in this context.
Stats$Below.Three.Standard.Deviations[Stats$Below.Three.Standard.Deviations < 0] <- 0
Stats$Below.Two.Standard.Deviations[Stats$Below.Two.Standard.Deviations < 0] <- 0
Stats$Below.One.Standard.Deviation[Stats$Below.One.Standard.Deviation < 0] <- 0

#Arrange variables in a better order
Stats <- Stats[,c(
  "DEPARTMENT",
  "WORK.GROUP",
  "Creation.Fiscal.Year",
  "CREATION.YEAR",
  "CREATION.MONTH",
  "Month.Year",
  "Date.End.Month",
  "Total.Number.Of.Cases.Created",
  "Number.Of.Cases.Remaining.Open",
  "Number.Of.Cases.Exceeding.Timeframe",
  "Percent.Of.Cases.Exceeding.Timeframe",
  "Mean.Days.To.Close.Of.Closed.Cases",
  "Median.Days.To.Close.Closed.Cases",
  "Standard.Deviation",
  "Above.One.Standard.Deviation",
  "Above.Two.Standard.Deviations",
  "Above.Three.Standard.Deviations",
  "Below.One.Standard.Deviation",
  "Below.Two.Standard.Deviations",
  "Below.Three.Standard.Deviations",
  "UniqueID"
)]

write.csv(Stats, file = "311Stats.csv")



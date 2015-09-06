#Testing a giant output using dplyr
library(RSocrata)
library(dplyr)
library(lubridate)
library(scales)

token <- XXXXXXX
Data.311 <- read.socrata("https://data.kcmo.org/311/KCMOPS311-Data/7at3-sxhp")

#Inspect the data
nrow(Data.311)
Work.Groups <- names(table(Data.311$WORK.GROUP))

#Clean Data
Data.311$CREATION.DATE <- as.POSIXct(Data.311$CREATION.DATE)
Data.311$CLOSED.DATE <- as.POSIXct(Data.311$CLOSED.DATE)
Data.311$EXCEEDED.EST.TIMEFRAME[Data.311$EXCEEDED.EST.TIMEFRAME=="Y"] <- 1
Data.311$EXCEEDED.EST.TIMEFRAME[Data.311$EXCEEDED.EST.TIMEFRAME=="N"] <- 0
Data.311$EXCEEDED.EST.TIMEFRAME <- as.numeric(Data.311$EXCEEDED.EST.TIMEFRAME)
Data.311 <- mutate(Data.311, Number.Of.Open.Cases = (STATUS))
Data.311$Number.Of.Open.Cases[Data.311$Number.Of.Open.Cases == "OPEN"] <- 1
Data.311$Number.Of.Open.Cases[Data.311$Number.Of.Open.Cases != "1"] <- 0
Data.311$Number.Of.Open.Cases <- as.numeric(Data.311$Number.Of.Open.Cases)
Data.311$DAYS.TO.CLOSE <- as.numeric(Data.311$DAYS.TO.CLOSE)

#Filter data to only closed cases


#Grouping

group <- group_by(Data.311, DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH)
stats <- summarise(group,
                   Number.Of.Cases = length(CASE.ID),
                   Number.Of.Open.Cases = sum(Number.Of.Open.Cases),
                   Number.Of.Cases.Exceeding.Timeframe = sum(EXCEEDED.EST.TIMEFRAME),
                   Percent.Of.Cases.Exceeding.Timeframe = (Number.Of.Cases.Exceeding.Timeframe/Number.Of.Cases)*100,
                   Mean.Days.To.Close = mean(DAYS.TO.CLOSE), 
                   Median.Days.To.Close = (median(DAYS.TO.CLOSE)*1),
                   Standard.Deviation = sd(DAYS.TO.CLOSE),
                   Above.One.Standard.Deviation = (Standard.Deviation + Mean.Days.To.Close),
                   Above.Two.Standard.Deviations = ((Standard.Deviation*2) + Mean.Days.To.Close),
                   Above.Three.Standard.Deviations = ((Standard.Deviation*3) + Mean.Days.To.Close),
                   Below.One.Standard.Deviation = (Mean.Days.To.Close - Standard.Deviation),
                   Below.Two.Standard.Deviations = (Mean.Days.To.Close - (Standard.Deviation*2)),
                   Below.Three.Standard.Deviations = (Mean.Days.To.Close - (Standard.Deviation*3))
)

write.csv(stats, file = "Stats.csv")
write.csv(Data.311, file = "Data.311.csv")


#Getting lots of NA's in the stats section due to not filtering for cases closed. 
#Use Dplyr to filter down to only closed cases. Be sure to save filtered data to some other variable so that we can still pull in the number of open cases via a table merge.
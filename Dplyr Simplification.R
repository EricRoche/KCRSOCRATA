#Testing a giant output using dplyr
require(RSocrata)
require(dplyr)
require(lubridate)
library(scales)

token <- XXXXXXX
Data.311 <- read.socrata("https://data.kcmo.org/311/KCMOPS311-Data/7at3-sxhp")

#Inspect the data
nrow(Data.311)
Work.Groups <- names(table(Data.311$WORK.GROUP))

#Clean Data
Data.311$CREATION.DATE <- as.POSIXct(Data.311$CREATION.DATE)
Data.311$CLOSED.DATE <- as.POSIXct(Data.311$CLOSED.DATE)

#Grouping

group <- group_by(Data.311, WORK.GROUP, CREATION.YEAR, CREATION.MONTH)
stats <- summarise(group,
           Number.Of.Cases = length(CASE.ID),
           Mean.Days.To.Close = mean(DAYS.TO.CLOSE), 
           Median.Days.To.Close = median(DAYS.TO.CLOSE),
           Standard.Deviation = sd(DAYS.TO.CLOSE),
           Above.One.Standard.Deviation = (Standard.Deviation + Mean.Days.To.Close),
           Above.Two.Standard.Deviations = ((Standard.Deviation*2) + Mean.Days.To.Close),
           Above.Three.Standard.Deviations = ((Standard.Deviation*3) + Mean.Days.To.Close),
           Below.One.Standard.Deviation = (Mean.Days.To.Close - Standard.Deviation),
           Below.Two.Standard.Deviations = (Mean.Days.To.Close - (Standard.Deviation*2)),
           Below.Three.Standard.Deviations = (Mean.Days.To.Close - (Standard.Deviation*3))
           )
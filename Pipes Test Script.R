#This script runs all of the summary statistics by group


StatsPart1 <- Data.311 %>% 
                filter(STATUS == "RESOL" | STATUS == "OPEN") %>%
                  group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
                    summarise(
                      Number.Of.Cases = length(CASE.ID),
                      Number.Of.Open.Cases = sum(Number.Of.Open.Cases),
                      Number.Of.Cases.Exceeding.Timeframe = sum(EXCEEDED.EST.TIMEFRAME),
                      Percent.Of.Cases.Exceeding.Timeframe = (Number.Of.Cases.Exceeding.Timeframe/Number.Of.Cases)*100
                      )
                      
#P2 only calculated on closed data
StatsPart2 <- Data.311 %>% 
                filter(STATUS == "RESOL" | STATUS == "OPEN" & length(CASE.ID) > 1) %>%
                  group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
                    summarise(
                      Mean.Days.To.Close.Of.Closed.Cases = mean(DAYS.TO.CLOSE, na.rm=TRUE),
                      Median.Days.To.Close.Closed.Cases = (median(DAYS.TO.CLOSE, na.rm=TRUE)*1),
                      Mean.Days.Open.For.Open.And.Closed.Cases = mean(Days.Open),
                      Median.Days.Open.For.Open.And.Closed.Cases = (median(Days.Open)*1),
                      Standard.Deviation.of.Days.Open = sd(Days.Open, na.rm=TRUE),
                      Above.One.Standard.Deviation = (Standard.Deviation.of.Days.Open + Mean.Days.Open.For.Open.And.Closed.Cases),
                      Above.Two.Standard.Deviations = ((Standard.Deviation.of.Days.Open*2) + Mean.Days.Open.For.Open.And.Closed.Cases),
                      Above.Three.Standard.Deviations = ((Standard.Deviation.of.Days.Open*3) + Mean.Days.Open.For.Open.And.Closed.Cases),
                      Below.One.Standard.Deviation = (Mean.Days.Open.For.Open.And.Closed.Cases - Standard.Deviation.of.Days.Open),
                      Below.Two.Standard.Deviations = (Mean.Days.Open.For.Open.And.Closed.Cases - (Standard.Deviation.of.Days.Open*2)),
                      Below.Three.Standard.Deviations = (Mean.Days.Open.For.Open.And.Closed.Cases - (Standard.Deviation.of.Days.Open*3)))

#Change all Standard deviations that are less than zero to zero so that they graph correctly. 
#Negative SD's on days to close doesn't make sense and just messes up graphing.
StatsPart2$Below.One.Standard.Deviation[StatsPart2$Below.One.Standard.Deviation <0] <- 0
StatsPart2$Below.Two.Standard.Deviations[StatsPart2$Below.Two.Standard.Deviations <0] <- 0
StatsPart2$Below.Three.Standard.Deviations[StatsPart2$Below.Three.Standard.Deviations <0] <- 0
                
#Join the tables together
#Add a unique identified that is shared between both datasets.

StatsPart1 <- mutate(StatsPart1, UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))
StatsPart2 <- mutate(StatsPart2, UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))

#Drops duplicate columns but retains unique ID then combines the two data frames. 
StatsPart2 <- subset(StatsPart2, select = -c(DEPARTMENT:CREATION.MONTH))
StatsFull <- full_join(StatsPart1, StatsPart2, by = "UniqueID")

#Write to CSV
write.csv(StatsFull, file = "311Stats.CSV")





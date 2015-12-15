#P1 includes all data


PipesTestP1 <- Data.311 %>% 
                filter(STATUS == "RESOL" | STATUS == "OPEN") %>%
                  group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
                    summarise(
                      Number.Of.Cases = length(CASE.ID),
                      Number.Of.Open.Cases = sum(Number.Of.Open.Cases),
                      Number.Of.Cases.Exceeding.Timeframe = sum(EXCEEDED.EST.TIMEFRAME),
                      Percent.Of.Cases.Exceeding.Timeframe = (Number.Of.Cases.Exceeding.Timeframe/Number.Of.Cases)*100
                      )
                      
#P2 only calculated on closed data
PipesTestP2 <- Data.311 %>% 
                filter(STATUS == "RESOL" | STATUS == "OPEN" & length(CASE.ID) > 1) %>%
                  group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
                    summarise(
                      Mean.Days.To.Close.Of.Closed.Cases = mean(DAYS.TO.CLOSE),
                      Median.Days.To.Close.Closed.Cases = (median(DAYS.TO.CLOSE)*1),
                      Standard.Deviation = sd(DAYS.TO.CLOSE, na.rm=TRUE),
                      Above.One.Standard.Deviation = (Standard.Deviation + Mean.Days.To.Close.Of.Closed.Cases),
                      Above.Two.Standard.Deviations = ((Standard.Deviation*2) + Mean.Days.To.Close.Of.Closed.Cases),
                      Above.Three.Standard.Deviations = ((Standard.Deviation*3) + Mean.Days.To.Close.Of.Closed.Cases),
                      Below.One.Standard.Deviation = (Mean.Days.To.Close.Of.Closed.Cases - Standard.Deviation),
                      Below.Two.Standard.Deviations = (Mean.Days.To.Close.Of.Closed.Cases - (Standard.Deviation*2)),
                      Below.Three.Standard.Deviations = (Mean.Days.To.Close.Of.Closed.Cases - (Standard.Deviation*3)))
                
#Join the tables together
#Add a unique identified that is shared between both datasets.

PipesTestP1 <- mutate(PipesTestP1, UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))
PipesTestP2 <- mutate(PipesTestP2, UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))

#Drops duplicate columns but retains unique ID then combines the two data frames. 
PipesTestP2 <- subset(PipesTestP2, select = -c(DEPARTMENT:CREATION.MONTH))
PipedTestAllData <- full_join(PipesTestP1, PipesTestP2, by = "UniqueID")

#Had an issue where I needed to map the data out by timeline and the funciton needed the last day of the month to properly space things
#This code generates a dataframe of the last days of the month
Date.End.Month <- seq(as.Date("2005-02-01"),length=240,by="months")-1
Date.End.Month <- as.data.frame(Date.End.Month)
Date.End.Month <- format(Date.End.Month$Date.End.Month, "%Y/%m/%d")
Date.End.Month <- as.data.frame(Date.End.Month)
Date.End.Month <- mutate(Date.End.Month, Month.Year =  paste((year(Date.End.Month)), month(Date.End.Month),sep = "."))
PipedTestAllData <- mutate(PipedTestAllData , Month.Year =  paste(CREATION.YEAR, CREATION.MONTH, sep = "."))
PipedTestAllData <- full_join(PipedTestAllData, Date.End.Month, by = "Month.Year" )
PipedTestAllData$Date.End.Month <- as.POSIXct(PipedTestAllData$Date.End.Month)

#Add in Fiscal Year
FiscalYears <- seq(as.POSIXct("2000-05-01"), length=35, by="year")
PipedTestAllData$Fiscal.Year <- (2001:2025)[ findInterval(PipedTestAllData$Date.End.Month, FiscalYears)]

write.csv(PipedTestAllData, file = "311Stats.csv")


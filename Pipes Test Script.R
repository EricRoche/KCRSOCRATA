#P1 includes all data


PipesTestP1 <- Data.311 %>% 
                group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
                  summarise(
                      Number.Of.Cases = length(CASE.ID),
                      Number.Of.Open.Cases = sum(Number.Of.Open.Cases),
                      Number.Of.Cases.Exceeding.Timeframe = sum(EXCEEDED.EST.TIMEFRAME),
                      Percent.Of.Cases.Exceeding.Timeframe = (Number.Of.Cases.Exceeding.Timeframe/Number.Of.Cases)*100)

#P2 only calculated on closed data
PipesTestP2 <- Data.311 %>% 
                group_by(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH) %>%
                  filter(STATUS == "RESOL" & length(CASE.ID) > 1) %>%
                    summarise(
                      Mean.Days.To.Close = mean(DAYS.TO.CLOSE),
                      Median.Days.To.Close = (median(DAYS.TO.CLOSE)*1),
                      Standard.Deviation = sd(DAYS.TO.CLOSE, na.rm=TRUE),
                      Above.One.Standard.Deviation = (Standard.Deviation + Mean.Days.To.Close),
                      Above.Two.Standard.Deviations = ((Standard.Deviation*2) + Mean.Days.To.Close),
                      Above.Three.Standard.Deviations = ((Standard.Deviation*3) + Mean.Days.To.Close),
                      Below.One.Standard.Deviation = (Mean.Days.To.Close - Standard.Deviation),
                      Below.Two.Standard.Deviations = (Mean.Days.To.Close - (Standard.Deviation*2)),
                      Below.Three.Standard.Deviations = (Mean.Days.To.Close - (Standard.Deviation*3)))
                
#Join the tables together
#Add a unique identified that is shared between both datasets.

PipesTestP1 <- mutate(PipesTestP1, UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))
PipesTestP2 <- mutate(PipesTestP1, UniqueID = paste(DEPARTMENT, WORK.GROUP, CREATION.YEAR, CREATION.MONTH, sep = " "))
PipedTestAllData <- left_join(PipesTestP1, PipesTestP2, by = "UniqueID")


#Remove rows which are now duplicative. 
#Testing a giant output using dplyr
library(RSocrata)
library(dplyr)
library(lubridate)

#Read in 311 data from Open Data. This uses the RSocrata package.
Data.311 <- read.socrata("https://data.kcmo.org/311/KCMOPS311-Data/7at3-sxhp")

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
Current.Date <- as.POSIXct(Sys.Date())




#Filters 311 Data down to only Open and Closed cases
Data.311 <- filter(Data.311, STATUS == "OPEN" | STATUS == "RESOL")

#Add in a column for Days to Close for Open and closed cases
Open.Cases.Days.Open <- Data.311%>%
              filter(STATUS == "OPEN") %>%
                select(CASE.ID, CREATION.DATE) %>%
                  mutate(Days.Open.For.Open.Cases.Only = (Current.Date - CREATION.DATE)) %>%
                    select(CASE.ID, Days.Open.For.Open.Cases.Only)

#Subracting PosixCT results in the number of seconds, this converts it to days.and then rounds off the decimals
Open.Cases.Days.Open$Days.Open.For.Open.Cases.Only <- as.numeric(Open.Cases.Days.Open$Days.Open.For.Open.Cases.Only, units="days")
Open.Cases.Days.Open$Days.Open.For.Open.Cases.Only <- round(Open.Cases.Days.Open$Days.Open.For.Open.Cases.Only, digits = 0)

#Merges the two dataframes                      
Data.311 <- merge(x = Data.311, y = Open.Cases.Days.Open, by = "CASE.ID", all.x = TRUE)

#Generates a column with "days open" for closed and open cases" If a case is closed it uses that date, if it is open it will pull the Days.Open.For.Open.Cases.Only date
Data.311$Days.Open = rowSums(cbind(Data.311$Days.Open.For.Open.Cases.Only, Data.311$DAYS.TO.CLOSE), na.rm=TRUE)






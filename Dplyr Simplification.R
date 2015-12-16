#Testing a giant output using dplyr
library(RSocrata)
library(dplyr)
library(lubridate)


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
Current.Date <- as.POSIXct(Sys.Date())
Current.Date <- round(Current.Date, "days")


#Add in a column for Days to Close for Open and closed cases
Data.311 <- filter(Data.311, STATUS == "OPEN" | STATUS == "RESOL")



Data.312 <- Data.311%>%
              filter(STATUS == "OPEN") %>%
                select(CASE.ID, CREATION.DATE) %>%
                  mutate(Days.Open.For.Open.Cases.Only = as.numeric(Current.Date - CREATION.DATE)) %>%
                    select(CASE.ID, Days.Open.For.Open.Cases.Only)
                      
Data.311 <- merge(x = Data.311, y = Data.312, by = "CASE.ID", all.x = TRUE)
#Add two days to close columns together columns together
#Generates a column with "days open" for closed and open cases"
Data.311$Days.Open = rowSums(cbind(Data.311$Days.Open.For.Open.Cases.Only, Data.311$DAYS.TO.CLOSE), na.rm=TRUE)






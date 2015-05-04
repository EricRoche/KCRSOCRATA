require(RSocrata)
require(plyr)
require(dplyr)


#Load 311 Data
token <- "JJ_K1U6j1aV1y-3L4BQEDk6gPmdsqYf_evZg"
Data.311 <- read.socrata("https://data.kcmo.org/311/KCMOPS311-Data/7at3-sxhp")

  #Inspect the data
  nrow(Data.311)
  Work.Groups <- names(table(Data.311$WORK.GROUP))
  
  #Clean Data
  Data.311$CREATION.DATE <- as.POSIXct(Data.311$CREATION.DATE)
  Data.311$CLOSED.DATE <- as.POSIXct(Data.311$CLOSED.DATE)
  
###
# 311 Volume by Month for a defined timeframe
###

  #Filter to Work Group you are interested in
  Work.Group.DF <- dplyr::filter(Data.311, WORK.GROUP %in% "Public Works-Street and Traffic-Streetlights")
  
  #Add a column in the "M-Y" format. For rolling up I guess?
  Work.Group.DF$Creation.Month.Year <- strftime(Work.Group.DF$CREATION.DATE, format="%m/%Y")

  Output <- ddply(Work.Group.DF, c("Creation.Month.Year"), summarise,
                 Numbe.Of.Cases    = length(CASE.ID),
                 Mean.Days.To.Close = mean(DAYS.TO.CLOSE, na.rm=TRUE),
                 sd   = sd(DAYS.TO.CLOSE, na.rm=TRUE)
                )
                 
##How do I rollup by dates? Test <- is trying to format my new column as posix but it isn't working right. 
#Can I just use the original full creation date and just rollup by months?


  
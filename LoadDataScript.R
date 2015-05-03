require(RSocrata)
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

#Grouping Data
  Work.Group.DF <- dplyr::filter(Data.311, WORK.GROUP %in% "Public Works-Street and Traffic-Streetlights")
  

require(ggplot2)
p <- ggplot(Work.Group.DF, aes(x=CREATION.YEAR, y=count(CASE.ID), group=CREATION.MONTH))
p + geom_line()




  
require(RSocrata)
require(plyr)
require(dplyr)
require(lubridate)
require(ggplot2)
library(scales)


#Load 311 Data
token <- XXXXXXX
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

###INPUTS###
TargetWG <- "Public Works-Street and Traffic-Streetlights"
By.Day <- "1 day"
By.Month <- "1 month"
By.Year <-  "1 year"
From.Month.Year <-  as.POSIXct("2010-01-01 UTC")
To.Date <- as.POSIXct("2012-01-01 UTC")

#Filter to Work Group you are interested in
Work.Group.DF <- dplyr::filter(Data.311, WORK.GROUP %in% ( TargetWG))

#Add a column in the "M-Y" format. For rolling up I guess?
Work.Group.DF$Creation.Month.Year <- strftime(Work.Group.DF$CREATION.DATE, format="%m%Y")

Output <- ddply(Work.Group.DF, c("Creation.Month.Year"), summarise,
                Number.Of.Cases    = length(CASE.ID),
                Mean.Days.To.Close = mean(DAYS.TO.CLOSE, na.rm=TRUE),
                Standard.Deviation  = sd(DAYS.TO.CLOSE, na.rm=TRUE), 
                Median.Days.To.Close = median(DAYS.TO.CLOSE, na.rm=TRUE)
)


##How do I rollup by dates? Test <- is trying to format my new column as posix but it isn't working right. 
#Can I just use the original full creation date and just rollup by months?
Output$Creation.Month.Year <-  parse_date_time(Output$Creation.Month.Year, order="my")
Output[order(Output$Creation.Month.Year , decreasing = FALSE ),]

#add in top and bottom sd
require(dplyr)
Output <- mutate(Output, "Upper SD Limit"=  Mean.Days.To.Close + Standard.Deviation) 
Output <- mutate(Output, "Lower SD Limit" = Mean.Days.To.Close - Standard.Deviation)


#Filter Output to timeframe
FilterFunc <- function(x,y){Output[Output$Creation.Month.Year >= x & Output$Creation.Month.Year <= y,]}
Test <- FilterFunc(From.Month.Year,To.Date)  
Test[order(Test$Creation.Month.Year , decreasing = FALSE ),]

#plot
p <- ggplot(Output, aes(x=Creation.Month.Year, y=Number.Of.Cases, group= 1)) 
p2 <- p + geom_line(size = 2) + 
          stat_smooth(
                      se = F,
                      size = 2, 
                      alpha = 1
                      )+
        #Date - Picks this from an earlier value
        scale_x_datetime(labels=date_format("%m-%Y"),
                         breaks = date_breaks(By.Month))+
        #Labels
          labs(title = TargetWG,
              x = "Creation Month - Year",
              y = "Total Number of 311 Cases Created Per Month") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        #Theme
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=18,face=))+
          theme(plot.title = element_text(size=18, face="bold"))
  
print(p2)        


  
          



ggsave(p2, file="sample.jpg", dpi = 600)

## To fix##
#Need to test plotting using the test filter function
#Need to use a way to set "zoom paramaters" on chart
#need to generate a second chart with median days to close + SD's
#Need to implement timeframe selection
#Create timeframe vectors with Fiscal Years
#Need to be able to have standard "from" and "to" fields 

##If you want to simply "zoom" in on a specific part of a plot, look at xlim() and ylim() respectively. 


, 
limits = (From.Month.Year), (To.Date)
        )+
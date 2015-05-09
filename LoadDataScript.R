require(RSocrata)
require(plyr)
require(dplyr)
require(lubridate)
require(ggplot2)


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

#Filter to Work Group you are interested in
TargetWG <- "Public Works-Street and Traffic-Streetlights"
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

#plot
p <- ggplot(Output, aes(x=Creation.Month.Year, y=Number.Of.Cases, group= 1))
p2 <- p + geom_line(size = 2) + 
          stat_smooth(
                      se = F,
                      size = 2, 
                      alpha = 1
                      )+
          labs(title = TargetWG,
              x = "Creation Month - Year"
              y = "Total Number of Cases Per Month")



ggsave(p2, file="sample.jpg", dpi = 600)

## To fix##
#Need to fix labels for chart.
#Need to smooth line on chart
#Need to make labels more attractive
#Need to add title to chart that pulls from the selected workgroup
#need to generate a second chart with median days to close + SD's
#Need to implement timeframe selection
#Create timeframe vectors with Fiscal Years
#Need to be able to have standard "from" and "to" fields 

library(plyr)
library(ggplot2)

path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, file.path(path, "repdata-data-StormData.csv.bz2"))

Storm_Data <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))
names(Storm_Data)

## Find the event that kills the most
fatalityData <- aggregate(FATALITIES ~ EVTYPE, data = Storm_Data, FUN ="sum")
most_fatal <- which.max(fatalityData$FATALITIES)

fatalityData[most_fatal,]

## Find the 10 events that kill the most
fatalityData <- arrange(fatalityData, desc(fatalityData[,2]))
top10fatData <- fatalityData[1:10,]
top10fatData

## Plot the 10 deadliest events

ggplot(top10fatData, aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES)) + 
  geom_bar(stat = "identity") + 
  ggtitle("10 deadliest events") + 
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust =1))



## Find the event that leads to the most injured people
injuriesData <- aggregate(INJURIES ~ EVTYPE, data=Storm_Data, FUN = "sum")
most_injuries <- which.max(injuriesData$INJURIES)

injuriesData[most_injuries,]

## Find the 10 events that lead to the most injured people
injuriesData <- arrange(injuriesData, desc(injuriesData[,2]))
top10injData <- injuriesData[1:10,]
top10injData


## Plot the 10 events that injured the most people
ggplot(top10injData, aes(x = reorder(EVTYPE, -INJURIES), y = INJURIES)) + 
  geom_bar(stat = "identity") + 
  ggtitle("10 events leading to the most injuries") + 
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust =1))


EconomicImpactData <- aggregate(PROPDMG + CROPDMG ~ EVTYPE, data = Storm_Data, FUN=sum)
names(EconomicImpactData) <- c("EVTYPE", "Damages")

EcoImpact <- arrange(EconomicImpactData, desc(EconomicImpactData[, 2]))
top10EcoData <- EcoImpact[1:10,]
top10EcoData

ggplot(top10EcoData, aes(x = reorder(EVTYPE, -Damages), y = Damages)) + 
  geom_bar(stat = "identity") + 
  ggtitle("10 events having the greatest economic damages") + 
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 90))

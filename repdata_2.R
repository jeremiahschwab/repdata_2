plot6 <- function() {
  

  library(reshape2)

  raw_data <- read.csv("repdata-data-StormData.csv.bz2")

  raw_data$BGN_DATE <- as.Date(raw_data$BGN_DATE, "%m/%d/%Y")
  raw_data$YEAR <- format(raw_data$BGN_DATE, "%Y")
  
  processed_data <- raw_data[,c("YEAR","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
  recent_data <- processed_data[processed_data$YEAR > 1999,]
  recent_data <- droplevels(recent_data)
  
  recent_data$PROPDMG <- ifelse(recent_data$PROPDMGEXP == "K",
                                recent_data$PROPDMG * 1000,ifelse(recent_data$PROPDMGEXP == "M",
                                                                  recent_data$PROPDMG * 1000000,ifelse(recent_data$PROPDMGEXP == "B",
                                                                                                       recent_data$PROPDMG * 1000000000,recent_data$PROPDMG)))
  recent_data$CROPDMG <- ifelse(recent_data$CROPDMGEXP == "K",
                                recent_data$CROPDMG * 1000,ifelse(recent_data$CROPDMGEXP == "M",
                                                                  recent_data$CROPDMG * 1000000,ifelse(recent_data$CROPDMGEXP == "B",
                                                                                                       recent_data$CROPDMG * 1000000000,recent_data$CROPDMG)))
  recent_data$ECONOMIC <- recent_data$PROPDMG + recent_data$CROPDMG
  
  melted_data <- melt(recent_data, id="EVTYPE", measure = c("FATALITIES", "INJURIES", "ECONOMIC"))
  casted_data <- dcast(melted_data, EVTYPE ~ variable, sum)
   
  casted_data <- casted_data[order(casted_data$FATALITIES, decreasing = TRUE), ]
  top_fata_data <- casted_data[1:3,]
  top_fata_data <- droplevels(top_fata_data)
  casted_data <- casted_data[order(casted_data$INJURIES, decreasing = TRUE), ]
  top_inju_data <- casted_data[1:3,]
  top_inju_data <- droplevels(top_inju_data)
  casted_data <- casted_data[order(casted_data$ECONOMIC, decreasing = TRUE), ]
  casted_data$ECONOMIC <- casted_data$ECONOMIC / 1000000000
  top_econ_data <- casted_data[1:3,]
  top_econ_data <- droplevels(top_econ_data)
  
  barplot(top_fata_data$FATALITIES,names.arg = top_fata_data$EVTYPE,col = "cyan",xlab = "Event Type", ylab = "Fatalities", main = "Event Types with Most Fatalities Since Year 2000")
  text(c(0.75,1.875,3.125), top_fata_data$FATALITIES-100, top_fata_data$FATALITIES, cex=1.5, col = "black")
  
  barplot(top_inju_data$INJURIES,names.arg = top_inju_data$EVTYPE,col = "cyan",xlab = "Event Type", ylab = "Injuries", main = "Event Types with Most Injuries Since Year 2000")
  text(c(0.75,1.875,3.125), top_inju_data$INJURIES-1000, top_inju_data$INJURIES, cex=1.5, col = "black")
  
  barplot(top_econ_data$ECONOMIC,names.arg = top_econ_data$EVTYPE,col = "cyan",xlab = "Event Type", ylab = "Economic Damage (in Billions of Dollars)", main = "Event Types with Most Economic Damage Since Year 2000")
  text(c(0.75,1.875,3.125), top_econ_data$ECONOMIC-10, round(top_econ_data$ECONOMIC,1), cex=1.5, col = "black")
  
  
}

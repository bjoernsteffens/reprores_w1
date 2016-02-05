

setwd("~/mycloud/Private_DataScience/Coursera/10 Data Science Specialisation/40 Reproducible Research/Assignments/week1")


#
# Load the libraries we are going to use
# and clean out any left-overs in the $env
rm(list=ls())
library(ggplot2)

#
# Read the csv file and transform the date string to a proper date
# and the interval to a factor
steps <- read.csv("activity.csv")
steps$date <- as.Date(steps$date,"%Y-%m-%d")
steps$interval <- as.factor(steps$interval)

#
# Clean out the NAs
steps <- steps[complete.cases(steps),]

#
# Get the number of steps per day and clean up column names
stepsDay <- as.data.frame(tapply(steps$steps,steps$date,sum))
stepsDay$Day <- row.names(stepsDay)
colnames(stepsDay) <- c("Steps","Day")


#
# Plot the historgram of the step groups.
png(filename = "reprores1a.png", width = 640, height = 640)
par(mar=c(10,10,10,10))
hist(stepsDay$Steps, 
     xlab = "Total Number of Steps in Groups
     Example: 5 days with less than 5000 steps", 
     ylab = "Days with Step Ranges", 
     main = "Number of Steps per Day in Groups",
     col = "lightblue")
dev.off()

#
# Plot it on the X axis, dont plot legend and turn the x-labels
png(filename = "reprores1b.png", width = 960, height = 640)
g <- ggplot(stepsDay, aes(x=Day, y=Steps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Number of Recorded Step") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Number of Steps Recorded per Day") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off()   

#
# The mean and median values for the documentation
median(stepsDay$Steps)
mean(stepsDay$Steps)

#
# Add the mean and median values to the data.frame
stepsDay$AvgSteps <- rep(mean(stepsDay$Steps),nrow(stepsDay))
stepsDay$MedSteps <- rep(median(stepsDay$Steps),nrow(stepsDay))

#
# Plot that time series 
png(filename = "reprores1c.png", width = 960, height = 640)
g <- ggplot(stepsDay, aes(x=Day, y=AvgSteps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Steps per Day") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Average Number of Steps") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps per Day") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off() 


#
# 5 Minute interval

#
# Lets grab the average values for the intervals first
# and fix the column names
stepsAvgInt <- as.data.frame(tapply(steps$steps,steps$interval, mean))
stepsAvgInt$Interval <- row.names(stepsAvgInt)
colnames(stepsAvgInt) <- c("Steps","Interval")

#
# Ensure geom_bar does not try and sort the x Axis
stepsAvgInt$Interval <- factor(stepsAvgInt$Interval, levels = stepsAvgInt$Interval)


#
# lets plot that
png(filename = "reprores1d.png", width = 960, height = 640)
g <- ggplot(stepsAvgInt, aes(x=Interval, y=Steps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) + geom_hline(yintercept=100, col = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    #theme(axis.ticks.x = element_blank()) +
    #theme(axis.ticks.length = 12) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Daily Intervals") +
    #
    # ticks only at the full hour
    scale_x_discrete(breaks=seq(0,2355,100)) +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Number of Steps per Interval") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Average Number of Steps per Interval") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off()

#
# visual inspection gives us the time interval between 0800 and 0900 
# in the morning seems to be the most active ones. Can
# we verify this by taking the means of every hours bucket?
# I want twelve buckets and the average of those

#
# No time for that. Lets grab the values between 800 and 855 and plot those
stepsMaxInt <- stepsAvgInt[with(stepsAvgInt, stepsAvgInt$Interval %in% seq(800,855,5)),]

#
# lets plot that
png(filename = "reprores1e.png", width = 960, height = 640)
g <- ggplot(stepsMaxInt, aes(x=Interval, y=Steps, fill = 20))
g + geom_bar(stat = "Identity", alpha = 0.9) + geom_hline(yintercept=100, col = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    #theme(axis.ticks.x = element_blank()) +
    #theme(axis.ticks.length = 12) +
    theme(panel.background = element_rect(fill = "lightblue")) +
    theme(strip.background = element_rect(fill = "lightblue")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey95")) +
    theme(plot.margin=unit(c(2,1,1.5,1.2),"cm")) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position="none") +
    xlab("Between 800 and 855") +
    #
    # ticks only at the full hour
    #scale_x_discrete(breaks=seq(0,2355,100)) +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Number of Steps per Interval") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Steps per Interval between 800 and 855") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off()

#
# Even though there are steps < 100 this is the interval with the highest number of steps


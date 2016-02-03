

setwd("~/mycloud/Private_DataScience/Coursera/10 Data Science Specialisation/40 Reproducible Research/Assignments/week1")


#
# Load the libraries we are going to use
# and clean out any left-overs in the $env
rm(list=ls())
library(ggplot2)

#
# Read the csv file and transform the date string to a proper date
steps <- read.csv("activity.csv")
steps$date <- as.Date(steps$date,"%Y-%m-%d")

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
    xlab("Steps per Day
         Missing values in \"empty\" days") +
    theme(axis.text.x = element_text(size=10,margin = margin(0,0,20,0))) +
    ylab("Number of Recorded Step") + 
    theme(axis.text.y = element_text(size=10,margin = margin(0,0,0,10))) +
    ggtitle("Number of Steps Recorded per Day") +
    theme(plot.title = element_text(size = 20,margin = margin(0,0,30,0)))
dev.off()   
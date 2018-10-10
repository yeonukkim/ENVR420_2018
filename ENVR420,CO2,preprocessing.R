# This script was designed for preprocessing the chamber co2 flux data.
#Clear memory, confirm working directory
rm(list = ls())
ls()
getwd()

# load library
library(tidyverse)
library(chron)

# Load the original data file
system1 <- read.csv("https://raw.githubusercontent.com/yeonukkim/ENVR420_2018/master/ENVR420_18_system1.csv", header = FALSE)
system2 <- read.csv("https://raw.githubusercontent.com/yeonukkim/ENVR420_2018/master/ENVR420_18_system2.csv", header = FALSE)

system1 <- system1[,-c(1,13,14)]
system2 <- system2[,-c(1,13,14)]

# col name 
head <- c("Year","DoY", "HHMM", "SS", 
					"BattVolt.V", "CO2.mV", "CO2.ppm", "CO2.ppm.cal", "system.temp", "RH.pct", "chamber.temp.degC")
colnames(system1) <- head
colnames(system2) <- head

#make datetime col
system1$DoY <- as.numeric(system1$DoY)
system2$DoY <- as.numeric(system2$DoY)
system1$SS <- round(system1$SS)
system2$SS <- round(system2$SS)

Date <- as.Date(system1$DoY, origin = "2018-01-01")
date.time <- strptime(paste0(substr(as.character(system1$HHMM),1,2),":",substr(as.character(system1$HHMM),3,4),":",as.character(system1$SS)), "%H:%M:%S")
(Time <- times(format(date.time, "%H:%M:%S")))

result1 <- cbind(Date,Time,date.time, system1[,-c(1:4)])

Date <- as.Date(system2$DoY, origin = "2018-01-01")
date.time <- strptime(paste0(substr(as.character(system2$HHMM),1,2),":",substr(as.character(system2$HHMM),3,4),":",as.character(system2$SS)), "%H:%M:%S")
(Time <- times(format(date.time, "%H:%M:%S")))

result2 <- cbind(Date,Time,date.time, system2[,-c(1:4)])

# Select start and end points of slope calculations
# Create an index of start times identifying slopes
plot(result1$date.time,result1$CO2.ppm.cal)
slope_start_ind1 <- identify(result1$date.time,result1$CO2.ppm.cal)
slope_end_ind1 <- identify(result1$date.time,result1$CO2.ppm.cal)

plot(result2$date.time,result2$CO2.ppm.cal)
slope_start_ind2 <- identify(result2$date.time,result2$CO2.ppm.cal)
slope_end_ind2 <- identify(result2$date.time,result2$CO2.ppm.cal)

# make location col and remove data don't need.
result1$sample <- c(rep(0,slope_start_ind1[1]-1),
											rep(1,abs(slope_start_ind1[1]-slope_end_ind1[1])),
											rep(0,abs(slope_end_ind1[1]-slope_start_ind1[2])),
											rep(2,abs(slope_start_ind1[2]-slope_end_ind1[2])),
											rep(0,abs(slope_end_ind1[2]-slope_start_ind1[3])),
											rep(3,abs(slope_start_ind1[3]-slope_end_ind1[3])),
											rep(0,abs(slope_end_ind1[3]-slope_start_ind1[4])),
											rep(4,abs(slope_start_ind1[4]-slope_end_ind1[4])),
											rep(0,abs(slope_end_ind1[4]-slope_start_ind1[5])),
											rep(5,abs(slope_start_ind1[5]-slope_end_ind1[5])),
											rep(0,abs(slope_end_ind1[5]-slope_start_ind1[6])),
											rep(6,abs(slope_start_ind1[6]-slope_end_ind1[6])),
											rep(0,abs(slope_end_ind1[6]-nrow(result1))+1)
											)

result2$sample <- c(rep(0,slope_start_ind2[1]-1),
										rep(7,abs(slope_start_ind2[1]-slope_end_ind2[1])),
										rep(0,abs(slope_end_ind2[1]-slope_start_ind2[2])),
										rep(8,abs(slope_start_ind2[2]-slope_end_ind2[2])),
										rep(0,abs(slope_end_ind2[2]-slope_start_ind2[3])),
										rep(9,abs(slope_start_ind2[3]-slope_end_ind2[3])),
										rep(0,abs(slope_end_ind2[3]-slope_start_ind2[4])),
										rep(10,abs(slope_start_ind2[4]-slope_end_ind2[4])),
										rep(0,abs(slope_end_ind2[4]-slope_start_ind2[5])),
										rep(11,abs(slope_start_ind2[5]-slope_end_ind2[5])),
										rep(0,abs(slope_end_ind2[5]-slope_start_ind2[6])),
										rep(12,abs(slope_start_ind2[6]-slope_end_ind2[6])),
										rep(0,abs(slope_end_ind2[6]-nrow(result2))+1)
)

full <- rbind(result1,result2)
full <- full[-which(full$sample == 0),]

plot(full$CO2.ppm.cal)
plot(full$RH.pct)

#sameple location
location <- c("grass.bare","grass.bare","grass.live","grass.live","grass.live","grass.bare",
							"meadow","meadow","meadow","raised.bed","raised.bed","raised.bed")

full$Location <- ifelse(full$sample %in% c(1,2,6),"grass.bare",
												ifelse(full$sample %in% c(3,4,5),"grass.live",
															 ifelse(full$sample %in% c(7,8,9),"meadow","raised.bed"
															 			 )
															 )
												)

write_csv(full,"ENVR_420_18_co2f.csv")




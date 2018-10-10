# This script was designed for interactive use when calculating CO2 fluxes in units of 
# micromoles CO2 per m2 per sec based on ppm CO2 data collected using the 
# UBC Biometeorology soil respiration measurment system

# Step 0: Make sure your .csv dataset is "clean" - use NA for any blank cells, no complex column names

# Step 1: read data from .csv file
# Step 2: pre-process data (approximately lines 27-36 of this file) 
# Step 3: Using the cursor interactively select (press once) the desired start point for each slope, press "esc" or "finish" on plot when complete
#     -- this is in line 40-41    
# Step 4: Calculate fluxes
#     -- Select and run lines 44:90

#confirm working directory
getwd()

# Load package
library(lubridate) #load the package "lubridate" - make sure that it is installed on your computer 
library(tidyverse) #load the package "tidyverse" - make sure that it is installed on your computer

# Load the file containing data
CO2.all <- read.csv("https://raw.githubusercontent.com/yeonukkim/ENVR420_2018/master/soilco2_lab/ENVR_420_18_co2f.csv")

# fix date string
class(CO2.all$Date) #check which class variable "Date" is
CO2.all$Date <- as.Date(CO2.all$Date,"%Y-%m-%d") #convert variable "Date" into class "Date"
class(CO2.all$Date) #check which class variable "Date" is now
?paste # see what the command "paste" does
CO2.all$date.time <- paste(CO2.all$Date,CO2.all$Time) # paste is used to join fields
class(CO2.all$date.time) #check which class variable "date.time" is
?ymd_hms
CO2.all$date.time <- ymd_hms(CO2.all$date.time) 
class(CO2.all$date.time) #check which class variable "date.time" is now

# change location and sampe class to factor
# factor class is useful for grouping the dataset.
class(CO2.all$sample) #this one looks integer
class(CO2.all$Location) # this one looks factor
CO2.all$sample <- as.factor(CO2.all$sample)
summary(CO2.all$sample) 
summary(CO2.all$Location)


# Plot data for initial examination
ggplot(CO2.all, aes(x = date.time, y = CO2.ppm.cal, colour = Location)) + geom_point() # slopes for CO2
ggplot(CO2.all, aes(x = date.time, y = CO2.ppm.cal, colour = sample)) + geom_point() + facet_wrap(~Location)

ggplot(CO2.all, aes(x = date.time, y = RH.pct, colour = Location)) + geom_point() # rh
ggplot(CO2.all, aes(x = date.time, y = chamber.temp.degC, colour = Location)) + geom_point() # chamber

##################### basic linear flux calculation output micromole per m2 per second 
### function to calculate co2 flux (regression method)

co2flux <- function(co2,airT,rH){

	# only use First 50 seconds
	CO2_tmp <- co2[1:50]
	Air_T_tmp <- mean(airT[1:50]);
	H2O_tmp <- rH[1:50] # relative humidity (%)
	
	
	# define parameters
	Pbar_tmp <- 101;  # kPa
	Pbar <- Pbar_tmp * 1000; # Pa
	Vol  <- 0.0014;  # volume of chamber (in m3)
	Sur  <- 0.0079;  # surface area of chamber (in m2)
	R <- 8.3144; # Universal Gas Constant in J mol-1 K-1 (or J Kg-1 K-1)
	
	
	# convert to mixing ratios
	H2O_p <- H2O_tmp*(0.61365*exp((17.502*Air_T_tmp)/(240.97+Air_T_tmp)))/100 #kPa
	H2O_mole_fraction <- H2O_p/Pbar_tmp                       #mol/mol wet air
	H2O_mix_ratio <- H2O_mole_fraction/(1-H2O_mole_fraction) #mol H2O/mol dry air
	
	CO2_mix_ratio <- CO2_tmp/(1-H2O_mole_fraction) 
	
	
	#creating x to be time in seconds
	time_s <- 1:length(CO2_tmp)
	
	#fit linear midel
	lm.r <- lm(CO2_mix_ratio ~ time_s)
	
	#slope of linear model fit
	dcdt_tmp <- lm.r$coefficients[2]
	
	rho <- Pbar/ (R * (Air_T_tmp + 273.15));  # mol/m3 ( P is in Pa)
	
	flux <- rho * Vol / Sur * dcdt_tmp/(1 + mean(H2O_mix_ratio));
	
	return(flux)
}


# apply the fuction to the dataset
#soil.flux is the soil respiration in micromol CO2 per square meter per second
result <- CO2.all %>% 
							group_by(sample, Location) %>%
							summarise(soil.flux = co2flux(CO2.ppm.cal,chamber.temp.degC,RH.pct))

result
boxplot(soil.flux ~ Location, data = result)



# For comparing the four treatments, be sure to use ANOVA and Tukey HSD as we did in the first lab
# If you want to do a t-test, first need to do a subset to create a data.frame with only two treatments


### Assignment:
# Prepare a brief report describing the experiment and results determined from the data collected.
# Format: 
# 1. Introduction
# 2. Materials and Methods
# 3. Results and Discussion (include mean +/- SD when reporting results, include p-values when making comparisons)
# 4. Conclusions

# Things to think about: The data aren't perfect - what do you think could be sources of error in the measurements, and which 
# of these are likely observed in the data (e.g. looking at the slopes plotted)

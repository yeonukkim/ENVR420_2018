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

#Clear memory, confirm working directory
rm(list = ls())
ls()
getwd()

# Load the file containing data
CO2.all <- read.csv("https://raw.githubusercontent.com/yeonukkim/ENVR420_2018/master/ENVR_420_18_co2f.csv")

# fix date string
library(lubridate) #load the package "lubridate" - make sure that it is installed on your computer 
class(CO2.all$Date) #check which class variable "Date" is
CO2.all$Date <- as.Date(CO2.all$Date,"%Y-%m-%d") #convert variable "Date" into class "Date"
class(CO2.all$Date) #check which class variable "Date" is now
?paste # see what the command "paste" does
CO2.all$date.time <- paste(CO2.all$Date,CO2.all$Time) # paste is used to join fields
class(CO2.all$date.time) #check which class variable "date.time" is
?ymd_hms
CO2.all$date.time <- ymd_hms(CO2.all$date.time) 
class(CO2.all$date.time) #check which class variable "date.time" is now

# Plot data for initial examination
plot(CO2.all$date.time,CO2.all$CO2.ppm.cal) # slopes for CO2
plot(CO2.all$date.time,CO2.all$RH.pct)      # slopes of relative humidity

# Select start point of slope calculations, and create index for loop
# Create an index of start times identifying slopes
plot(CO2.all$date.time,CO2.all$CO2.ppm.cal)
slope_start_ind <- identify(CO2.all$date.time,CO2.all$CO2.ppm.cal) # identify points manually then press finish on plot

# ** You have to press ESC to end the slope selection process **

##################### basic linear flux calculation output micromole per m2 per second 
Pbar_tmp <- 101;  # kPa
Pbar <- Pbar_tmp * 1000; # Pa

Vol  <- 0.0014;  # volume of chamber (in m3)
Sur  <- 0.0079;  # surface area of chamber (in m2)
R <- 8.3144; # Universal Gas Constant in J mol-1 K-1 (or J Kg-1 K-1)

# calculate fluxes (regression method)
nbr_sample <- length(slope_start_ind)

for (i in 1:nbr_sample){
	#############################################################################start of loop
	ind <- slope_start_ind[i]  # ind = individual slope
	ind2 <- ind+50
	CO2_tmp <- CO2.all$CO2.ppm.cal[ind:ind2]
	Air_T_tmp <- 23;
	H2O_tmp <- CO2.all$RH.pct[ind:ind2]  # relative humidity (%)
	
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
	
	F[i] <- rho * Vol / Sur * dcdt_tmp/(1 + mean(H2O_mix_ratio));
	
}
#################################################################################End of loop

plot(F) #this just displays the calculated fluxs 

CO2.exp <- as.data.frame(F)
CO2.exp$Reading.number <- c(1:9) # assign the "Reading number" to the data.frame
CO2.exp$Treatment <- c(rep("green.grass",3),rep("brown.grass",3),rep("meadow",3)) #populate the variable "Treatment"
CO2.exp <- CO2.exp[,c(2,1,3)] #rearrange data.frame columns
names(CO2.exp) <- c("Reading.number","soil.flux","Treatment")
#soil.flux is the soil respiration in micromol CO2 per square meter per second

CO2.exp
boxplot(soil.flux ~ Treatment, data = CO2.exp)

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

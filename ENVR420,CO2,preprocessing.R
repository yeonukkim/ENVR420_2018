# This script was designed for preprocessing the chamber co2 flux data.
#Clear memory, confirm working directory
rm(list = ls())
ls()
getwd()

# Load the original data file
system1 <- read.csv("https://github.com/yeonukkim/ENVR420_2018/blob/master/ENVR420_18_system1.DAT")

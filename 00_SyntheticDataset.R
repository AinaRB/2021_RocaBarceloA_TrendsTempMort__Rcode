#####################################################################################################
# #      TRENDS IN TEMPERATURE-MORTALITY ASSOCIATION IN SAO PAOLO 2000-2018           
# #                              by Aina Roca-Barcelo                       
#####################################################################################################
# AUTHOR: Aina Roca Barcelo                                                                         
# CREATED: 17.11.2020                                                                               
# LAST MODIFIED: 17.11.2020                                                                         
# R Version 3.6.3.                                                                                  

# PAPER #############################################################################################
# Latest version of R code for the analysis in:                                                     
#
# [ADD PAPER + LINK]
#####################################################################################################

# METADATA ##########################################################################################                                                                                         
# This code creates a demo datset based on a modification of the chicagoNMMAPS dataset available 
# within the DLNM package. It only includes 3 outcomes (total, cvd, resp).To have an overview 
# of the complete target dataset structure, see file "DataFormat.xls"
# 
# REFERENCE: Roger Peng & Leah Welty & Aidan McDermott, 2004. "The National Morbidity, Mortality, and 
# Air Pollution Study Database in R," Johns Hopkins University Dept. of Biostatistics Working Paper 
# Series 1044, Berkeley Electronic Press.
#####################################################################################################

#####################################################################################################
# IMPORTANT NOTE!
# * This dataset is an altered version of NMMAPS and DOES NOT represent the reality of that data. 
# * It does NOT include data for Sao Paulo, and so, the results WILL NOT coincide with those in the publication
# 
# -> THIS IS AN ILLUSTRATIVE DATASET, DO NOT TAKE CONCLUSIONS FROM THE OBTAINED RESULTS <-
# 
####################################################################################################

rm(list=ls())
.libPaths("C:/icnas1.cc.ic.ac.uk/arocabar/R/win-library/3.6")
getwd()

#LOAD PACKAGES
library(readxl); library(ggplot2);library(plyr); library(dplyr);library(rio); library(purrr);library(RColorBrewer);library(plyr); library(purrr)
library(dlnm); library(utils); library(tidyr)

# IMPORT DATASET 
holidays<-read.csv("holidays.csv"); holidays$date<-as.Date(holidays$date)
chicagoNMMAPS<-subset(chicagoNMMAPS, select=c(date,temp, rhum, cvd, death, resp, dow, pm10))
chicagoNMMAPS %>% group_by(date,temp, rhum, cvd, death, resp, dow, pm10) %>% expand(ID = 1:2) %>%
  arrange(ID) %>% slice_head(n=6940)->chicagoNMMAPS

# CREATE DATASET WITH ESSENTIAL VARIABLES 
simDF<-data.frame(date=seq(as.Date("2000-01-01"), as.Date("2018-12-31"), by="day"), temp_mean=chicagoNMMAPS$temp, 
                      HR_mean=chicagoNMMAPS$rhum, nonext=chicagoNMMAPS$death, 
                      cvd=chicagoNMMAPS$cvd, resp=chicagoNMMAPS$resp, dow=chicagoNMMAPS$dow, 
                      pm10_mean.mean=chicagoNMMAPS$pm10)
# ADD HOLIDAYS VARIABLE
simDF<-merge(simDF, holidays, by=c("date"), all=T); 
names(simDF)[names(simDF) == "name"]<-"holidays"
simDF$holidays<-as.character(simDF$holidays)
simDF$holidays[!is.na(simDF$holidays)]<-1;simDF$holidays[is.na(simDF$holidays)]<-0

# SAVE FILE
write.csv(simDF, "S:/Projects/AinaRB_ClimateHealth/Part1_Temporal/PhD_Git_repo/papers/adaptation/scripts/GitHub_Public/simulatedDF.csv")



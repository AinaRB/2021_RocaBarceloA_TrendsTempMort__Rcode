#####################################################################################################
# #      Trends in Temperature-associated Mortality in São Paulo (Brazil) between 2000 and 2018: 
# #             an Example of Disparities in Adaptation to Cold and Heat          
# #                              by Aina Roca-Barcelo                       
#####################################################################################################
# AUTHOR: Aina Roca Barcelo                                                                         
# CREATED: 17.11.2021                                                                               
# LAST MODIFIED: 27.10.2022                                                                         
# R Version 3.6.3.                                                                                  

# PAPER #############################################################################################
# Latest version of R code for the analyses in paper:                                                   
#
# Roca-Barceló, A., Fecht, D., Pirani, M. et al. Trends in Temperature-associated Mortality in
# São Paulo (Brazil) between 2000 and 2018: an Example of Disparities in Adaptation to Cold and Heat. 
# J Urban Health (2022). https://doi.org/10.1007/s11524-022-00695-7
#####################################################################################################

# METADATA ##########################################################################################                                                                                         
# This code creates a synthetic datset to illustrate the use of the code. It is based on a 
# modification of the chicagoNMMAPS dataset available within the DLNM package. It only includes
# 3 outcomes (i.e., total, cvd, resp).
#
# For an overview of the dataset structure required to run the code, see file "DataFormat.xls"
# 
# Dataset reference: Roger Peng & Leah Welty & Aidan McDermott, 2004. "The National Morbidity, Mortality, and 
# Air Pollution Study Database in R," Johns Hopkins University Dept. of Biostatistics Working Paper 
# Series 1044, Berkeley Electronic Press.
#####################################################################################################

#####################################################################################################
# IMPORTANT NOTE!
# * This dataset is an altered version of chicagoNMMAPS and does NOT represent the reality of that data. 
# * It does NOT include data for Sao Paulo, and so, the results WILL NOT coincide with those in the publication
# 
# -> THIS IS AN ILLUSTRATIVE DATASET, DO NOT TAKE CONCLUSIONS FROM THE OBTAINED RESULTS <-
# 
####################################################################################################


#DEFINE ENVIRONMENT
rm(list=ls())
.libPaths("directory/for/your/rversion/library") #modfiy with the name of your Rversion lib directory
mainDir<-getwd()
subDir<-'data'
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)

#LOAD PACKAGES
##Use install.packages("namepackage") if not installed already
library(readxl); library(ggplot2);library(plyr); library(dplyr);library(rio); library(purrr);library(RColorBrewer);library(plyr); library(purrr)
library(dlnm); library(utils); library(tidyr); library(lubridate)

#IMPORT DATASET & SELECT VAR OF INTEREST
data(chicagoNMMAPS)
chicagoNMMAPS<-as.data.frame(subset(chicagoNMMAPS, select=c(date,temp, rhum, cvd, death, resp, dow, pm10)))
dim(chicagoNMMAPS) #5114 x 8

#EXPAND DATASET (target size: 6940 observations -> every year between 2000-2018)
as.data.frame(chicagoNMMAPS %>% group_by(date,temp, rhum, cvd, death, resp, dow, pm10) %>% expand(ID = 1:2) %>%
  arrange(ID) )%>% slice_head(n=6940) -> chicagoNMMAPS_sim
dim(chicagoNMMAPS_sim) #6940x9

# CREATE DATASET WITH ESSENTIAL VARIABLES 
simDF<-data.frame(date=seq(as.Date("2000-01-01"), as.Date("2018-12-31"), by="day"), temp_mean=chicagoNMMAPS_sim$temp, 
                      HR_mean=chicagoNMMAPS_sim$rhum, total=chicagoNMMAPS_sim$death, 
                      cvd=chicagoNMMAPS_sim$cvd, resp=chicagoNMMAPS_sim$resp, dow=chicagoNMMAPS_sim$dow,
                      pm10_mean.mean=chicagoNMMAPS_sim$pm10)
head(simDF)
class(simDF)#dataframe

# ADD HOLIDAYS and YEAR VARIABLE (this file will vary for each country)
holidays<-read.csv("S:/Projects/AinaRB_ClimateHealth/Part1_Temporal/PhD_Git_repo/papers/adaptation/scripts/GitHub_Public_Resubmission/holidays.csv"); holidays$date<-as.Date(holidays$date)
#holidays<-read.csv("data/holidays.csv"); holidays$date<-as.Date(holidays$date)
simDF<-merge(simDF, holidays, by=c("date"), all=T); 
names(simDF)[names(simDF) == "name"]<-"holidays"
simDF$holidays<-as.character(simDF$holidays)
simDF$holidays[!is.na(simDF$holidays)]<-1;simDF$holidays[is.na(simDF$holidays)]<-0
simDF$year<-year(simDF$date)

# CREATE OUTCOME VARIABLES 
## To create the synthetic variabels we will apply some hyothetic population group proportions 
## i.e., males:0.45/females:0.55/RETI_65-79:0.15/RETI_80:0.05/whites:0.85/colored:0.12 and their combinations
## ATTENTION: these are selected randomly and do NOT represent the population distribution of any country/city.
simDF$males<-round(simDF$total*0.45)
simDF$females<-round(simDF$total*0.55)
simDF$RETI_65.79<-round(simDF$total*0.15)
simDF$RETII_.80<-round(simDF$total*0.05)
simDF$females_RETI_65.79<-round(simDF$total*0.15*0.55)
simDF$males_RETI_65.79<-round(simDF$total*0.15*0.45)
simDF$females_RETII_.80<-round(simDF$total*0.05*0.55)
simDF$males_RETII_.80<-round(simDF$total*0.05*0.45)
simDF$white<-round(simDF$total*0.85)
simDF$colored<-round(simDF$total*0.12)
simDF$males_white<-round(simDF$total*0.85*0.45)
simDF$males_colored<-round(simDF$total*0.12*0.45)
simDF$females_white<-round(simDF$total*0.85*0.55)
simDF$females_colored<-round(simDF$total*0.12*0.55)
simDF$RETI_65.79_white<-round(simDF$total*0.15*0.85)
simDF$RETII_.80_white<-round(simDF$total*0.05*0.85)
simDF$RETI_65.79_colored<-round(simDF$total*0.15*0.12)
simDF$RETII_.80_colored<-round(simDF$total*0.05*0.12)

# SAVE FILE
write.csv(simDF, paste0(getwd(),"/",subDir, "/syntheticDF.csv"))


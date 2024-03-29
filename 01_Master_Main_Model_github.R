#####################################################################################################
# #      Trends in Temperature-associated Mortality in S�o Paulo (Brazil) between 2000 and 2018: 
# #             an Example of Disparities in Adaptation to Cold and Heat          
# #                              by Aina Roca-Barcelo                       
#####################################################################################################
# AUTHOR: Aina Roca Barcelo                                                                         
# CREATED: 17.11.2021                                                                               
# LAST MODIFIED: 27.10.2022                                                                         
# R Version 3.6.3.                                                                                  

# PAPER #############################################################################################
# Latest version of R code for the analysis in:                                                     
#
# Roca-Barcel�, A., Fecht, D., Pirani, M. et al. Trends in Temperature-associated Mortality in
# S�o Paulo (Brazil) between 2000 and 2018: an Example of Disparities in Adaptation to Cold and Heat. 
# J Urban Health (2022). https://doi.org/10.1007/s11524-022-00695-7
#####################################################################################################

# METADATA ##########################################################################################                                                                                         
#     * Exposure: Time series mean daily temperature - source: USP Station                          
#     * Outcome: Mortality data daily counts by gender, age and ethnic group - source: DATASUS      
#     * Confounders:                                                                                
#           * Daily mean relative humidity - source: USP Station                                    
#           * Daily mean PM10 - source : regional Environment Agency (CETESB)                                                        
#           * Day of the week (dow)                                                                 
#           * National holidays (holidays)                                                          
#           * Long-term trends & seasonality - natural cubic spline 10df per year                    
#####################################################################################################

#####################################################################################################
# MASTER MAIN MODEL
# This code: 
# (1) sets the parameters for the main model (i.e. the best fitting model based on AIC of sensitivity analyses); 
# (2) calls source code for tv-DLNM model, and 
# (3) defines lists to save/export outputs in a formated file and figures of choice
#####################################################################################################

#---------------------------------------------------------------------------------------------------------#
#THIS CHUNK ONLY RUN THE FIRST TIME#
rm(list=ls()) #clear the slate
.libPaths("directory/for/your/rversion/library") # Define location of your libraries directory
mainDir<-getwd() # Check directory is correct; otherwise set using 'setwd()'
mmt_noint<-list()#List to save mmt overall (no interaction model)

# 1. LOAD LIBRARIES AND DIRECTORIES
###########################
library(dlnm);library(mvmeta) ; library(splines); library(lubridate); library(MASS); library(tsModel); library(Epi)
library(pspline); library(mda); library(ggplot2); library(ggformula); library(data.table); library(splines); 
library(MASS); library(tsModel); library(openxlsx);library(stats); library(writexl); library(zoo)
#---------------------------------------------------------------------------------------------------------#

# 2. DEFINE/LOAD FUNCTIONS 
###########################
#We will need the following functions which are saved in 'functions.R':fqaic, findmin and  Ztest
#We can load them by calling the file with 'source'
source('functions.R', local=T)

# 3. SET DIRECTORY
####################
# Example for the follwoing folder structure: ./outputs/tables/date   and  ./outputs/figures/date
# We include today's date to avoid overwriting outputs
date<-Sys.Date()
dateFig<-file.path(mainDir, "outputs/figures/",date) # Desired end dir for figures
dateTab<-file.path(mainDir, "outputs/tables/",date) # Desired end dir for tables
sapply(c(dateFig, dateTab), function(x) if (!dir.exists(x)) dir.create(x)) # Creates the above dir if they don't exists

# 4.LOAD AND PREPARE DATA
##########################
# LOAD DATA
data<-read.csv("syntheticDF.csv") #replace this for your data

# DATA CHECKS
data$date<-as.Date(data$date, format="%Y-%m-%d") #check date format
data$year<-year(data$date) #Extract year from date
names(data)[names(data)=="nonext"]<-"total" #rename "nonext" to "total"

# DEFINE INTERACTION TERMS, STANDARDIZED; ANNUAL CENTERING POINT (1st July)
datenum <- as.numeric(data$date)
cen_datenum<-0 #Create an empty vector to save centering point
cen<-as.Date("2000-07-01")#Define centering point: 1st July
for(i in 1:19){
  year(cen)<-1999+i
  cen_datenum[[i]]<-as.numeric(cen)
}
#Define interaction using centeirng point
centering<-list() #empty list to save interaction varaible
for(i in 1:length(cen_datenum)){
  centering[[i]]<- ((datenum-cen_datenum[[i]])/(length(datenum)))
}

# DEFINE COVARIATES AS PER FINAL MODEL PARAMETRIZATION
data$dow<-weekdays(as.Date(data$date)) # Day of the week (DOW)
rh02<-ns(as.numeric(frollmean(data$HR_mean, 3, align="right",fill=NA)), df=3) #RH lag 0-2, natural cubic sline, 3df
pm10_lag0_2<-rollmean(data$pm10_mean.mean, 3, align="right",fill=NA)# PM10 lag 0-2

# DEFINE OUTCOMES TO EXPLORE
outcome<-c("total", "males", "females", "RETI_65.79", "RETII_.80", "females_RETI_65.79","females_RETII_.80", "males_RETI_65.79","males_RETII_.80",
           "white", "colored","males_colored","males_white", "females_colored", "females_white", "RETI_65.79_colored","RETI_65.79_white",
           "RETII_.80_colored","RETII_.80_white", "cvd", "resp" )#DO NOT RUN



# 5. MODEL OUTPUTS
###################
#DEFINE WETHER WE RUN INTERACTION MODEL (TRUE) OR FULL MODEL WITH NO INTERACTION (FALSE)
interac<-TRUE

#LOOP OVER INTERACTIONS / FULL MODEL(no interactions)
if(interac==TRUE){
  interactions<-c("int2000", "int2001",  "int2002", "int2003", "int2004", "int2005", "int2006", "int2007", "int2008", "int2009", "int2010", "int2011", "int2012","int2013", "int2014", "int2015", "int2016", "int2017", "int2018")
}else{interactions<-c("noint")}

# START LOOP FOR ANNUAL INTERACTIONS 
for(y in 1:length(interactions)){
  int<-interactions[[y]]

  # CREATE LISTS TO SAVE MODEL OUTPUTS
  out<-NA; results<-NA; qaic <- NA; WTint<-NA; mmt_constr_list <- NA; mmt_constr_low<-NA;mmt_constr_high<- mmt_constr_se<-NA;
  maxT<-NA; minT<-NA; meanT<-NA; start_year<-NA; end_year<-NA; 
  temp_list<-list(); RR_list<-list(); RRmmt_con_list<-list(); RRmmt_uncon_list<-list();RRmmt_int_con_list<-list()
  
  #Print year to know centering point
  print(unique(data$year))
  
  for(x in outcome){
        cat("Outcome ", x) #Print outcome for internal check
    health<-x
    out[x]<-health
    print(out)
   
    #DEFINE PARAMETRIZATION FOR MAIN MODEL
    # Parametrization is defined in "[DO NOT RUN]DLNM_source_code.R" based on list of sensitivity analysis (see paper Table SX). By restricting the list 
    # to num.1 we are calling for the Main Model parameters which are as follows:
    # Exposure basis function: cubic beta-spline with 1 knot at 75th percentile 
    # Lag basis function: cubic beta-spline with 3 knots equally spaces log scale + intercept + lag max: 21 days
    # Confounders: long-term trends and seasonality as natural cubic beta spline of time with 8df per year; RH term as ns(RH0, df=3) and PM10 term as pm10_lag02
    param<-25 #main model according to the sensitivity analyses
    #LOOP OVER ALL SENSITIVITY ANALYSES
    for(i in param){
      cat("Population group: ", x)
      cat("Parametrization group: ", i)
      sa<-i
    #call the script to be run
    source("DLNM_source_code_github.R")
    }  
    }

  #8.1. SUMMARY TABLE
  options(scipen = 999) # set decimal points
    model_summary<-data.frame(health=out,
                              qaic=qaic,
                              WaldTest=WTint,
                              mmt_constr=mmt_constr_list, mmt_constr_low=mmt_constr_low, mmt_constr_high=mmt_constr_high, mmt_constr_se=mmt_constr_se)
                              
    openxlsx::write.xlsx(model_summary, paste0(dateTab, "/Main_Model_summary_", int, ".xlsx"), overwrite = T)
    
    #save
    openxlsx::write.xlsx(RRmmt_con_list, paste0(dateTab, "/Main_Model_RR_MMTconstr_", int, ".xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)
    openxlsx::write.xlsx(RRmmt_int_con_list, paste0(dateTab, "/Main_Model_RR_INT_MMTconstr_", int, ".xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)
}
  

#END OF FILE


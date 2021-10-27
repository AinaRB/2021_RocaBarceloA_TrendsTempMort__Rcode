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
#     * Exposure: Time series mean daily temperature - source: USP Station                          
#     * Outcome: Mortality data daily counts by gender, age and ethnic group - source: DATASUS      
#     * Confounders:                                                                                
#           * Daily mean relative humidity - source: USP Station                                    
#           * Daily mean PM10 - source : XX?                                                        
#           * Day of the week (dow)                                                                 
#           * National holidays (holidays)                                                          
#           * Long-term trends & seasonality - natural cubic spline 8df per year                    
#####################################################################################################

#####################################################################################################
# MASTER MAIN MODEL
# This code: 
# (1) sets the parameters for the main model (i.e. thebest fitting model based on AIC of sensitivity analyses); 
# (2) calls source code for tv-DLNM model, and 
# (3) defines lists to save/export outputs in a formated file and figures of choice
#####################################################################################################

rm(list=ls()) #clear the slate

# 1. LOAD LIBRARIES AND DIRECTORIES
#####################################
library(dlnm);library(mvmeta) ; library(splines); library(lubridate); library(MASS); library(tsModel); library(Epi)
library(pspline); library(mda); library(ggplot2); library(ggformula); library(data.table); library(splines); 
library(MASS); library(tsModel); library(openxlsx);library(stats); library(writexl); library(zoo)

# 2. DEFINE/LOAD FUNCTIONS 
###########################
# FOR Q-AIC [Gasparrini A B Armstrong, and M G Kenward (2010)]
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}
# FOR FINDING MINIMUM MORTALITY TEMPERATURE WITH UNCERTAINITIES 
# Source: Tobias A, Armstrong B, Gasparrini A. Investigating uncertainty in the minimum mortality temperature: methods and application to 52 Spanish cities. Epidemiology. 2017;28(1):72-76
# GitHub: https://github.com/gasparrini/2017_tobias_Epidem_Rcodedata
download.file(url = "https://github.com/gasparrini/2017_tobias_Epidem_Rcodedata-master.zip"
              , destfile = "2017_tobias_Epidem_Rcodedata-master.zip") # Download folder from GitHug
unzip(zipfile = "2017_tobias_Epidem_Rcodedata-master.zip")# unzip the .zip file
file.remove("2017_tobias_Epidem_Rcodedata-master.zip") #remove .zip file
source("2017_tobias_Epidem_Rcodedata-master/findmin.R", local=TRUE)

# FOR MULTIVARIATE WALD TEST
Ztest <- function(b1,V1,b2=NULL,V2=NULL) {
  invVp <- if(is.null(b2)) solve(V1) else solve(V1+V2)
  b <- if(is.null(b2)) b1 else b1-b2
  stat <- t(b)%*%invVp%*%(b) #Given a matrix or data.frame x, t returns the transpose of x. // %*% is matrix multiplication. 
  df <- length(b1) 
  pchisq(stat,df,lower.tail=FALSE) 
}


# 3. SET DIRECTORY
####################
# Example for the follwoing folder structure: ./outputs/tables/date   and  ./outputs/figures/date
# We include today's date to avoid overwriting outputs
date<-Sys.Date()
getwd() # check directory is correct
dateFig<-file.path(getwd(), "outputs/figures/",date) # Desired end dir for figures
dateTab<-file.path(getwd(), "outputs/tables/",date) # Desired end dir for tables
sapply(c(dateFig, dateTab), function(x) if (!dir.exists(x)) dir.create(x)) # Creates the above dir if they don't exists

# 4.LOAD AND PREPARE DATA
##########################
# LOAD DATA
data<-read.csv("mortality_sp_model_exploration.csv")

# DATA CHECKS
data$date<-as.Date(data$date, format="%Y-%m-%d") #check date format
data$year<-year(data$date) #Extract year from date
names(data)[names(data)=="nonext"]<-"total" #rename "nonext" to "total"

# DEFINE INTERACTION TERMS, STANDARDIZED; ANNUAL CENTERING POINT (15th August)
datenum <- as.numeric(data$date)
day0<-as.numeric(as.Date("2000-08-01"));day1<-as.numeric(as.Date("2001-08-01"));day2<-as.numeric(as.Date("2002-08-01"));
day3<-as.numeric(as.Date("2003-08-01"));day4<-as.numeric(as.Date("2004-08-01"));day5<-as.numeric(as.Date("2005-08-01"));
day6<-as.numeric(as.Date("2006-08-01"));day7<-as.numeric(as.Date("2007-08-01"));day8<-as.numeric(as.Date("2008-08-01"));
day9<-as.numeric(as.Date("2009-08-01"));day10<-as.numeric(as.Date("2010-08-01"));day11<-as.numeric(as.Date("2011-08-01"));
day12<-as.numeric(as.Date("2012-08-01"));day13<-as.numeric(as.Date("2013-08-01"));day14<-as.numeric(as.Date("2014-08-01"));
day15<-as.numeric(as.Date("2015-08-01"));day16<-as.numeric(as.Date("2016-08-01"));day17<-as.numeric(as.Date("2017-08-01"));
day18<-as.numeric(as.Date("2018-08-01"));day19<-as.numeric(as.Date("2018-12-31"));
interactions<-c("int2000", "int2001",  "int2002", "int2003", "int2004", "int2005", "int2006", "int2007", "int2008", "int2009", "int2010", "int2011", "int2012", "int2013", "int2014", "int2015", "int2016", "int2017", "int2018")

# DEFINE COVARIATES 
data$dow<-weekdays(as.Date(data$date)) # Day of the week (DOW)
rh02<-ns(as.numeric(frollmean(data$HR_mean, 3, align="right",fill=NA)), df=3) #RH
pm10_lag0_2<-rollmean(data$pm10_mean.mean, 3, align="right",fill=NA)# PM10


# DEFINE OUTCOMES TO EXPLORE
outcome<-c("total", "males", "females", "RETI_65.79", "RETII_.80", "females_RETI_65.79","females_RETII_.80", "males_RETI_65.79","males_RETII_.80",
           "cvd","resp","white", "colored","males_colored","males_white", "females_colored", "females_white", "RETI_65.79_colored","RETI_65.79_white",
           "RETII_.80_colored","RETII_.80_white")


# 5. MODEL OUTPUTS
###################
#DEFINE WETHER WE RUN INTERACTION MODEL OR FULL MODEL
interac<-T

#LOOP OVER ITNERACTIONS / FULL MODEL
if(interac==TRUE){
  interactions<-c("int2000", "int2001",  "int2002", "int2003", "int2004", "int2005", "int2006", "int2007", "int2008", "int2009", "int2010", "int2011", "int2012","int2013", "int2014", "int2015", "int2016", "int2017", "int2018")
}else{interactions<-c("noint")}

# START LOOP FOR ANNUAL INTERACTIONS 
for(y in interactions){
  int<-y
  
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
    param<-22
    #LOOP OVER ALL SENSITIVITY ANALYSES
    for(i in param){
      cat("Sensitivity Analysis ", i)
      sa<-i
    #call the script to be run
    source("[DO NOT RUN] DLNM_source_code.R")
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
    names(RRmmt_con_list)<-outcome
    openxlsx::write.xlsx(RRmmt_con_list, paste0(dateTab, "/Main_Model_RR_MMTconstr_", int, ".xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)
    openxlsx::write.xlsx(RRmmt_int_con_list, paste0(dateTab, "/Main_Model_RR_INT_MMTconstr_", int, ".xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)
}
  

#END OF FILE


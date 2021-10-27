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
# MASTER SENSITIVITY ANALYSES
# This code: 
# (1) sets the parameters for all sensitivity analyses (see paper Table Sx); 
# (2) calls source code for tv-DLNM model, and 
# (3) defines lists to save/export outputs in a formated file and figures of choice
#####################################################################################################
# NOTE: Please consider the follwoing:
#   * The sensitivity analysis are run on "total" cases only. 
#   * Model fit for the interaction models does not change with the centering point. 
#     Hence, we run only for one centering point i.e., year 2000
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
# Example for the follwoing folder structure: ./sa/tables/date   and  ./sa/figures/date
# We include today's date to avoid overwriting outputs
date<-Sys.Date()
getwd() # check directory is correct
dateFig<-file.path(getwd(), "sa/figures/",date) # Desired end dir for figures
dateTab<-file.path(getwd(), "sa/tables/",date) # Desired end dir for tables
sapply(c(dateFig, dateTab), function(x) if (!dir.exists(x)) dir.create(x, recursive = TRUE)) # Creates the above dir if they don't exists

# 4.LOAD AND PREPARE DATA
##########################
# LOAD DATA
data<-read.csv("mortality_sp_model_exploration.csv")

# DATA CHECKS
data$date<-as.Date(data$date, format="%Y-%m-%d") #check date format
data$year<-year(data$date) #Extract year from date
names(data)[names(data)=="nonext"]<-"total" #rename "nonext" to "total"

# DEFINE COVARIATES 
#Relative humidity (RH)
data$HR_lag0_1<-frollmean(data$HR_mean, 2, align="right",fill=NA)
data$HR_lag0_2<-frollmean(data$HR_mean, 3, align="right",fill=NA)
rh0<-ns(as.numeric(data$HR_mean), df=3)
rh01<-ns(as.numeric(data$HR_lag0_1), df=3)
rh02<-ns(as.numeric(data$HR_lag0_2), df=3)
rh0_df4<-ns(as.numeric(data$HR_mean), df=4)

#Air pollution, PM10
data$pm10_lag0_1<-rollmean(data$pm10_mean.mean, 2, align="right",fill=NA)
data$pm10_lag0_2<-rollmean(data$pm10_mean.mean, 3, align="right",fill=NA)
data$pm10_lag0_3<-rollmean(data$pm10_mean.mean, 4, align="right",fill=NA)

#Day of the week
data$dow<-weekdays(as.Date(data$date)) # Day of the week (DOW)

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

# DEFINE OUTCOMES TO EXPLORE
outcome<-c("total", "males", "females", "RETI_65.79", "RETII_.80", "females_RETI_65.79","females_RETII_.80", "males_RETI_65.79","males_RETII_.80",
           "cvd","resp","white", "colored","males_colored","males_white", "females_colored", "females_white", "RETI_65.79_colored","RETI_65.79_white",
           "RETII_.80_colored","RETII_.80_white")

#3. FIT MODEL

#list to save model parameters
out<-0; results<-0; qaic <- 0; WTint<-0; qaic_res1<-0; qaic_res2<-0; qaic_res3<-0; pacf<-0; pacf_res1<-0
pacf_res2<-0; pacf_res3<-0; mmt_unconstr_list <- 0; mmt_unconstr_low<-0;mmt_unconstr_high<- mmt_unconstr_se<-0;
mmt_constr1_list <- 0; mmt_constr1_low<-0;mmt_constr1_high<-0; mmt_constr1_se<-0;
mmt_constr_list <- 0; mmt_constr_low<-0;mmt_constr_high<- mmt_constr_se<-0;
maxT<-0; minT<-0; meanT<-0; start_year<-0; end_year<-0;
maxT<-0; minT<-0; meanT<-0; start_year<-0; end_year<-0;
temp_list<-list() ;   RR_list<-list();  RRmmt_con_list<-list() ;  RRmmt_uncon_list<-list();RRmmt_int_con_list<-list()

#subset data by running periods
print(unique(data$year))

#DEFINE OUTCOME
health<-"total"
#DEFINE WEHTHER WE EXPLORE INTERACITON OR FULL MODEL
interac<-FALSE

if(interac==TRUE){
  interactions<-c("int2000")
  }else{interactions<-c("noint")}

#DEFINE SENSITIVYT ANALYSIS (NUMBERS CORRESPOND TO TABLE X in paper)
param<-c(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10, 11, 12, 13, 14, 15,16,17,18,19,20,21,22)

#LOOP OVER INTERACTIONS/FULL MODEL  
for(i in interactions){
    int<-i
    #LOOP OVER ALL SENSITIVITY ANALYSES
    for(x in param){
      cat("Sensitivity Analysis ", i)
      sa<-x
       #call the script to be run
      source("[DO NOT RUN] DLNM_source_code.R")
    }
    }
model_summary<-data.frame(SA=param,
                          qaic=qaic,
                          WaldTest=WTint,
                          mmt_constr=mmt_constr_list, mmt_constr_low=mmt_constr_low, mmt_constr_high=mmt_constr_high, mmt_constr_se=mmt_constr_se)

openxlsx::write.xlsx(model_summary, paste0(dateTab, "/" , int, "_model_summary.xlsx"), overwrite = T)
#save
names(RRmmt_con_list)<-param
openxlsx::write.xlsx(RRmmt_con_list, paste0(dateTab, "/", int, "_model_RR_MMTconstr.xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)

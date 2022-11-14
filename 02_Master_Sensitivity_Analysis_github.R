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
# Latest version of R code for the analysis in:                                                     
#
# Roca-Barceló, A., Fecht, D., Pirani, M. et al. Trends in Temperature-associated Mortality in
# São Paulo (Brazil) between 2000 and 2018: an Example of Disparities in Adaptation to Cold and Heat. 
# J Urban Health (2022). https://doi.org/10.1007/s11524-022-00695-7
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
dateFig<-file.path(mainDir, "sa/figures/",date) # Desired end dir for figures
dateTab<-file.path(mainDir, "sa/tables/",date) # Desired end dir for tables
sapply(c(dateFig, dateTab), function(x) if (!dir.exists(x)) dir.create(x)) # Creates the above dir if they don't exists

# 4.LOAD AND PREPARE DATA
##########################
# LOAD DATA
data<-read.csv("syntheticDF.csv") #replace this for your data

#DEFINE SENSITIVYT ANALYSIS (NUMBERS CORRESPOND TO TABLE X in paper)
param<-c(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10, 11, 12, 13, 14, 15,16,17,18,19,20,21,22,23,24,25)

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
                            SA=param,
                            qaic=qaic,
                            WaldTest=WTint,
                            mmt_constr=mmt_constr_list, mmt_constr_low=mmt_constr_low, mmt_constr_high=mmt_constr_high, mmt_constr_se=mmt_constr_se)
  
  openxlsx::write.xlsx(model_summary, paste0(dateTab, "/Main_Model_summary_", int, ".xlsx"), overwrite = T)
  
  #save
  openxlsx::write.xlsx(RRmmt_con_list, paste0(dateTab, "/Main_Model_RR_MMTconstr_", int, ".xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)
  openxlsx::write.xlsx(RRmmt_int_con_list, paste0(dateTab, "/Main_Model_RR_INT_MMTconstr_", int, ".xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)
}


#END OF FILE





























#---------------------------------------------------------------------------------------------------------#
#THIS CHUNK ONLY RUN THE FIRST TIME#
rm(list=ls()) #clear the slate
.libPaths("C:/icnas1.cc.ic.ac.uk/arocabar/R/win-library/3.6") # Define location of your libraries directory
dir<-getwd() # check directory is correct; otherwise set using 'setwd()'
mmt_noint<-list()#List to save mmt overall (no interaction model)

# 1. LOAD LIBRARIES AND DIRECTORIES
library(dlnm);library(mvmeta) ; library(splines); library(lubridate); library(MASS); library(tsModel); library(Epi)
library(pspline); library(mda); library(ggplot2); library(ggformula); library(data.table); library(splines); 
library(MASS); library(tsModel); library(openxlsx);library(stats); library(writexl); library(zoo)
#---------------------------------------------------------------------------------------------------------#

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
#download.file(url = "https://github.com/gasparrini/2017_tobias_Epidem_Rcodedata-master.zip"
#              , destfile = "2017_tobias_Epidem_Rcodedata-master.zip") # Download folder from GitHug
#unzip(zipfile = "2017_tobias_Epidem_Rcodedata-master.zip")# unzip the .zip file
#file.remove("2017_tobias_Epidem_Rcodedata-master.zip") #remove .zip file
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
data<-read.csv("mortality_sp_model_exploration_Nov2021.csv")

# DATA CHECKS
data$date<-as.Date(data$date, format="%Y-%m-%d") #check date format
data$year<-year(data$date) #Extract year from date
names(data)[names(data)=="nonext"]<-"total" #rename "nonext" to "total"

# DEFINE INTERACTION TERMS, STANDARDIZED; ANNUAL CENTERING POINT (15th August)
datenum <- as.numeric(data$date)
cen_datenum<-0 #Create an empty vector to save centering point
cen<-as.Date("2000-07-01")#define centering point: 1st July
for(i in 1:19){
  year(cen)<-1999+i
  cen_datenum[[i]]<-as.numeric(cen)
}
#Define interaction using centeirng point
centering<-list() #empty list to save interaction varaible
for(i in 1:length(cen_datenum)){
  #centering[[i]]<- ((datenum-cen_datenum[[i]])/(tail(cen_datenum, n=1)-head(cen_datenum, n=1)))
  centering[[i]]<- ((datenum-cen_datenum[[i]])/(length(datenum)))
}
#DEFINE WETHER WE RUN INTERACTION MODEL OR FULL MODEL
interac<-TRUE

#NAME THE ITNERACTIONS 
if(interac==TRUE){
  interactions<-c("int2000", "int2001",  "int2002", "int2003", "int2004", "int2005", "int2006", "int2007", "int2008", "int2009", "int2010", "int2011", "int2012","int2013", "int2014", "int2015", "int2016", "int2017", "int2018")
}else{interactions<-c("noint")}

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

# 5. MODEL OUTPUTS
###################
#DEFINE SENSITIVYT ANALYSIS (NUMBERS CORRESPOND TO TABLE X in paper)
param<-c(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10, 11, 12, 13, 14, 15,16,17,18,19,20,21,22,23,24,25)

#DEFINE HEALTH OUTCOME TO EXPLORE: TOTAL
health<-"total"

# START LOOP FOR ANNUAL INTERACTIONS 
for(y in 1:length(interactions)){
  int<-interactions[[y]]
  
  # CREATE LISTS TO SAVE MODEL OUTPUTS
  out<-NA; results<-NA; qaic <- NA; WTint<-NA; mmt_constr_list <- NA; mmt_constr_low<-NA;mmt_constr_high<- mmt_constr_se<-NA;
  maxT<-NA; minT<-NA; meanT<-NA; start_year<-NA; end_year<-NA; 
  temp_list<-list(); RR_list<-list(); RRmmt_con_list<-list(); RRmmt_uncon_list<-list();RRmmt_int_con_list<-list()
  
  #Print year to know centering point
  print(unique(data$year))
  
  #LOOP OVER ALL SENSITIVITY ANALYSES
  for(x in 1:25){
    cat("Sensitivity Analysis ", x)
    sa<-x
    #call the script to be run
    source("DLNM_source_code.R")
  }
  
  model_summary<-data.frame(SA=param,
                            qaic=qaic,
                            WaldTest=WTint,
                            mmt_constr=mmt_constr_list, mmt_constr_low=mmt_constr_low, mmt_constr_high=mmt_constr_high, mmt_constr_se=mmt_constr_se)
  
  openxlsx::write.xlsx(model_summary, paste0(dateTab, "/" , int, "_model_summary.xlsx"), overwrite = T)
  #save
  names(RRmmt_con_list)<-param
  openxlsx::write.xlsx(RRmmt_con_list, paste0(dateTab, "/", int, "_model_RR_MMTconstr.xlsx"), overwrite = T, colNames = TRUE,asTable = FALSE)
  
}


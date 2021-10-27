#####################################################################
# #                  MODEL EXPLORATION                            # #
# #                       Trends                                  # #
#####################################################################
#Author: Aina Roca Barcelo
#Created: 17.11.2020
#Last modified: 17.11.2020
#Version 3.6.3.

# METADATA ###########################################################################################
# For this first exploration of the models and in preparation of the ISEE Young conference, we wll   # 
# run the models on the following "test datasets":                                                   #
#     * Exposure: Time series mean daily temperature from USP Station                                #
#     * Outcome: Mortality data all cause - complete dataset only (i.e. sex, age)                    #
#     * Confounders: Humidity series (same as exposure); air pollution (ESA)                         #
######################################################################################################

# TRENDS  ###########################################################################################
# This analysis has been inspired by the previous work of                                           #
#    Achebak H and Ballester J (2019) Trends in temperature-related age-specific and sex-specific   #
#    mortality from cardiovascular diseases in Spain: a national time-series analysis               #
#####################################################################################################

#---------- No need to run this chunck if runing through master script-------------------------------#

library(dlnm)


#2. SPECIFY PARAMETERS FOR MODEL
################################
lag_max<-0; lag_knots<-0; lag_fun<-list();lag_int<-0;temp_fun<-0;temp_degree<-0; temp_per<-list(); temp_knots<-list(); trend_df<-list(); season_df<-list()

#2.1. SPECIFY TEMPERATURE DIST PERCNTILS
##############
#LAG BASIS PARAMETERS
if(sa==9){lag<-15} else if (sa==10){lag<-24} else {lag<-21}; print(lag); lag_max[i]<-lag
if(sa==8){lagnk<-2} else {lagnk<-3}; print(lagnk); lag_knots[i]<-lagnk
#lagnk <- 2 #number of knots for lag
lagfun<-"ns"; lag_fun[i]<-lagfun
lagint<-TRUE; lag_int[i]<-lagint #intercept for lag basis

#TEMPERATURE BASIS PARAMETERS
tempfun<- "bs"; temp_fun[i]<-tempfun #function for temperature
if(sa==1){degree<-2} else {degree<-3}; print(degree); temp_degree[i]<-degree
if(sa==3){varper<-c(90)} else if(sa==4){varper<-c(50,90)} else if(sa==5){varper<-c(25,50,90)} else if(sa==6){varper<-c(25,50,90,95)} else if (sa==7){varper<-c(10, 75, 90)} else {varper<-c(75)} ; print(varper);temp_per[i]<-paste(varper, collapse=",") #location knots temperature percentiles
temp_knots[i]<-length(varper)

#CONFOUNDERS PARAMETERS
if(sa==11){dfseason<-7} else if(sa==12){dfseason<-9} else if (sa==13){dfseason<-10} else if (sa==14){dfseason<-3} else if (sa==2){dfseason<-8} else {dfseason<-10}; print(dfseason); season_df[i] <-dfseason#degrees of freedom for season
if(sa==15){rh_pm10<-"rh0"}else if(sa==16){rh_pm10<-"rh01"}else if(sa==17){rh_pm10<-"rh02"}else if(sa==18){rh_pm10<-"rh0_df4"}else if(sa==19){rh_pm10<-"pm10_lag0_1"}else if(sa==20){rh_pm10<-"pm10_lag0_2"}else if(sa==21){rh_pm10<-"pm10_lag0_3"} else if(sa==22){pm10<-"pm10_lag0_2"; rh<-"rh02"}

#1. DESCRIPTIVES FOR TEMPERATURE
##################################
meanT[x]<-mean(data$temp_mean)
maxT[x]<-max(data$temp_max)
minT[x]<-min(data$temp_min)

#2.2. CREATE ARGUMENTS TO PASS TO FORMULA
#########################################
argvar<-list(fun=tempfun, degree=degree,knots=quantile(data$temp_mean,varper/100,na.rm=T))
arglag<-list(knots=logknots(lag,lagnk), intercept=lagint, fun=lagfun)

#2.2. subecify crossbasis functions
###################################
#OPTION A
cb<-crossbasis(data$temp_mean, lag, argvar, arglag)
summary(cb); plot(cb)

#2.4. MODEL FORMULAE
if(sa<15){if(interac==TRUE){formula <- paste0("`", x, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) + INT")} else{formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year)))")}}else if(sa==22){if(interac==TRUE){
  formula <- paste0("`", x, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) + INT +`", pm10, "`+`",rh, "`" )} else{formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) +`", pm10, "`+`",rh, "`")}}else{
    if(interac==TRUE){formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) + INT+ `", rh_pm10, "`")} else{formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year)))+ `", rh_pm10, "`")}}


#DEFINE INTERACTION (if time_verying model; i.e. int<-TRUE
if(int=="int2000"){ INT <- ((datenum-day0)/(day18-day0))*cb}else if(int=="int2001"){INT <- ((datenum-day1)/(day18-day1))*cb}else if(int=="int2002"){
  INT <- ((datenum-day2)/(day18-day2))*cb}else if(int=="int2003"){INT <- ((datenum-day3)/(day18-day3))*cb}else if(int=="int2004"){
    INT <- ((datenum-day4)/(day18-day4))*cb}else if(int=="int2005"){INT <- ((datenum-day5)/(day18-day5))*cb}else if(int=="int2006"){
      INT <- ((datenum-day6)/(day18-day6))*cb}else if(int=="int2007"){INT <- ((datenum-day7)/(day18-day7))*cb}else if(int=="int2008"){
        INT <- ((datenum-day8)/(day18-day8))*cb}else if(int=="int2009"){INT <- ((datenum-day9)/(day18-day9))*cb}else if(int=="int2010"){
          INT <- ((datenum-day10)/(day18-day10))*cb}else if(int=="int2011"){INT <- ((datenum-day11)/(day18-day11))*cb}else if(int=="int2012"){
            INT <- ((datenum-day12)/(day18-day12))*cb}else if(int=="int2013"){INT <- ((datenum-day13)/(day18-day13))*cb}else if(int=="int2014"){
              INT <- ((datenum-day14)/(day18-day14))*cb}else if(int=="int2015"){INT <- ((datenum-day15)/(day18-day15))*cb}else if(int=="int2016"){
                INT <- ((datenum-day16)/(day18-day16))*cb}else if(int=="int2017"){INT <- ((datenum-day17)/(day18-day17))*cb}else{INT <- ((datenum-day18)/(day19-day18))*cb}
  

# 3. RUN THE MODEL 
model1<-glm(formula = formula, family=quasipoisson(), data, na.action = na.exclude)
print(summary(model1))
fqaic(model1)

##3.1. MODEL PERFORMANCE CHECKS - QAIC
qaic[x]<-fqaic(model1)


#4.SUMMARIZING RESULTS
######################
# 4.1. EXPONENTIAL COEFFICIENTS
###############################
print(ci.exp(model1, subset="cb"))
# Comments: The coefficients of the unconstrained DLM represent the RR subecific to each lag.
# The coefficients of the subline DLM are difficult to interpret. 
# In addition, the most interesting estimate, that is the net RR
# cumulate along lags, is not available.

#4.2. OBTAIN MMT
#################
# We used the function developed by Tobias et al 2017 and avaialble at Github repository:https://github.com/gasparrini/2017_tobias_Epidem_Rcodedata
# To avoid high uncertainty in the tails affecting the placement of the MMT, we restricted its computation between the 1st/99th percentile temperature distribution
print("OBTAIN 1th/99th percentile constrained MMP")
per1<-quantile(data$temp_mean, 0.01, na.rm=T);per99<-quantile(data$temp_mean, 0.99, na.rm=T) 
predper <- c(seq(0,1,0.1),2:98,seq(99,100,0.1))
set.seed(50000000)
per<-quantile(jitter(data$temp_mean), predper/100,na.rm=T)
(min2_constr<-findmin(cb, model1,at=per,from=per1, to=per99)); mmt_constr_list[x]<-min2_constr #we use the percentile distribution (per) and limit it to be between 1st and 99th
(min2ci_constr <- quantile(findmin(cb,model1,at=per, from=per1, to=per99, sim=T),c(5,95)/100, na.rm=TRUE)); mmt_constr_low[x]<-min2ci_constr[1]; mmt_constr_high[x]<-min2ci_constr[2]
(min2se_constr <- sd(findmin(cb,model1,at=per, from=per1, to=per99,sim=T))); mmt_constr_se[x]<-min2se_constr

#Use the estimated MMP to readjust the predictions
pred.temp.mmt.const<-crosspred(cb, model1, at=per, cen=min2_constr, cumul=T) # we get the values for all temperature srecorded in "per" which correspond to specific percentiles 
if(int=="noint"){print("no interaction  crosspredict")}else{pred.int.mmt.const<-crosspred(INT, model1, at=per, cen=min2_constr, cumul=T)} # we get the values for all temperature srecorded in "per" which correspond to specific percentiles 
plot(pred.temp.mmt.const)


#4. WALD TEST FOR CHANGE
##########################
#CROSSPRED INTERACTION (SAME FOR ALL INTERACTIONS)
if(interac==TRUE){
  cpint<- crosspred(INT, model1, at=per, cen=min2_constr, cumul=T) # we get the values for all temperature srecorded in "per" which correspond to specific percentiles 
  # MULTIVARIATE TESTS FOR A NULL INTERACTION (P-VALUE)
  WTint[i]<-Ztest(coef(cpint),vcov(cpint)); print(WTint)
}else{print("no interaction")}

#5.3. Summary of exposure respoonse at subecific predictor value over different lags
#####
var99th<-per[109]; var90th<-per[100] # explore percentile 99th & 90th
var99thMMT<-round(var99th); var90thMMT<-round(var90th); #for MMT we predicted by=1, so we round that value

# Extract the overall RR and CI and store
RRmmt.con<-as.data.frame(with(pred.temp.mmt.const, cbind(allRRfit, allRRlow, allRRhigh))); RRmmt.con$index<-row.names(RRmmt.con); RRmmt.con$per<-rownames(as.data.frame(per))
RRmmt_con_list[[x]]<-as.data.frame(RRmmt.con)
if(int=="noint"){print("no interaction  crosspredict")}else{RRmmt.int.con<-as.data.frame(with(pred.int.mmt.const, cbind(allRRfit, allRRlow, allRRhigh))); RRmmt.int.con$index<-row.names(RRmmt.int.con); RRmmt.int.con$per<-rownames(as.data.frame(per));
RRmmt_int_con_list[[x]]<-as.data.frame(RRmmt.int.con)}

#6. REDUCED DLNM
#################
#A complexity of DLNM is the fact that the model is defined by a combination of two functions, typically#
#generating a high number of parameters. For some applications, it is possible to reduce a DLNM to summaries
#defined in only one dimension of either predictor or lags, re-expressing the fit in terms of reduced parameters
#for the related uni-dimensional basis functions.The argument ptype by default is "overall", thus reducing to the overall cumulative exposure-resubonse.
#The other exposure-resubonse at a subecific lag value (4) and lag-resubonse at a subecific exposure value (25?C)
#are also derived.
#we will centre the predictions o the MMT
#value in lag specified the single lag value used to define the association
#value in var specified the single temp value used to define the association
redall<-crossreduce(cb, model1, type="overall", cen = min2_constr, by=1, bylag=0.2)
redlag<-crossreduce(cb, model1, type="lag", value=lag, cen = min2_constr, by=1, bylag=1)
redvar<-crossreduce(cb, model1, type="var", value=var90thMMT, cen = min2_constr, by=1, bylag=1)

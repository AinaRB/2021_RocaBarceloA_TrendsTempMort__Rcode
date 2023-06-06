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

# METADATA ###########################################################################################
# This script contains the main model for all the analyses (interacction and no-interaction) called  #
# by the other files. The [] refer are specified in file: 01.Mater_Main_Model_github.R, where:       #
# i: year/ x: outcome                                                                                #
######################################################################################################


#--------- No need to run this script if running through master script (recommended) -----------------#



#2. SPECIFY PARAMETERS FOR MODEL
################################
lag_max<-0; lag_knots<-0; lag_fun<-list();lag_int<-0;temp_fun<-0;temp_degree<-0; temp_per<-list(); temp_knots<-list(); trend_df<-list(); season_df<-list()

#2.1. SPECIFY TEMPERATURE DIST PERCNTILS
##############
#LAG BASIS PARAMETERS
##The numebring of the sensitivity analsyes (SA) relates to Table S4 Supplematnary Material
if(sa==11){lag<-15} else if (sa==12){lag<-24} else {lag<-21}; print(lag); lag_max[i]<-lag
if(sa==8){lagnk<-2}  else {lagnk<-3}; print(lagnk); lag_knots[i]<-lagnk
#lagnk <- 2 #number of knots for lag
lagfun<-"ns"; lag_fun[i]<-lagfun
lagint<-TRUE; lag_int[i]<-lagint #intercept for lag basis

#TEMPERATURE BASIS PARAMETERS
tempfun<- "bs"; temp_fun[i]<-tempfun #function for temperature
if(sa==1){degree<-2} else {degree<-3}; print(degree); temp_degree[i]<-degree
if(sa==2){varper<-c(75)} else if(sa==3){varper<-c(90)} else if(sa==4){varper<-c(50,90)} else if(sa==5){varper<-c(25,50,90)} else if(sa==6){varper<-c(25,50,90,95)} else if (sa==7){varper<-c(10, 75, 90)} else {varper<-c(75)} ; print(varper);temp_per[i]<-paste(varper, collapse=",") #location knots temperature percentiles
temp_knots[i]<-length(varper)

#CONFOUNDERS PARAMETERS
if(sa==13){dfseason<-10} else if(sa==14){dfseason<-9} else if (sa==15){dfseason<-8} else if (sa==16){dfseason<-7} else if (sa==17){dfseason<-3} else {dfseason<-10}; print(dfseason); season_df[i] <-dfseason#degrees of freedom for season
if(sa==18){rh_pm10<-"rh0"}else if(sa==19){rh_pm10<-"rh01"}else if(sa==20){rh_pm10<-"rh02"}else if(sa==21){rh_pm10<-"rh0_df4"}else if(sa==22){rh_pm10<-"pm10_lag0_1"}else if(sa==23){rh_pm10<-"pm10_lag0_2"}else if(sa==24){rh_pm10<-"pm10_lag0_3"} else if(sa==25) {pm10<-"pm10_lag0_2"; rh<-"rh02"}

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
cb<-crossbasis(data$temp_mean, lag, argvar, arglag)
summary(cb); plot(cb)

#DEFINE INTERACTION (if time_verying model; i.e. int<-TRUE
INT<- ((datenum-cen_datenum[[y]])/(tail(cen_datenum, n=1)-head(cen_datenum, n=1)))*cb

#2.4. MODEL FORMULAE
if(sa<18){if(interac==TRUE){formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) + INT")} else{formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year)))")}}else if(sa==25){if(interac==TRUE){
  formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) + INT +`", pm10, "`+`",rh, "`" )} else{formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) +`", pm10, "`+`",rh, "`")}}else{
    if(interac==TRUE){formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year))) + INT+ `", rh_pm10, "`")} else{formula <- paste0("`", health, "` ~ cb + dow + as.factor(holidays) + ns(date,df=dfseason*length(unique(year)))+ `", rh_pm10, "`")}}

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
if(int=="noint"){print("no interaction  crosspredict"); mmt_noint[[x]]<-min2_constr}else{print(mmt_noint[[x]]);pred.int.mmt.const<-crosspred(INT, model1, at=per, cen=mmt_noint[[x]], cumul=T)} # we get the values for all temperature srecorded in "per" which correspond to specific percentiles 
plot(pred.temp.mmt.const)


#4. WALD TEST FOR CHANGE
##########################
#CROSSPRED INTERACTION (SAME FOR ALL INTERACTIONS)
if(interac==TRUE){
  cpint<- crosspred(INT, model1, at=per, cen=min2_constr, cumul=T) # we get the values for all temperature srecorded in "per" which correspond to specific percentiles 
  # MULTIVARIATE TESTS FOR A NULL INTERACTION (P-VALUE)
  WTint[x]<-Ztest(coef(cpint),vcov(cpint)); print(WTint)
}else{print("no interaction")}

#5.3. Summary of exposure respoonse at subecific predictor value over different lags
#####
var99th<-per[109]; var90th<-per[100] # explore percentile 99th & 90th
var99thMMT<-round(var99th); var90thMMT<-round(var90th); #for MMT we predicted by=1, so we round that value

# Extract the overall RR and CI and store
RRmmt.con<-as.data.frame(with(pred.temp.mmt.const, cbind(allRRfit, allRRlow, allRRhigh))); RRmmt.con$index<-row.names(RRmmt.con); RRmmt.con$per<-rownames(as.data.frame(per))
RRmmt_con_list[[x]]<-as.data.frame(RRmmt.con)
if(int=="noint"){print("no interaction  crosspredict")}else{
  RRmmt.int.con<-as.data.frame(with(pred.int.mmt.const, cbind(allRRfit, allRRlow, allRRhigh))); RRmmt.int.con$index<-row.names(RRmmt.int.con); RRmmt.int.con$per<-rownames(as.data.frame(per));
RRmmt_int_con_list[[x]]<-as.data.frame(RRmmt.int.con)}

#6. REDUCED DLNM
#################
#we will centre the predictions o the MMT
#value in lag specified the single lag value used to define the association
#value in var specified the single temp value used to define the association
redall<-crossreduce(cb, model1, type="overall", cen = min2_constr, by=1, bylag=0.2)
redlag<-crossreduce(cb, model1, type="lag", value=lag, cen = min2_constr, by=1, bylag=1)
redvar<-crossreduce(cb, model1, type="var", value=var90thMMT, cen = min2_constr, by=1, bylag=1)


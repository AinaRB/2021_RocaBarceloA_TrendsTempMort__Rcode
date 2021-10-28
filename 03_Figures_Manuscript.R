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
# FIGURES MANUSCRIPT
# This code generates the figures for the manuscript
#####################################################################################################

rm(list=ls())
.libPaths("C:/icnas1.cc.ic.ac.uk/arocabar/R/win-library/3.6")
#LOAD PACKAGES
library(readxl); library(ggplot2);library(plyr); library(dplyr);library(rio); library(purrr);library(RColorBrewer);library(plyr); library(purrr)


#0. DEFINE LISTS AND PLOTTING GROUPS
#####################################################################################################
mmtlist = list(); aflist=list(); anlist=list() # Define list to store results
date<-Sys.Date() # Define latest model update date

#CREATE OUTCOME GROUPS & LABELS FOR PLOTTING
year<-c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017","2018")

outcome<-c("total", "females","males", "RETI_65.79", "RETII_.80", 
           "females_RETI_65.79","females_RETII_.80", "males_RETI_65.79","males_RETII_.80",
           "cvd","resp" ,"white", "colored", "males_colored","males_white", "females_colored", "females_white", 
           "RETI_65.79_colored","RETI_65.79_white","RETII_.80_colored" ,  "RETII_.80_white", 
           "single", "divorced", "couple", "females_single", "females_divorced", "females_couple", "males_single", "males_divoced", "males_couple",
           "RETI_65.79_divorced","RETII_.80_divorced", "WCII_40.64_divorced", "RETI_65.79_couple",  "RETII_.80_couple", "WCII_40.64_couple",
           "X4to11","less3", "more11", "females_4to11","females_less3","females_more11","males_4to11","males_less3","males_more11")
out<-c("total", "females", "males", "RETI_65.79", "females_RETI_65.79", "males_RETI_65.79","RETII_.80","females_RETII_.80", "males_RETII_.80","colored", "females_colored","males_colored","white", "females_white","males_white",
       "RETI_65.79_colored","RETI_65.79_white","RETII_.80_colored" ,  "RETII_.80_white")
out1<-c("total", "females", "males", "RETI_65.79", "females_RETI_65.79", "males_RETI_65.79","RETII_.80","females_RETII_.80", "males_RETII_.80","colored", "females_colored","males_colored","white", "females_white","males_white")
out2<-c("RETI_65.79_colored","RETI_65.79_white","RETII_.80_colored" ,  "RETII_.80_white")
out3<-c("less3", "females_less3","males_less3","X4to11","females_4to11","males_4to11","more11", "females_more11","males_more11", "RETI_65.79_less3","RETI_65.79_4to11","RETI_65.79_more11",   "RETII_.80_less3","RETII_.80_4to11","RETII_.80_more11")

labels<-c(total="A. All non-external causes (all ages)", females="B. Females (all ages)", males="C. Males (all ages)", 
          RETI_65.79="D. Aged 65-79y", females_RETI_65.79="E.Females (aged 65-79y)", males_RETI_65.79="F.Males (aged 65-79y)", RETII_.80="G. Aged 80+y", 
          females_RETII_.80="H. Females (aged 80+y)", males_RETII_.80="I. Males (aged 80+y)", colored="J. Non-white (all ages)",females_colored= "K. Females non-white (all ages)",
          males_colored="L. Males non white (all ages)", white="M. Whites (all ages)", females_white="N. Females white (all ages)", males_white="O. Males white (all ages)",
          RETI_65.79_colored="P. Non-white (aged 65-79y)",RETI_65.79_white="Q. White (aged 65-79y)",RETII_.80_colored="R. Non-white (aged 80+y)" ,  RETII_.80_white="S. White (aged 80+y)")
labels1<-c(total="A. All non-external causes (all ages)", females="B. Females (all ages)", males="C. Males (all ages)", 
           RETI_65.79="D. Aged 65-79y", females_RETI_65.79="E.Females (aged 65-79y)", males_RETI_65.79="F.Males (aged 65-79y)", RETII_.80="G. Aged 80+y", 
           females_RETII_.80="H. Females (aged 80+y)", males_RETII_.80="I. Males (aged 80+y)", colored="J. Non-white (all ages)",females_colored= "K. Females non-white (all ages)",
           males_colored="L. Males non white (all ages)", white="M. Whites (all ages)", females_white="N. Females white (all ages)", males_white="O. Males white (all ages)")
labels2<-c(RETI_65.79_colored="P. Non-white (aged 65-79y)",RETI_65.79_white="Q. White (aged 65-79y)",RETII_.80_colored="R. Non-white (aged 80+y)" ,  RETII_.80_white="S. White (aged 80+y)")
labels3<-c(less3="A1.Education <3years (all ages)", females_less3="A2.Females with education <3 years (all ages)",males_less3="A3.Males with education <3 years (all ages)", X4to11="A4.Education 4 to 11years (all ages)",females_4to11="A5.Females with education 4 to 11 years",males_4to11="A6.Males with education 4 to 11 years",
           more11="A7.Education >11years(all ages)", females_more11="A8.Females with education >11years (all ages)",males_more11="A9.Males with education >11years(all ages)",RETI_65.79_less3="A10.Education <3years (aged 65-79y)",RETI_65.79_4to11="A11.Education 4 to 11 year (aged 65-79y)", 
           RETI_65.79_more11="A12.Education >11years (aged 65-79y)", RETII_.80_less3="A13.Education <3years (aged 80+y)",  RETII_.80_4to11="A14.Education 4 to 11years (aged 80+y)",RETII_.80_more11="A15.Education >11years (aged 80+y)") 

#DEFINE COLORS FOR PLOTTING
greenL<-"#9FD482"; greenD<-"#308A67"; purpleL<-"#9D77C7"; purpleD<-"#56106D"; yellowL<-"#8b670f";yellowD<-"#da9f11";blueL<-"lightblue3";blueD<-"blue4";redL<-"tomato3"; redD<-"red4"
grayD<-"gray30"; grayL<-"gray70"; transparent<- adjustcolor( "red", alpha.f = 0)


# 1. IMPORT AND FORMAT DATASETS FOR PLOTTING (RUN ONLY ONCE)
##################################################################################################################################################
#1.1. LOAD MMT DATA: 
## OBTAIN LATEST FOLDER DIRECTORY
#define latest upload
date<-"2021-10-26"
input<-paste0(getwd(),"/outputs/tables/", date,"/")

#import data
int<- import_list(paste0(input, "/Main_Model_summary_int", year,".xlsx"),setclass = "tbl")
#int<-int[-8]#

#kEEP DATASETS IN A LIST
for(i in 1:length(int)){  int[[i]]$int<-substr(names(int[i]),23,29)}
int<-do.call(rbind, int)
noint<- read_xlsx(paste0(input, "/Main_Model_summary_noint.xlsx"))
noint$int<-"nointeraction"
data<-rbind.fill(int,noint)
data<-subset(data, data$health!="0")#drop "o"
openxlsx::write.xlsx(data, paste0(input, "summary_all_mmt_af_an.xlsx"), colNames = TRUE,asTable = FALSE, overwrite = T)


#1.2. LOAD cRR DATA
##################
read_sheets <- function(dir_path, file){
  xlsx_file <- paste0(dir_path, file)
  xlsx_file %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel, path = xlsx_file, .id = 'sheet_name') %>% 
    mutate(file_name = file) %>% 
    dplyr::select(file_name, sheet_name, everything())
}
re_file<-"Main_Model_RR_MMTconstr_int*"
df <- list.files(input, re_file) %>% map_df(~ read_sheets(input, .))
safe<-df#create a safety copy 

#Rename interaction
df$year <- substr(df$file_name, 28,31) 
df$year[df$year=="nt_m"] <- "nointeraction"

#merge and save
dfall<-merge(df, data[, c("health","int", "mmt_constr", "mmt_constr_low", "mmt_constr_high")], by.x = c("sheet_name", "year"), by.y=c("health","int"))
openxlsx::write.xlsx(df, paste0(input, "/RRdata_all.xlsx"), colNames = TRUE,asTable = FALSE, overwrite=T)

#Do the same for the no interaction file (ONLY one year as they identical)
re_file<-paste0("Main_Model_RR_INT_MMTconstr_int*")
dfINT <- list.files(input, re_file) %>% map_df(~ read_sheets(input, .))
dfINT$year <- substr(dfINT$file_name, 32,35) 
dfINT<-subset(dfINT, dfINT$year=="2000")
openxlsx::write.xlsx(dfINT, paste0(input, "/RRdata_INT.xlsx"), colNames = TRUE,asTable = FALSE, overwrite=T)



# 2. IMPORT DATAFRAMES FOR PLOTTING 
#####################################################################################################
#1.DEFINE DIRECTORY
figurespath<-paste0(getwd(), "/outputs/figures/", date, "/")

#LOAD FILES
rr<-read_xlsx(paste0( input,"/RRdata_all.xlsx"))
rrINT<-read_xlsx(paste0( input,"/RRdata_INT.xlsx"))
mmt<-read_xlsx(paste0(input,"/summary_all_mmt_af_an.xlsx"))
temp<-read.csv("R:/Projects_SAHSU/ClimateChange_Health/Project/Data/HealthData/Sao Paulo/Mortality Data/Clean Datasets/All_city/mortality_sp_model_exploration.csv")
#temp<-read.csv("TempTS.csv")# Import temperature time series; should be saved on your dir

#FOR EXCOLA WE WILL REMOVE <2012 DUE TO MISSING VALUES >30%
mmt$mmt_constr[(mmt$health=="X4to11" | mmt$health=="less3" | mmt$health=="more11" | mmt$health=="females_4to11" | mmt$health=="females_less3" | mmt$health=="females_more11" | mmt$health=="males_4to11" | mmt$health=="males_less3" | mmt$health=="males_more11" | mmt$health=="RETI_65.79_4to11" | mmt$health=="RETI_65.79_less3" | mmt$health=="RETI_65.79_more11" | mmt$health=="RETII_.80_4to11" | mmt$health=="RETII_.80_less3" | mmt$health=="RETII_.80_more11") & mmt$int<=2011]<-NA
mmt$mmt_constr_low[(mmt$health=="X4to11" | mmt$health=="less3" | mmt$health=="more11" | mmt$health=="females_4to11" | mmt$health=="females_less3" | mmt$health=="females_more11" | mmt$health=="males_4to11" | mmt$health=="males_less3" | mmt$health=="males_more11" | mmt$health=="RETI_65.79_4to11" | mmt$health=="RETI_65.79_less3" | mmt$health=="RETI_65.79_more11" | mmt$health=="RETII_.80_4to11" | mmt$health=="RETII_.80_less3" | mmt$health=="RETII_.80_more11") & mmt$int<=2011]<-NA
mmt$mmt_constr_high[(mmt$health=="X4to11" | mmt$health=="less3" | mmt$health=="more11" | mmt$health=="females_4to11" | mmt$health=="females_less3" | mmt$health=="females_more11" | mmt$health=="males_4to11" | mmt$health=="males_less3" | mmt$health=="males_more11" | mmt$health=="RETI_65.79_4to11" | mmt$health=="RETI_65.79_less3" | mmt$health=="RETI_65.79_more11" | mmt$health=="RETII_.80_4to11" | mmt$health=="RETII_.80_less3" | mmt$health=="RETII_.80_more11") & mmt$int<=2011]<-NA

rr$allRRfit[(rr$sheet_name=="X4to11" | rr$sheet_name=="less3" | rr$sheet_name=="more11" | rr$sheet_name=="females_4to11" | rr$sheet_name=="females_less3" | rr$sheet_name=="females_more11" | rr$sheet_name=="males_4to11" | rr$sheet_name=="males_less3" | rr$sheet_name=="males_more11" | rr$sheet_name=="RETI_65.79_4to11" | rr$sheet_name=="RETI_65.79_less3" | rr$sheet_name=="RETI_65.79_more11" | rr$sheet_name=="RETII_.80_4to11" | rr$sheet_name=="RETII_.80_less3" | rr$sheet_name=="RETII_.80_more11") & rr$year<=2011]<-NA
rr$allRRlow[(rr$sheet_name=="X4to11" | rr$sheet_name=="less3" | rr$sheet_name=="more11" | rr$sheet_name=="females_4to11" | rr$sheet_name=="females_less3" | rr$sheet_name=="females_more11" | rr$sheet_name=="males_4to11" | rr$sheet_name=="males_less3" | rr$sheet_name=="males_more11" | rr$sheet_name=="RETI_65.79_4to11" | rr$sheet_name=="RETI_65.79_less3" | rr$sheet_name=="RETI_65.79_more11" | rr$sheet_name=="RETII_.80_4to11" | rr$sheet_name=="RETII_.80_less3" | rr$sheet_name=="RETII_.80_more11") & rr$year<=2011]<-NA
rr$allRRhigh[(rr$sheet_name=="X4to11" | rr$sheet_name=="less3" | rr$sheet_name=="more11" | rr$sheet_name=="females_4to11" | rr$sheet_name=="females_less3" | rr$sheet_name=="females_more11" | rr$sheet_name=="males_4to11" | rr$sheet_name=="males_less3" | rr$sheet_name=="males_more11" | rr$sheet_name=="RETI_65.79_4to11" | rr$sheet_name=="RETI_65.79_less3" | rr$sheet_name=="RETI_65.79_more11" | rr$sheet_name=="RETII_.80_4to11" | rr$sheet_name=="RETII_.80_less3" | rr$sheet_name=="RETII_.80_more11") & rr$year<=2011]<-NA

#3. PLOTS FOR MANUSCRIPT
########################################################################################################################################################

## TIME SERIES METEOROLOGICAL VARS AND MORTALITY DATA 
############################################################################################################
library(gridExtra)
# Define plotting function :
timeseries<-function(data, x, y, color, YLAB,title){ 
  ggplot(data, aes(x=as.Date(x), y=y))+
    geom_point(size=0.2, col=color)+
    ggtitle(title)+
    xlab("Years")+ ylab(YLAB)+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",date_minor_breaks = "1 month")+
    theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="black"),
          panel.grid.major = element_line(color = "gray70", linetype = "dashed", size=0.3),
          axis.text.y = element_text(color="gray40", size=6, family="sans"),
          title = element_text(color="gray10", size=6, family="sans", face="bold"),
          axis.text.x = element_text(color="gray40", size=6, family="sans",angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=6, face="bold", family="sans"),
          axis.title.y = element_text(color="black", size=6, face="bold",  family="sans"))
}

timeseriesGroups2<-function(data, x,ylim, y1.1, y1.2,y2.1, y2.2, color1.1, color1.2, color2.1,color2.2, title, yaxis){
  ggplot(data, aes(x=as.Date(x)))+
    geom_point(aes(y=y1.1),size=0.2, col=color1.1)+
    geom_point(aes(y=y1.2),size=0.2, col=color1.2)+
    geom_point(aes(y=y2.1),size=0.2, col=color2.1)+
    geom_point(aes(y=y2.2),size=0.2, col=color2.2)+
    ggtitle(title)+ylim(0,ylim)+
    xlab("")+ ylab(yaxis)+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",date_minor_breaks = "1 month")+
    theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="black"),
          panel.grid.major = element_line(color = "gray70", linetype = "dashed", size=0.3),
          axis.text.y = element_text(color="gray40", size=6, family="sans"),
          title = element_text(color="gray10", size=6, family="sans", face="bold"),
          axis.text.x = element_text(color="gray40", size=6, family="sans",angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=6, face="bold", family="sans"),
          axis.title.y = element_text(color="black", size=6, face="bold",  family="sans"))
}


library(seastests)
#FIGURE 2:PLOT MAIN PAPER - Time series of daily mean temperature & mortality (all non-accidental, cvd and resp)
###########
tiff(paste0(figurespath, "Figure2_Temp_Mort_TS.tiff" ), units="in", width=10, height=6, res=900)
plot1<-timeseries(temp, temp$date, temp$temp_mean, redD ,"Temperature (*C)",  "A. Mean daily temperature (*C)") # TEMPERATURE
plot2<-timeseriesGroups2(temp, temp$date, 1000,temp$cvd, temp$resp, temp$nonext, temp$nonext,greenD, yellowD, grayD, transparent,"B.Mortality by cause of death", "") # BY CAUSE OF DEATH
grid.arrange(plot1, plot2, nrow=2, ncol=1)
dev.off()

#Ollech and Webel's combined seasonality test
combined_test(temp$temp_mean, freq = 365)
isSeasonal(temp$temp_mean, test = "combined", freq = 365)

# FIGURE S1: TIME SERIES OF TEMPERATURE AND RELATIVE HUMIDITY
###########
tiff(paste0(figurespath, "FigureS2_timeseriesClimate.tiff" ), units="in", width=5, height=3, res=400)
plot1<-timeseries(temp, temp$date, temp$temp_mean, redD , "Temperature (*C)", "") # TEMPERATURE
plot2<-timeseries(temp, temp$date, temp$HR_mean, blueD,"Relative Humidity (%)", "") # RELATIVE HUMIDITY
grid.arrange(plot1, plot2, nrow=2, ncol=1)
dev.off()

# FIGURE S3: TIME SERIES MORTALITY BY POPULATION GROUP 
#############
tiff(paste0(figurespath, "FigureS3_Mort_By_GroupGeneral.tiff" ), units="in", width=14, height=10, res=800)
plot1<-timeseriesGroups2(temp, temp$date, 1000,temp$cvd, temp$resp, temp$nonext, temp$nonext,greenD, yellowD, grayD, transparent,"A.By cause of death", "") # BY CAUSE OF DEATH
plot2<-timeseriesGroups2(temp, temp$date, 800,temp$females, temp$males, temp$nonext, temp$males, purpleD, purpleL, transparent, transparent, "B. By gender", "Death count") # BY GENDER
plot3<-timeseriesGroups2(temp, temp$date, 800,temp$RETII_.80, temp$RETI_65.79, temp$nonext, temp$males, blueD,blueL, transparent, transparent, "C. By age group", "Death count") # BY AGE GROUP
plot4<-timeseriesGroups2(temp, temp$date, 800, temp$colored,  temp$white, temp$nonext,  temp$white, redL, redD, transparent, transparent, "D. By ethnic group", "Death count") # BY ETHNIC GROUP
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
dev.off()

#FIGURE S4. PLOT MORTALITY BY DOUBLE CATEGORY OF POPULATION GROUP 
#############
#by ethnic group
tiff(paste0(figurespath, "FigureS4_Mort_By_ethnicGroup.tiff" ), units="in", width=8, height=14, res=900)
plot1<-timeseriesGroups2(temp, temp$date, 800, temp$colored,  temp$white, temp$nonext,  temp$white,  redL, redD,transparent, transparent, "By ethnic group \n  Total", "") # OVERALL
plot2<-timeseriesGroups2(temp, temp$date,800 ,temp$females_colored , temp$females_white, temp$females_RETI_65.79, temp$females_RETII_.80,redL, redD, transparent, transparent, " Females", "") # FEMALES
plot3<-timeseriesGroups2(temp, temp$date,800 ,temp$males_colored , temp$males_white,temp$males_RETI_65.79, temp$males_RETII_.80, redL, redD, transparent, transparent, " Males", "") # MALES
plot4<-timeseriesGroups2(temp, temp$date,800 ,temp$RETI_65.79_colored, temp$RETI_65.79_white, temp$females_RETI_65.79, temp$females_RETII_.80,redL, redD, transparent, transparent, " Aged 65-79y", "") # AGED 65-79Y
plot5<-timeseriesGroups2(temp, temp$date,800 ,temp$RETII_.80_colored, temp$RETII_.80_white,temp$males_RETI_65.79, temp$males_RETII_.80, redL, redD, transparent, transparent, " Aged 80+y", "") # AGED 80+ Y
grid.arrange(plot1, plot2, plot3,plot4, plot5, nrow=5, ncol=1)
dev.off()
#by  gender
tiff(paste0(figurespath, "FigureS4_Mort_By_Gender.tiff" ), units="in", width=8, height=14, res=900)
plot1<-timeseriesGroups2(temp, temp$date, 600, temp$females,  temp$males, temp$nonext,  temp$white,  purpleD, purpleL,transparent, transparent, "By gender \n  Total", "") # TOTAL NON-EXTERNAL DEATHS
plot2<-timeseriesGroups2(temp, temp$date,600 ,temp$females_RETI_65.79 , temp$males_RETI_65.79, temp$females_RETI_65.79, temp$females_RETII_.80,purpleD, purpleL, transparent, transparent, " Aged 65-79y", "") # TOTAL NON-EXTERNAL DEATHS
plot3<-timeseriesGroups2(temp, temp$date,600 ,temp$females_RETII_.80 , temp$males_RETII_.80,temp$males_RETI_65.79, temp$males_RETII_.80, purpleD, purpleL, transparent, transparent, " Aged +80y", "") # TOTAL NON-EXTERNAL DEATHS
plot4<-timeseriesGroups2(temp, temp$date,600 ,temp$females_colored, temp$males_colored, temp$females_RETI_65.79, temp$females_RETII_.80,purpleD, purpleL, transparent, transparent, " Non-whites", "") # TOTAL NON-EXTERNAL DEATHS
plot5<-timeseriesGroups2(temp, temp$date,600 ,temp$females_white, temp$males_white,temp$males_RETI_65.79, temp$males_RETII_.80, purpleD, purpleL, transparent, transparent, " Whites", "") # TOTAL NON-EXTERNAL DEATHS
grid.arrange(plot1, plot2, plot3,plot4, plot5, nrow=5, ncol=1)
dev.off()
#by age 
tiff(paste0(figurespath, "FigureS4_Mort_By_Age.tiff" ), units="in", width=8, height=14, res=900)
plot1<-timeseriesGroups2(temp, temp$date,450, temp$RETI_65.79,  temp$RETII_.80, temp$nonext,  temp$white,  blueD,blueL,transparent, transparent, "By age \n  Total", "") # TOTAL NON-EXTERNAL DEATHS
plot2<-timeseriesGroups2(temp, temp$date,450 ,temp$females_RETI_65.79 , temp$females_RETII_.80, temp$females_RETI_65.79, temp$females_RETII_.80,blueD,blueL, transparent, transparent, " Females", "") # TOTAL NON-EXTERNAL DEATHS
plot3<-timeseriesGroups2(temp, temp$date,450 ,temp$males_RETI_65.79 , temp$males_RETII_.80,temp$males_RETI_65.79, temp$males_RETII_.80, blueD,blueL, transparent, transparent, " Males", "") # TOTAL NON-EXTERNAL DEATHS
plot4<-timeseriesGroups2(temp, temp$date,450 ,temp$RETI_65.79_colored, temp$RETII_.80_colored, temp$females_RETI_65.79, temp$females_RETII_.80,blueD,blueL, transparent, transparent, " Non-whites", "") # TOTAL NON-EXTERNAL DEATHS
plot5<-timeseriesGroups2(temp, temp$date,450 ,temp$RETI_65.79_white, temp$RETII_.80_white,temp$males_RETI_65.79, temp$males_RETII_.80,blueD,blueL, transparent, transparent, " Whites", "") # TOTAL NON-EXTERNAL DEATHS
grid.arrange(plot1, plot2, plot3,plot4, plot5, nrow=5, ncol=1)
dev.off()

# TEMPERATURE DISTRIBUTION PLOTS
############################################################################################################

#FIGURE 3. TEMPERATURE DISTRIBUTION 
############
##DEFINE PERIODS TO PLOT
temp$Period<-NA; temp$mean<-NA
temp$Period[temp$year==2002]<-"2002"
temp$Period[temp$year==2009]<-"2009"
temp$Period[temp$year==2016]<-"2016"
##ESTIMATE MEAN FOR EACH PERIOD
temp$mean[temp$Period=="2002"]<-mean(temp$temp_mean[temp$Period=="2002"], na.rm=T)
temp$mean[temp$Period=="2009"]<-mean(temp$temp_mean[temp$Period=="2009"], na.rm=T)
temp$mean[temp$Period=="2016"]<-mean(temp$temp_mean[temp$Period=="2016"], na.rm=T)

#ESTIMATE P-VALUE FOR DIFFERENCE IN DISTRIBUTION (to add to plot as text)
#Statistical test for changes: one-sided Wilcoxon-Mann-Whitney test. Refer to:https://bookdown.org/egarpor/NP-UC3M/nptests-comp.html
p2009<-wilcox.test(x=temp$temp_mean[temp$year==2009], y=temp$temp_mean[temp$year==2002], alternative = "less") # H1: P[X >= Y] > 0.5
p2016<-wilcox.test(x=temp$temp_mean[temp$year==2016], y=temp$temp_mean[temp$year==2002], alternative = "less") # H1: P[X >= Y] > 0.5
text42009<-paste0("P-value [2009:2002] = ", round(as.numeric(p2009[3]), digits=5))
text42016<-paste0("P-value [2016:2002] = ", round(as.numeric(p2016[3]), digits=5))

#PLOT
library(scales)
tiff(paste0(figurespath, "Figure3_TempDist.tiff" ), units="in", width=5, height=4, res=900)
ggplot(subset(temp, !is.na(temp$Period)), aes(x=temp_mean, color=Period, fill=Period))+geom_density(position = 'identity',alpha=0.4)+
  xlab("Daily mean temperature (*C)")+ylab("Frequency (%)")+
  scale_fill_manual(values=c(purpleD, greenD, yellowD))+
  scale_color_manual(values=c(purpleD, greenD, yellowD))+
  geom_vline(data=temp, aes(xintercept=mean, color=Period),linetype="dashed", size=0.5)+
  scale_x_continuous(breaks=seq(5,30, by=5),limits=c(5, 30))+
  scale_y_continuous(labels = percent_format(),breaks=seq(0,0.209, by=0.05),limits=c(0, 0.160)) +
  theme_classic()+
  annotate("text", x = 5, y = 0.14, label = text42009, color="gray42",hjust = 0, size=3)+
  annotate("text", x = 5, y = 0.13, label = text42016, color="gray42",hjust = 0, size=3)+
  theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
        axis.text.y = element_text(color="gray40", size=7, family="sans"),
        title = element_text(color="gray10", size=9, family="sans", face="bold"),
        axis.text.x = element_text(color="gray40", size=7, family="sans" ),
        axis.title.x = element_text(color="black", size=9, face="bold", family="sans"),
        axis.title.y = element_text(color="black", size=9, face="bold",  family="sans"))
dev.off()


#FIGURE S2. TEMP DISTRIBUTION OVERALL, HOTTEST & COLDEST MONTHS 
############
#DEFINE PLOTTING FUNCTION
distplot<-function(dataset){
ggplot(subset(dataset, !is.na(Period)), aes(x=temp_mean, color=Period, fill=Period))+geom_density(position = 'identity',alpha=0.4)+
  xlab("Daily mean temperature (*C)")+ylab("Frequency (%)")+
  scale_fill_manual(values=c(purpleD, greenD, yellowD))+
  scale_color_manual(values=c(purpleD, greenD, yellowD))+
  #geom_vline(data=hot, aes(xintercept=mean, color=Period),linetype="dashed", size=0.5)+
  scale_x_continuous(breaks=seq(5,30, by=5),limits=c(5, 30))+
  scale_y_continuous(labels = percent_format(),breaks=seq(0,0.250, by=0.05),limits=c(0, 0.250)) +
  theme_classic()+
  theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
        axis.text.y = element_text(color="gray40", size=7, family="sans"),
        title = element_text(color="gray10", size=9, family="sans", face="bold"),
        axis.text.x = element_text(color="gray40", size=7, family="sans" ),
        axis.title.x = element_text(color="black", size=9, face="bold", family="sans"),
        axis.title.y = element_text(color="black", size=9, face="bold",  family="sans"))
  }

# 4 AVERAGE HOTTEST MONTHS 
library(lubridate)
temp$month<-month(temp$date)
sort((1:12)[order(tapply(temp$temp_mean,temp$month,mean,na.rm=T))][9:12])
hot <- subset(temp,month %in% 1:3 | month==12)
hot$mean[hot$Period=="2002"]<-mean(hot$temp_mean[hot$Period=="2002"], na.rm=T)
hot$mean[hot$Period=="2009"]<-mean(hot$temp_mean[hot$Period=="2009"], na.rm=T)
hot$mean[hot$Period=="2016"]<-mean(hot$temp_mean[hot$Period=="2016"], na.rm=T)

tiff(paste0(figurespath, "/FigureS2B_TempDist_hot.tiff" ), units="in", width=5, height=4, res=900)
distplot(hot)
dev.off()

# 4 AVERAGE COLDEST MONTHS 
temp$month<-month(temp$date)
sort((1:12)[order(-tapply(temp$temp_mean,temp$month,mean,na.rm=T))][9:12])
cold <- subset(temp,month %in% 5:8)
cold$mean[cold$Period==1]<-mean(cold$temp_mean[cold$Period==1], na.rm=T)
cold$mean[cold$Period==2]<-mean(cold$temp_mean[cold$Period==2], na.rm=T)
cold$mean[cold$Period==3]<-mean(cold$temp_mean[cold$Period==3], na.rm=T)

tiff(paste0(figurespath, "/FigureS2C_TempDist_cold.tiff" ), units="in", width=5, height=4, res=900)
distplot(cold)
dev.off()

# MINIMUM MORTALITY TEMPERATURE (MMT) PLOTS
##################################################################################################################################################
# DEFINE PLOTTING FUNCTIONS: 3 YEARS MMT PLOT with point&wiskers + bars for difference 
MMT_plot1<-function(data, x, y, ribbonmin, ribbonmax, ylimtop, ylimbottom, col1, col2, lbl){
  ggplot(data, aes(x=int, col=y > value)) + 
    geom_linerange(aes(ymin=value, ymax=y), size=10, alpha=0.9) +
    geom_errorbar(aes(ymin=ribbonmin, ymax=ribbonmax), width=0.25, colour="gray30") +
    geom_point(aes(y=y), size=1,col="gray25")+
    geom_hline(aes(yintercept=value), col="gray70")+
    scale_y_continuous(breaks=seq(15,30, by=2),limits=c(15, 30))+
    scale_color_manual(values = c(col1, col2),labels = c("Reduced MMT", "Increased MMT"), "Absolute difference \n(ref. 2000)")+
    xlab("Years") + ylab("Minimum Mortality Temperature (°C)")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"), panel.grid.major = element_line(linetype = "dashed",colour = "gray80", size=0.4),
          axis.title.x = element_text(color="black", size=8, face="bold", family="sans"),
          axis.title.y = element_text(color="black", size=8, face="bold",  family="sans"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=7),axis.text.y = element_text(size=7),
          legend.text=element_text(size=6),legend.title=element_text(size=7, face="bold"), legend.position="bottom",
          strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0, family="sans"), 
          strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
    facet_wrap(~health, ncol=3,  labeller=labeller(health=lbl))
}

# FIGURE 6. MMT BY POPULATION SUBGROUP 
###########
#PAGE1
data<-subset(mmt, mmt$health %in% out1 & mmt$int!="nointeraction")
data$health=factor(data$health, levels=out1)
intercept<-data %>%
  group_by(health) %>%
  summarise(value= first(na.omit(mmt_constr))) # set the intercept to be the first value in series, excluding NAs
data<-merge(data,intercept, by=c("health"))
tiff(paste0(figurespath, "/Figure6_MMT_page1.tiff" ),units="in", res=800, height = 10, width = 8)
print(MMT_plot1(data, as.numeric(data$int),data$mmt_constr, data$mmt_constr_low, data$mmt_constr_high, round(max(data$mmt_constr_high))+2,round(min(data$mmt_constr_low))-2 , yellowD,greenD, labels1))
dev.off()

# PAGE 2
data<-subset(mmt, mmt$health %in% out2 & mmt$int!="nointeraction")
data$health=factor(data$health, levels=out2)
intercept<-data %>%
  group_by(health) %>%
  summarise(value= first(na.omit(mmt_constr)))
data<-merge(data,intercept, by=c("health"))
tiff(paste0(figurespath, "/Figure6_MMT_page2.tiff" ),units="in", res=800, height = 4, width = 8)
print(MMT_plot1(data, as.numeric(data$int),data$mmt_constr, data$mmt_constr_low, data$mmt_constr_high, round(max(data$mmt_constr_high))+2,round(min(data$mmt_constr_low))-2, yellowD,greenD, labels2))
dev.off()

## FIGURE S11. ASOCIATION BETWEEN MMT & ANNUAL MEAN TEMPERATURE (AMT)
###############
#1. CALCUALTE AMT
library(doBy)
df1<-summaryBy(temp_mean ~ year, data=temp, FUN=function(x){c(mean=mean(x))}) #Calculate annual temperature mean
df2<-subset(mmt, select=c(int, health, mmt_constr)) #obtain year, outcome group and mmt value from orignal mmt dataset
df<-merge(df2, df1, by.x=c("int"), by.y=("year")) # merge datasets 
plot<-list() # empty list to save plots
for(i in 1:length(out)){
  pl<-subset(df, health==outcome[i])
  plot[[i]]<-ggplot(pl, aes(x=temp_mean.mean, y=mmt_constr))+
    geom_point()+ xlab("Mean Annual Temperature (°C)")+ ylab("MMT(°C)")+ggtitle(paste0(out[i]))+
    ggtitle(paste0(labels[i]))+xlab("")+ylab("")+ylim(19,24)+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"), panel.grid.major = element_line(linetype = "dashed",colour = "gray80", size=0.4),
        axis.title.x = element_text(color="black", size=8, face="bold", family="sans"),
        axis.title.y = element_text(color="black", size=8, face="bold",  family="sans"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=7),axis.text.y = element_text(size=7),
        legend.text=element_text(size=6),legend.title=element_text(size=7, face="bold"), legend.position="bottom",
        strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0, family="sans"), 
        strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
  theme_bw()
}
tiff(paste0(figurespath, "/FigureS11_MMT_MeanTemp.tiff" ), units="in", width=10, height=10, res=900)
do.call("grid.arrange", c(plot, ncol=floor(sqrt(length(plot)))))
dev.off()



#cRR curves
#############################################################################
#Calculate percentiles of temperature distribution
perc <- quantile(temp$temp_mean,c(0,0.01, 0.025,0.10, 0.5,0.90, 0.975,0.99,1), na.rm=TRUE); perc

#DEFINE FUNCTION FOR PLOTTING cRR
library(RColorBrewer)
plotRR<-function(data, x, y, cihigh, cilow,group, mycolors, lbl){
  print(ggplot(data, aes(x=as.numeric(x), y=y, group=group, col=as.factor(group), fill=as.factor(group)))+
          geom_ribbon(aes(ymin = cilow, ymax = cihigh, group=group,col=as.factor(group), fill=as.factor(group)), alpha=0.2, colour = NA, show.legend = T) +
          geom_line(size=0.8)+
          geom_vline(mapping=aes(xintercept=mmt_constr, color=as.factor(group)),data=df, show.legend = F, size=0.3,linetype="dashed")+
          geom_vline(xintercept=c(perc[2],perc[3], perc[7], perc[8]), color="gray60", linetype="dotted") + #add dotted lines for 10tha dn 90th percentiles
          xlab("Temperature (°C)")+ylab("Cumulative Relative Risk")+
          scale_x_continuous(breaks=round(seq(round(min(as.numeric(x))), round(max(as.numeric(x))+2), by=1),1))+
          coord_cartesian(ylim=c(0,2.2))+ labs(col = "year")+
          geom_hline(yintercept=1, linetype="solid", color = "grey50")+ # add baseline df=1
          scale_color_manual("Centering year",values=mycolors)+
          scale_fill_manual("Centering year",values=mycolors)+
          theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
                axis.text.y = element_text(color="gray40", size=5, family="sans"),
                title = element_text(color="gray10", size=7, family="sans", face="bold"),
                axis.text.x = element_text(color="gray40", size=5, family="sans"),
                axis.title.x = element_text(color="black", size=7, face="bold", family="sans"),
                axis.title.y = element_text(color="black", size=7, face="bold",  family="sans"),
                legend.text=element_text(size=5),legend.title=element_text(size=7, face="bold"), legend.position="bottom",
                strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0, family="sans"), 
                strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
        facet_wrap(~sheet_name, ncol=3,  labeller=labeller(sheet_name=lbl))+guides(col=guide_legend(ncol=10)))
}
#DEFINE COLORS
mycol<- c(yellowD, greenD, purpleD)

#CREATE DATASET FOR PLOTS
rr$year<-as.numeric(rr$year)#make sure year is numeric
dfall<-merge(rr, mmt, by.x = c("sheet_name", "year"), by.y=c("health","int"))

#FIGURE4: cRR FOR CVD AND RESP
###########
cause<-c("total", "cvd", "resp")
labelsCause<-c(total="A. All non-external causes", cvd="B. Cardiovascular Diseases (CVD)", resp="C. Respiratory diseases")
df<-subset(dfall, sheet_name %in% cause & (year==2002| year==2009 | year==2016))#subset data
df$sheet_name=factor(df$sheet_name, levels=cause)
tiff(paste0(figurespath, "/Figure4_cRR_cause.tiff" ),units="in", res=800, height = 3, width = 8)
plotRR(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year, mycol, labelsCause)#A) No confidence intervals
dev.off()


#FIGURE 5 (no 95%CI) and FIGURE S9 (with 95%CI)
###################
##PAGE 1
df<-subset(dfall, sheet_name %in% out1 & (year==2002| year==2009 | year==2016))#subset data
df$sheet_name=factor(df$sheet_name, levels=out1)
tiff(paste0(figurespath, "/Figure5_cRR_groups_page1.tiff" ), units="in", res=800, height = 10, width = 8)
plotRR(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year, mycol, labels1)#A) No confidence intervals
dev.off()
tiff(paste0(figurespath, "/FigureS6_cRR_groups_page1.tiff" ), units="in", res=800, height = 10, width = 8)
plotRR(df, df$index, df$allRRfit,df$allRRhigh, df$allRRlow , df$year, mycol, labels1) # B) With confidence intervals
dev.off()

##PAGE 2
df<-subset(dfall, sheet_name %in% out2 & (year==2002| year==2009 | year==2016))#subset data
df$sheet_name=factor(df$sheet_name, levels=out2)
tiff(paste0(figurespath, "/Figure5_cRR_groups_page2.tiff" ),units="in", res=800, height = 4, width = 8)
plotRR(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year, mycol, labels2)#A) No confidence intervals
dev.off()
tiff(paste0(figurespath, "/FigureS6_cRR_groups_page2.tiff" ),units="in", res=800, height = 4, width = 8)
plotRR(df, df$index, df$allRRfit,df$allRRhigh, df$allRRlow , df$year, mycol, labels2) # B) With confidence intervals
dev.off()

##FIGURE S9. CRR FOR Education (SUPPLEMENT)
#############
df<-subset(dfall, sheet_name %in% out3 & (year==2012| year==2015 | year==2018))#subset data
df$sheet_name=factor(df$sheet_name, levels=out3)
tiff(paste0(figurespath, "/FigureS9_cRR_Education.tiff" ),units="in", res=800, height = 10, width = 8)
plotRR(df, df$index, df$allRRfit,df$allRRhigh, df$allRRlow , df$year, mycol, labels3) # B) With confidence intervals
dev.off()


##FIGURE S7. CRR BY GROUP ALL YEARS + NO 95%CI
#############
#PAGE 1
df<-subset(dfall, sheet_name %in% out1 )#subset data
df$sheet_name=factor(df$sheet_name, levels=out1)
tiff(paste0(figurespath, "/FigureS7_cRR_groups_all_noCI_page1.tiff" ), units="in", res=800, height = 10, width = 8)
plotRR(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year, colYP18(19), labels1)#A) No confidence intervals
dev.off()

#PAGE 2
df<-subset(dfall, sheet_name %in% out2 )#subset data
df$sheet_name=factor(df$sheet_name, levels=out2)
tiff(paste0(figurespath, "/FigureS7_cRR_groups_all_noCI_page2.tiff" ),units="in", res=800, height = 4, width = 8)
plotRR(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year, colYP18(19), labels2)#A) No confidence intervals
dev.off()

#FIGURE S8. cRR curves for INTERACTION
############
dfINT$mmt_constr<-as.numeric(NA);dfINT$index<-as.numeric(dfINT$index); dfINT$year<-as.numeric(dfINT$year)
#page 1
df<-subset(dfINT, sheet_name %in% out1)
df<-subset(df, year==2000)#subset data
df$sheet_name=factor(df$sheet_name, levels=out1)
tiff(paste0(figurespath, "/Figures8_cRRlines_Interaction_page1.tiff" ), units="in", res=800, height = 10, width = 8)
plotRR(df, df$index, df$allRRfit, df$allRRhigh, df$allRRlow, 1, grayD, labels1)#A) No confidence intervals
dev.off()

##PAGE 2
df<-subset(dfINT, sheet_name %in% out2 )#subset data
df<-subset(df, year==2000)#subset data
df$sheet_name=factor(df$sheet_name, levels=out2)
tiff(paste0(figurespath, "/FigureS8_cRRlines_Interaction_page2.tiff" ),units="in", res=800, height = 4, width = 8)
plotRR(df,  df$index, df$allRRfit, df$allRRhigh, df$allRRlow, 1, grayD, labels2)#A) No confidence intervals
dev.off()




##############################################################################
## FIGURE S12. SENSITIVITY ANALYSIS 
##############################################################################
#define latest upload
input<-paste0(getwd(), "/sa/tables/", date, "/")

noint<- read_xlsx(paste0(input, "/noint_model_summary.xlsx"))
noint$int<-"nointeraction"
mmt<-noint

openxlsx::write.xlsx(mmt, paste0(input, "/summary_all_mmt_af_an.xlsx"), colNames = TRUE,asTable = FALSE, overwrite = T)


#2. LOAD cRR DATA
##################
dir_path<-paste0(input,"/")
re_file<-"int*_model_RR_MMTconstr"
df2 <- list.files(input, re_file) %>% map_df(~ read_sheets(input, .))
safe<-df
#Rename interaction
df$year <- substr(df$file_name, 4,7) 
df$year[df$year=="nt_m"] <- "nointeraction"

#merge and save
mmt$SA<-as.character(mmt$SA)
rr<-merge(df, mmt[, c("SA","int", "mmt_constr", "mmt_constr_low", "mmt_constr_high")], by.x = c("sheet_name", "year"), by.y=c("SA","int"))
openxlsx::write.xlsx(rr, paste0(input, "/RRdata_all.xlsx"), colNames = TRUE,asTable = FALSE, overwrite=T)

#1.DEFINE DIRECTORY
rr<-read_xlsx(paste0(input, "/RRdata_all.xlsx"))
mmt<-read_xlsx(paste0(input, "/summary_all_mmt_af_an.xlsx"))
date<-"2021-10-26"
figurespath<-paste0(getwd(), "/sa/figures/", date,"/")


#DEFIN FUNCTION
SAPlot<-function(dataset, nameList,title,  filename){
tiff(paste0(figurespath, "/FigureS12_SA_mmt", filename, ".tiff" ), units="in", res=800, height =4, width = 6)
 print(ggplot(dataset, aes(int, mmt_constr, fill=factor(SA)))+
    geom_bar(position="dodge",stat="identity")+
    ylab("Minimum Mortality Temperature (*C)")+xlab("")+
    scale_fill_brewer(name="Basis functions", labels=nameList)+
    theme_bw()+labs(fill = "Basis functions")+
    ggtitle(title)+
    theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
          axis.text.y = element_text(color="gray40", size=7, family="sans"),
          title = element_text(color="gray10", size=9, family="sans", face="bold"),
          axis.text.x = element_text(color="gray40", size=7, family="sans"),
          axis.title.x = element_text(color="black", size=9, face="bold", family="sans"),
          axis.title.y = element_text(color="black", size=9, face="bold",  family="sans"),
          legend.text=element_text(size=8),legend.title=element_text(size=8, face="bold"),
          strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0, family="sans"), 
          strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"),
          legend.position = "bottom", legend.direction = "horizontal"))
dev.off()
}

#A. EXPOSURE BASIS + LAG BASIS 
dataset<-subset(mmt, SA=="13"| SA=="1" | SA=="8")
SAPlot(dataset, c("Main Model (Cubic B-spline)", "Quadratic B-spline", "Lag basis 2knots"),"A. Exposure and lag basis","basis" )

#B. LAG BASIS
dataset<-subset(mmt,SA=="13"| SA=="3" | SA=="4"| SA=="5" | SA=="6"| SA=="7" )
SAPlot(dataset, c("Main Model (1knot 75thperc)", "1 knot (90th perc)","2 knot (50th/90th perc)", "3 knot (25th/50th/90th perc)","4 knot (25th/50th/90th/95th perc)",
                  "3 knot(10th/75th/90thperc"),"B. LAG BASIS KNOT PLACEMENT", "knots")

#C. LAG MAX
dataset<-subset(mmt,SA=="13"| SA=="9" | SA=="10")
SAPlot(dataset, c("Main Model (21days)", "Lag max 15days", "Lag max 24 days"),"C. MAXIMUM LAG", "maxlag")

#D. DF SEASONALITY
dataset<-subset(mmt,SA=="13"| SA=="11"| SA=="2" | SA=="12")
SAPlot(dataset, c("Main Model (10df/year)", "7df/year", "8df/year", "9df/year"),"D. SEASONALITY DFs PER YEAR", "seasdf")

#E. RELATIVE HUMIDITY 
dataset<-subset(mmt,SA=="13"| SA=="15" | SA=="16"| SA=="17" | SA=="18")
SAPlot(dataset, c("Main Model", "RH0 3df", "RH_Lag01 3df", "RH_Lag02 3df", "RH0 4df"),"E. RELATIVE HUMIDITY", "rh")

#F. AIR POLLTUION
dataset<-subset(mmt,SA=="13"| SA=="19" | SA=="20"| SA=="21" |SA=="22")
SAPlot(dataset, c("Main Model", "PM10_Lag01", "PM10_Lag02", "PM10_Lag03", "PM10_Lag02 & RH_Lag02"),"F. AIR POLLUTION (PM10)", "pm10")




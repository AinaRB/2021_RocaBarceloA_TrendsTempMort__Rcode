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
# This script contains the code to generate the images in the main manuscript and those in the           
# Supplementary Material except for the plots for the sensitivity analyses, not included here.
#####################################################################################################


#SET THE ENVIRONMENT
rm(list=ls()) #clear the slate
.libPaths("directory/for/your/rversion/library") # Define location of your libraries directory
mainDir<-getwd() # Check directory is correct; otherwise set using 'setwd()'

#LOAD PACKAGES
library(readxl); library(ggplot2);library(plyr); library(dplyr);library(rio); library(purrr);library(RColorBrewer);library(doBy);
library(viridis);library(plyr); library(purrr);library(rio);library(gridExtra);library(seastests);library(ggridges)library(ggpmisc)

#LOAD PLOTTING FUNCTIONS
source("functions.R")

#DEFINE COLORS FOR PLOTTING
greenL<-"#9FD482"; greenD<-"#308A67"; greenDD<-"#184533"; purpleL<-"#9D77C7"; purpleD<-"#56106D"; yellowL<-"#8b670f";yellowD<-"#da9f11";blueL<-"lightblue3";blueD<-"blue4";redL<-"tomato3"; redD<-"red4"
grayD<-"gray30"; grayL<-"gray70"; transparent<- adjustcolor( "red", alpha.f = 0)

#DEFINE DIRECTORIES
date<-Sys.Date()#define date of interest for dataset upload
dateFig<-file.path(mainDir, "outputs/figures/",date, "/") # Desired end dir for figures
dateTab<-file.path(mainDir, "outputs/tables/",date, "/") # Desired end dir for tables
sapply(c(dateFig, dateTab), function(x) if (!dir.exists(x)) dir.create(x)) # Creates the above dir if they don't exists


#0. DEFINE LISTS AND PLOTTING GROUPS
#####################################################################################################
mmtlist = list(); aflist=list(); anlist=list() # Define list to store results

#CREATE OUTCOME GROUPS& LABELS FOR PLOTTING
year<-c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017","2018")

out<-c("total", "females", "males", "RETI_65.79", "females_RETI_65.79", "males_RETI_65.79","RETII_.80","females_RETII_.80", "males_RETII_.80","colored", "females_colored","males_colored","white", "females_white","males_white",
       "RETI_65.79_colored","RETI_65.79_white","RETII_.80_colored" ,  "RETII_.80_white")
out1<-c("total", "females", "males", "RETI_65.79", "females_RETI_65.79", "males_RETI_65.79","RETII_.80","females_RETII_.80", "males_RETII_.80","colored", "females_colored","males_colored","white", "females_white","males_white")
out2<-c("RETI_65.79_colored","RETI_65.79_white","RETII_.80_colored" ,  "RETII_.80_white")
#out3<-c("less3", "females_less3","males_less3","X4to11","females_4to11","males_4to11","more11", "females_more11","males_more11", "RETI_65.79_less3","RETI_65.79_4to11","RETI_65.79_more11",   "RETII_.80_less3","RETII_.80_4to11","RETII_.80_more11")
cause<-c("cvd", "resp")

labelsCause<-c( cvd="I. Cardiovascular diseases", resp="II. Respiratory diseases")
labels<-c(total="a. All non-external causes (all ages)", females="b. Females (all ages)", males="c. Males (all ages)", 
          RETI_65.79="d. Aged 65-79y", females_RETI_65.79="e.Females (aged 65-79y)", males_RETI_65.79="f.Males (aged 65-79y)", RETII_.80="g. Aged 80+y", 
          females_RETII_.80="h. Females (aged 80+y)", males_RETII_.80="i. Males (aged 80+y)", colored="j. Non-white (all ages)",females_colored= "k. Females non-white (all ages)",
          males_colored="l. Males Non-White (all ages)", white="m. Whites (all ages)", females_white="n. Females White (all ages)", males_white="o. Males White (all ages)",
          RETI_65.79_colored="p. Non-white (aged 65-79y)",RETI_65.79_white="q. White (aged 65-79y)",RETII_.80_colored="r. Non-white (aged 80+y)" ,  RETII_.80_white="s. White (aged 80+y)")
labels1<-c(total="A. All non-external causes (all ages)", females="B. Females (all ages)", males="C. Males (all ages)", 
           RETI_65.79="D. Aged 65-79y", females_RETI_65.79="E.Females (aged 65-79y)", males_RETI_65.79="F.Males (aged 65-79y)", RETII_.80="G. Aged 80+y", 
           females_RETII_.80="H. Females (aged 80+y)", males_RETII_.80="I. Males (aged 80+y)", colored="J. Non-white (all ages)",females_colored= "K. Females non-white (all ages)",
           males_colored="L. Males Non-White (all ages)", white="M. Whites (all ages)", females_white="N. Females White (all ages)", males_white="O. Males White (all ages)")
labels2<-c(RETI_65.79_colored="P. Non-white (aged 65-79y)",RETI_65.79_white="Q. White (aged 65-79y)",RETII_.80_colored="R. Non-white (aged 80+y)" ,  RETII_.80_white="S. White (aged 80+y)")
#labels3<-c(less3="A1.Education <3years (all ages)", females_less3="A2.Females with education <3 years (all ages)",males_less3="A3.Males with education <3 years (all ages)", X4to11="A4.Education 4 to 11years (all ages)",females_4to11="A5.Females with education 4 to 11 years",males_4to11="A6.Males with education 4 to 11 years",
#           more11="A7.Education >11years(all ages)", females_more11="A8.Females with education >11years (all ages)",males_more11="A9.Males with education >11years(all ages)",RETI_65.79_less3="A10.Education <3years (aged 65-79y)",RETI_65.79_4to11="A11.Education 4 to 11 year (aged 65-79y)", 
#           RETI_65.79_more11="A12.Education >11years (aged 65-79y)", RETII_.80_less3="A13.Education <3years (aged 80+y)",  RETII_.80_4to11="A14.Education 4 to 11years (aged 80+y)",RETII_.80_more11="A15.Education >11years (aged 80+y)") 

#DEFINE COLORS FOR PLOTTING
greenL<-"#9FD482"; greenD<-"#308A67"; greenDD<-"#184533"; purpleL<-"#9D77C7"; purpleD<-"#56106D"; yellowL<-"#8b670f";yellowD<-"#da9f11";blueL<-"lightblue3";blueD<-"blue4";redL<-"tomato3"; redD<-"red4"
grayD<-"gray30"; grayL<-"gray70"; transparent<- adjustcolor( "red", alpha.f = 0)


# 1. IMPORT AND FORMAT DATASETS FOR PLOTTING (RUN ONLY ONCE)
##################################################################################################################################################
#1.1. IMPORT DATA
int<- import_list(paste0(dateTab, "/Main_Model_summary_int", year, ".xlsx"),setclass = "tbl")

#kEEP DATASETS IN A LIST
for(i in 1:length(int)){  int[[i]]$int<-substr(names(int[i]),23,29)}
int<-do.call(rbind, int)
noint<- read_xlsx(paste0(dateTab,"/Main_Model_summary_noint.xlsx"))
noint$int<-"nointeraction"
data<-rbind.fill(int,noint)
data<-subset(data, health!="0") #drop "o"
openxlsx::write.xlsx(data, paste0(dateTab, "summary_all_mmt_af_an.xlsx"), colNames = TRUE,asTable = FALSE, overwrite = T)


#1.2. LOAD cRR DATA
##################
re_file<-"Main_Model_RR_MMTconstr_*"
df <- list.files(dateTab, re_file) %>% map_df(~ read_sheets(dateTab, .)) #read_sheets() function specified in 'plotting_functions.R"

#Rename interaction
df$year <- substr(df$file_name, 28,31) 
df$year[df$year=="nt.x"] <- "nointeraction"

#merge and save
dfall<-merge(df, data[, c("health","int", "mmt_constr", "mmt_constr_low", "mmt_constr_high")], by.x = c("sheet_name", "year"), by.y=c("health","int"))
openxlsx::write.xlsx(df, paste0(dateTab, "/RRdata_all.xlsx"), colNames = TRUE,asTable = FALSE, overwrite=T)

#Do the same for the interaction file 
re_file<-paste0("Main_Model_RR_INT_MMTconstr_int*")
dfINT <- list.files(dateTab, re_file) %>% map_df(~ read_sheets(dateTab, .))
dfINT$year <- substr(dfINT$file_name, 32,35) 
dfINT<-subset(dfINT, dfINT$year=="2000")#Only keep one year as they identical
dfINT<-merge(dfINT, noint, by.x = "sheet_name", by.y="health") #Add MMT from non interaction model
openxlsx::write.xlsx(dfINT, paste0(dateTab, "/RRdata_INT.xlsx"), colNames = TRUE,asTable = FALSE, overwrite=T)


# 2. LOAD DATAFRAMES FOR PLOTTING 
#####################################################################################################
#LOAD FILES
rr<-read_xlsx(paste0( dateTab,"/RRdata_all.xlsx"))
rrINT<-read_xlsx(paste0( dateTab,"/RRdata_INT.xlsx"))
mmt<-read_xlsx(paste0(dateTab,"/summary_all_mmt_af_an.xlsx"))
temp<-read.csv(paste0(mainDir,"syntheticDF.csv"))# Import temperature time series from syntehtic dataset
dfINT<-read_xlsx(paste0(dateTab, "/RRdata_INT.xlsx"))

#3. PLOTS FOR MANUSCRIPT
########################################################################################################################################################
#METEROLOGICAL VARIABLES TRENDS
##Figure 1a:
tiff(paste0(dateFig, "FigureTempDist_violinplot.tiff" ), units="in", width=14, height=8, res=900,compression = "lzw")
ggplot(temp, aes(x=as.factor(year), y=temp_mean)) + 
  geom_violin(trim=F, fill="gray" ,col="gray50")+
  geom_boxplot(width=0.1, fill="white", col="gray50")+
  labs(title="",x="", y = "Temperature (°C)")+
  geom_hline(yintercept=mean(temp$temp_mean), linetype='dashed', size=0.7, col = 'red')+
  scale_fill_manual("#E7B800") + theme_classic()
dev.off()

##Figure 1b:
temp %>%  dplyr::group_by(year) %>%
  summarize(quantile = scales::percent(c(0.01, 0.5, 0.99)),
            temp = quantile(temp_mean, c(0.01, 0.5, 0.99))) -> perc #calculate percentiles
temp %>%  dplyr::group_by(year) %>%  summarize(quantile = "mean", temp = mean(temp_mean)) -> mean #calculate mean
annual<-rbind(perc, mean); annual<-subset(annual, quantile!="50%") #create datset:percentiles + mean
tiff(paste0(dateFig, "Fig_1b.tiff" ), width = 8, height = 4, units = 'in', res = 800,compression = "lzw")
ggplot(annual, aes(x=year, y=temp, group=quantile,shape=quantile, color=quantile))+
  geom_point(size=3)+geom_line()+
  ylab("Temperature (°C)")+xlab("Year")+ 
  scale_y_continuous(breaks=seq(round(min(annual$temp)-2),round(max(annual$temp)+2), by=5),limits=c(min(annual$temp)-2,max(annual$temp)+2))+
  scale_x_continuous(breaks=seq(2000,2018, by=1),limits=c(2000,2018))+
  scale_color_manual("Temperature percentiles:", values=c("blue", "red","gray40"), labels=c("1st ", "99th","AMT"))+
  scale_shape_manual("Temperature percentiles:",values=c(15,17,16), labels=c("1st ", "99th", "AMT"))+
  theme_classic()+theme(panel.grid.major = element_line(colour = "gray"),legend.position = "bottom",legend.text=element_text(color="gray40", size=9))
dev.off()

#Ollech and Webel's combined seasonality test
library(seastests)
combined_test(temp$temp_mean, freq = 365)
isSeasonal(temp$temp_mean, test = "combined", freq = 365)

# FIGURE S2:
tiff(paste0(dateFig, "Figure_S2.tiff" ), width = 7, height = 4, units = 'in', res = 800,compression = "lzw")
plot1<-timeseries(temp, temp$date, temp$temp_mean, redD , "Temperature (°C)", "a. Mean daily temperature (°C)") # ") # TEMPERATURE
plot2<-timeseries(temp, temp$date, temp$HR_mean, blueD,"Relative Humidity (%)", "b. Mean daily relative humidity (%)") # RELATIVE HUMIDITY
plot3<-timeseries(temp, temp$date, temp$pm10_mean.mean, grayD,"Air Pollution (µg/m3)", "c. Mean daily PM10 (µg/m3)") # AIR POLLUTION
grid.arrange(plot1, plot2, plot3, nrow=3, ncol=1)
dev.off()

#FIGURE S3:
#COLORED
tiff(paste0(dateFig, "FigureTempDist_annual.tiff" ), units="in", width=8, height=14, res=900,compression = "lzw")
temp %>%
  ggplot(aes(x = temp_mean, y = as.factor(year), fill=year)) +
  geom_density_ridges(quantile_lines=TRUE,  quantile_fun=function(x,...)mean(x))+
  scale_fill_viridis(name = "Year", option = "E") +
  labs(title="",x = "Temperature (°C)", y="")+theme_bw()
dev.off()
#B/W
tiff(paste0(dateFig, "FigureTempDist_annual_mono.tiff" ), units="in", width=8, height=14, res=900,compression = "lzw")
temp %>%
  ggplot(aes(x = temp_mean, y = as.factor(year))) +
  geom_density_ridges(fill="gray", quantile_lines=TRUE, quantile_fun=function(x,...)mean(x))+
  labs(title="",x = "Temperature (°C)", y="")+theme_bw()
dev.off()

#ANIMATED VERSION (WEBSITE ONLY)
library(gganimate);library(transformr)
p<-ggplot(temp, aes(x=temp_mean))+geom_density(position = 'identity',col="black", fill = "blue")+
  xlab("Daily mean temperature (°C)")+ylab("Percentage (%)")+
  theme_classic()+theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
        axis.text.y = element_text(color="gray40", size=7),
        title = element_text(color="gray10", size=9, face="bold"),
        axis.text.x = element_text(color="gray40", size=7 ),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="black", size=9, face="bold",  family="sans"))+
  transition_states(year,2,3, wrap=T)+shadow_mark(alpha = .2)
animate(p, renderer = gifski_renderer())


#TIME SERIES MORTALITY BY POPULATION GROUP 
####
#Figure 4
tiff(paste0(dateFig, "FigureS3_Mort_By_GroupGeneral.tiff" ), units="in", width=14, height=10, res=800,compression = "lzw")
plot1<-timeseriesGroups2(c("total", "cvd","resp"), 300,c("total", "cvd","resp", "date"), c(grayD,greenD, yellowD),c("All non-external", "CVD","Resp. Diseases"),c("A.By cause of death", ""), "bottom", 8)
plot2<-timeseriesGroups2(c("males", "females"), 300,c("males", "females", "date"), c(purpleL, purpleD),c("Males", "Females"),c("B. By gender", ""), "bottom", 8) # BY GENDER
plot3<-timeseriesGroups2(c("RETII_.80", "RETI_65.79"), 300,c("RETII_.80", "RETI_65.79", "date"), c(blueD, blueL),c("80+y", "65-79y"),c("C. By age group", ""), "bottom", 8) # BY AGE GROUP
plot4<-timeseriesGroups2(c("colored", "white"), 300,c("colored", "white", "date"), c(redD, redL),c("Non-White", "Whites"),c("D. By ethnic group", ""), "bottom", 8) # BY ETHNIC GROUP
#plot5<-timeseriesGroups2(c("less3", "X4to11", "more11"), 800,c("less3", "X4to11", "more11", "date"), c(greenD, greenL, greenDD),c("Less 3y", "Between 4-11y", "More 11y"),c("E. By Years of Education", ""), "bottom", 8) # BY ETHNIC GROUP
#grid.arrange(plot1, plot2, plot3, plot4,plot5, nrow=3, ncol=2)
grid.arrange(plot1, plot2, plot3, plot4, nrow=3, ncol=2)
dev.off()

#Figure S5 (by ethnic group)
tiff(paste0(dateFig, "FigureS4_Mort_By_ethnicGroup.tiff" ), units="in", width=8, height=14, res=900,compression = "lzw")
plot1<-timeseriesGroups2(c("colored", "white"), 300,c("colored", "white", "date"), c(redD, redL),c("Non-White", "Whites"),c("C. By ethnic group", ""), "none",8) # BY ETHNIC GROUP
plot2<-timeseriesGroups2(c("females_colored", "females_white"), 300,c("females_colored", "females_white", "date"), c(redD, redL),c("Females Non-White", "Females Whites"),c("Females", ""), "none",8) # BY ETHNIC GROUP
plot3<-timeseriesGroups2(c("males_colored", "males_white"), 300,c("males_colored", "males_white", "date"), c(redD, redL),c("Males Non-White", "Males Whites"),c("Males", ""), "none",8) # BY ETHNIC GROUP
plot4<-timeseriesGroups2(c("RETI_65.79_colored", "RETI_65.79_white"), 300,c("RETI_65.79_colored", "RETI_65.79_white", "date"), c(redD, redL),c("Non-White", "White"),c("Aged 65-79y", ""), "none",8) # BY ETHNIC GROUP
plot5<-timeseriesGroups2(c("RETII_.80_colored", "RETII_.80_white"), 300,c("RETII_.80_colored", "RETII_.80_white", "date"), c(redD, redL),c("Non-White", "White"),c("Aged 80+y", ""), "bottom",8) # BY ETHNIC GROUP
grid.arrange(plot1, plot2, plot3,plot4, plot5, nrow=5, ncol=1)
dev.off()

#Figure S5 (by  gender)
tiff(paste0(dateFig, "FigureS4_Mort_By_Gender.tiff" ), units="in", width=8, height=14, res=900,compression = "lzw")
plot1<-timeseriesGroups2(c("females", "males"), 200,c("females", "males", "date"), c(purpleD,purpleL),c("", ""),c("A. By gender", ""), "none",8) # BY GENDER
plot2<-timeseriesGroups2(c("females_RETI_65.79", "males_RETI_65.79"), 200,c("females_RETI_65.79", "males_RETI_65.79", "date"),c(purpleD,purpleL),c("", ""),c("Aged 65-79y", ""), "none",8) # BY ETHNIC GROUP
plot3<-timeseriesGroups2(c("females_RETII_.80", "males_RETII_.80"), 200,c("females_RETII_.80", "males_RETII_.80", "date"),c(purpleD,purpleL),c("", ""),c("Aged +80y", ""), "none",8) # BY ETHNIC GROUP
plot4<-timeseriesGroups2(c("females_colored", "males_colored"), 200,c("females_colored", "males_colored", "date"),c(purpleD,purpleL),c("", ""),c("Non-whites", ""), "none",8) # BY ETHNIC GROUP
plot5<-timeseriesGroups2(c("females_white", "males_white"), 200,c("females_white", "males_white", "date"),c(purpleD,purpleL),c("Males", "Females"),c("Whites", ""), "bottom",8) # BY ETHNIC GROUP
grid.arrange(plot1, plot2, plot3,plot4, plot5, nrow=5, ncol=1)
dev.off()

#Figure S5 (by age)
tiff(paste0(dateFig, "FigureS4_Mort_By_Age.tiff" ), units="in", width=8, height=14, res=900,compression = "lzw")
plot1<-timeseriesGroups2(c("RETI_65.79","RETII_.80"), 100,c("RETI_65.79","RETII_.80", "date"), c(blueD, blueL),c("", ""),c("B. By age group", ""), "none",8) # BY AGE GROUP
plot2<-timeseriesGroups2(c("females_RETI_65.79", "females_RETII_.80"), 50,c("females_RETI_65.79", "females_RETII_.80", "date"),c(blueD, blueL),c("", ""),c("Females", ""), "none",8) # BY ETHNIC GROUP
plot3<-timeseriesGroups2(c("males_RETI_65.79", "males_RETII_.80"), 50,c("males_RETI_65.79", "males_RETII_.80", "date"),c(blueD, blueL),c("", ""),c("Males", ""), "none",8) # BY ETHNIC GROUP
plot4<-timeseriesGroups2(c("RETI_65.79_colored", "RETII_.80_colored"), 50,c("RETI_65.79_colored", "RETII_.80_colored", "date"),c(blueD, blueL),c("", ""),c("Non-whites", ""), "none",8) # BY ETHNIC GROUP
plot5<-timeseriesGroups2(c("RETI_65.79_white", "RETII_.80_white"), 50,c("RETI_65.79_white", "RETII_.80_white", "date"),c(blueD, blueL),c("80+y", "65-79y"),c("Whites", ""), "bottom",8) # BY ETHNIC GROUP
grid.arrange(plot1, plot2, plot3,plot4, plot5, nrow=5, ncol=1)
dev.off()

#MINIMUM MORTALITY TEMPERATURE (MMT) FIGURES
######
#A)all non external causes by gender, age and ethnic group
data<-subset(mmt, mmt$health %in% out & mmt$int!="nointeraction")
data$health=factor(data$health, levels=out)
intercept<-data %>%
  group_by(health) %>%
  summarise(value= first(na.omit(mmt_constr))) # set the intercept to be the first value in series, excluding NAs
data<-merge(data,intercept, by=c("health"))

tiff(paste0(dateFig, "Fig_3.tiff"), res = 250,height = 10, width=7, units = 'in', compression = "lzw")
MMT_plot1(data, as.numeric(data$int),data$mmt_constr, data$mmt_constr_low, data$mmt_constr_high, round(max(data$mmt_constr_high))+2,round(min(data$mmt_constr_low))-2 , yellowD,greenD, labels, "bottom")
dev.off()
tiff(paste0(dateFig, "Fig_3_cor.tiff"), res = 250,height = 11, width=9, units = 'in', compression = "lzw")
MMT_plot2(data, as.numeric(data$int),data$mmt_constr, data$mmt_constr_low, data$mmt_constr_high, round(max(data$mmt_constr_high))+2,round(min(data$mmt_constr_low))-2 , yellowD,greenD, labels, "bottom")
dev.off()

#FIGURE 3:
plot1<-MMT_plot2(data, as.numeric(data$int),data$mmt_constr, data$mmt_constr_low, data$mmt_constr_high, 26,17 , yellowD,greenD, labels, "bottom")

#B) Add By cause
data<-subset(mmt, mmt$health %in% cause & mmt$int!="nointeraction")
data$health=factor(data$health, levels=cause)
intercept<-data %>%
  group_by(health) %>%
  summarise(value= first(na.omit(mmt_constr))) # set the intercept to be the first value in series, excluding NAs
data<-merge(data,intercept, by=c("health"))
plot2<-MMT_plot2(data, as.numeric(data$int),data$mmt_constr, data$mmt_constr_low, data$mmt_constr_high, round(max(data$mmt_constr_high))+2,round(min(data$mmt_constr_low))-2 , yellowD,greenD, labelsCause, "none")

#All together
tiff(paste0(dateFig, "Fig_MMT.tiff"), res = 250,height = 11, width=8, units = 'in',compression = "lzw")
grid.arrange(plot2, plot1, nrow=2, ncol=1,heights=c(0.15,0.85))
dev.off()


## FIGURE S9. MTT vs. AMT 
#Prepare data
df1<-summaryBy(temp_mean ~ year, data=temp, FUN=function(x){c(mean=mean(x))}) #Calculate AMT
df2<-subset(mmt, select=c(int, health, mmt_constr,mmt_constr_se)) #obtain year, outcome and MMT from orignal MMT dataset
df<-merge(df2, df1, by.x=c("int"), by.y=("year")) # merge datasets 
plot<-list() # empty list to save plots

#code for calling the regression equation:
for(i in 1:length(out)){
  pl<-subset(df, health==out[i])
  plot[[i]]<-ggplot(pl, aes(x=temp_mean.mean, y=mmt_constr, size=mmt_constr_se))+
    geom_point(color="darkorange2", alpha=0.5, size=3)+ xlab("Annual  Temperature (°C)")+ ylab("MMT(°C)")+ggtitle(paste0(out[i]))+
    labs(title=paste0(labels[i]))+xlab("")+ylab("")+
    ylim(22,26)+ #adapt to the desired limits
    geom_smooth(method="lm", aes(weight=mmt_constr_se), fullrange = TRUE, alpha = 0.3, col="gray", fill="gray")+
    stat_cor(col="darkorange2",alpha=0.7,method = "pearson", label.x = 12, label.y = 24.5, size=3)+ #adapt x axis limits to desired values
    stat_poly_eq(aes(label = after_stat(eq.label)),col="darkorange2",alpha=0.7, size=3) +
    #stat_poly_eq(label.y = 0.85,col="darkorange2",alpha=0.7, size=3) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
          panel.grid.major = element_line(linetype = "dashed",colour = "gray80", size=0.4),
          axis.title.x = element_text(color="black", size=5, face="bold"),
          axis.title.y = element_text(color="black", size=5, face="bold",  family="sans"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=7),axis.text.y = element_text(size=7),
          legend.text=element_text(size=6),legend.title=element_text(size=7, face="bold"), legend.position="bottom",
          strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
          strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"), 
          plot.title=element_text(size = 10))
}
tiff(paste0(dateFig, "/FigureS_MMT_AMT.tiff" ), units="in", width=11, height=10, res=900,compression = "lzw")
do.call("grid.arrange", c(plot, ncol=floor(sqrt(length(plot)))))
dev.off()



#CUMULATIVE RELATIVE RISK (cRR) FIGURES
######
perc <- quantile(temp$temp_mean,c(0,0.01, 0.025,0.10, 0.5,0.90, 0.975,0.99,1), na.rm=TRUE) #Define percentages

#FIGURE S6: NO INTERACTION
#Prepare data
rr$year<-as.numeric(rr$year)#make sure year is numeric
dfall<-merge(subset(rr, year=="nointeraction"), subset(mmt, int=="nointeraction"), by.x = c("sheet_name", "year"), by.y=c("health","int")) #add mmt values to rr
dfall<-merge(rr,mmt, by.x = c("sheet_name", "year"), by.y=c("health","int")) #add mmt values to rr

#plot
dfall<-subset(dfall, sheet_name %in% out)
dfall$sheet_name=factor(dfall$sheet_name, levels=out)
dfall$paint<-"blue"; dfall$paint[as.numeric(dfall$index)>dfall$mmt_constr]<-"red"
tiff(paste0(dateFig, "/Figure_cRR_noint.tiff" ),units="in", res=800, height = 11, width = 8,compression = "lzw")
ggplot(dfall, aes(x=as.numeric(index), y=allRRfit))+
  geom_ribbon(aes(ymin = allRRlow, ymax = allRRhigh), colour = NA,alpha=0.3, show.legend = F) +
  geom_line(size=0.8, color=dfall$paint)+ 
  geom_vline(mapping=aes(xintercept=mmt_constr), show.legend = F, size=0.3,linetype="dashed")+
  geom_vline(xintercept=c(perc[2],perc[3], perc[7], perc[8]), color="gray60", linetype="dotted") + #add dotted lines for 10tha dn 90th percentiles
  xlab("Temperature (°C)")+ylab("Cumulative Relative Risk")+
  scale_x_continuous(breaks=round(seq(round(min(as.numeric(dfall$index))), round(max(as.numeric(dfall$index))+2), by=1),1))+
  coord_cartesian(ylim=c(0.5,3))+ labs(col = "year")+
  geom_hline(yintercept=1, linetype="solid", color = "grey50")+ # add baseline df=1
  scale_color_identity() +
  scale_fill_viridis_c("Year")+
  theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
        axis.text.y = element_text(color="gray40", size=5),
        title = element_text(color="gray10", size=7, face="bold"),
        axis.text.x = element_text(color="gray40", size=5),
        axis.title.x = element_text(color="black", size=7, face="bold"),
        axis.title.y = element_text(color="black", size=7, face="bold"),
        legend.text=element_text(size=5),legend.title=element_text(size=7, face="bold"),  legend.position ="bottom",
        strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
        strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
  facet_wrap(~sheet_name, ncol=3,  labeller=labeller(sheet_name=labels))
dev.off()

#FIGURE 2 (no 95%CI) and FIGURE S9 (with 95%CI)
#A)by cause
df<-subset(dfall, sheet_name %in% cause )#subset data
df$sheet_name=factor(df$sheet_name, levels=cause)
df$mmt_constr[df$mmt_constr<10]<-NA
plot1<-plotRRannual(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year, "magma", labelsCause, "none")

#B)all non external causes by gender, age and ethnci group
df<-subset(dfall, sheet_name %in% out1 )#subset data
df$sheet_name=factor(df$sheet_name, levels=out1)
plot2<-plotRRannual(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year,"magma", labels1, "bottom")#A) No confidence intervals

#EXPORT
tiff(paste0(dateFig, "Fig2_P1.tiff"), res = 250,height = 10, width=7, units = 'in',compression = "lzw")
grid.arrange(plot1, plot2, nrow=2, ncol=1,heights=c(0.15,0.85))
dev.off()

##PAGE 2
df<-subset(dfall, sheet_name %in% out2)#subset data
df$sheet_name=factor(df$sheet_name, levels=out2)
tiff(paste0(dateFig, "/Fig2_P2.tiff" ),units="in", res=800, height = 4, width = 8)
plotRRannual(df, df$index, df$allRRfit, df$allRRfit, df$allRRfit, df$year,"magma", labels2, "bottom")#A) No confidence intervals
dev.off()

#FIGURE S7. cRR curves for INTERACTIOn
#by population group
df<-subset(dfINT, sheet_name %in% out)
df$sheet_name=factor(df$sheet_name, levels=out)
df$allRRlow[df$allRRlow< -0.5]<--0.5; df$allRRhigh[df$allRRhigh>4.5]<-4.5; 
plot2<-plotRRannual(df, as.numeric(df$index), df$allRRfit, df$allRRhigh, df$allRRlow, as.numeric(df$year), "magma",labels, "none")#A) No confidence intervals

#by cause
df<-subset(dfINT, sheet_name %in% cause )
df$sheet_name=factor(df$sheet_name, levels=cause)
plot1<-plotRRannual(df, as.numeric(df$index), df$allRRfit, df$allRRhigh, df$allRRlow, as.numeric(df$year), "magma",labelsCause, "none")#A) No confidence intervals

#plot together
tiff(paste0(dateFig, "/Fig_s7_Interaction.tiff" ), units="in", res=800, height = 10, width = 8)
grid.arrange(plot1, plot2, nrow=2, ncol=1,heights=c(0.15,0.85))
dev.off()

##FIGURE 4. TRENDS cRR 1ST 99TH PERCENTILE
#a) 99/1st percentiles
df<-subset(dfall, sheet_name %in% out & (per=="99.0%" |per=="1.0%"))#subset data
df$sheet_name=factor(df$sheet_name, levels=out)
df$allRRhigh[df$allRRhigh>2.1]<-2.1

plot1<-ggplot(df, aes(x=year, y=allRRfit, group=per, color=per, fill=per))+
  geom_line()+geom_point()+
  coord_cartesian(ylim=c(0.8,2))+
  geom_ribbon(aes(ymin=allRRlow, ymax=allRRhigh, color=NULL), alpha=0.3)+
  scale_fill_manual(labels = c("Cold", "Heat"), values = c("blue", "red")) +
  scale_color_manual(labels = c("Cold", "Heat"), values = c("blue", "red")) +
  scale_x_continuous(breaks=round(seq(round(min(as.numeric(year))), round(max(as.numeric(year))+2), by=1),1))+
  labs(fill="", color="")+ xlab("Years")+ylab("Cumulative Relative Risk")+
  theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
        panel.grid.major = element_line(color = "gray70", linetype = "dashed", size=0.15),
        axis.text.y = element_text(color="gray40", size=5),
        title = element_text(color="gray10", size=7, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=7),
        axis.title.x = element_text(color="black", size=7, face="bold"),
        axis.title.y = element_text(color="black", size=7, face="bold"),
        legend.text=element_text(size=5),legend.title=element_text(size=7, face="bold"),  legend.position ="bottom",
        strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
        strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
  facet_wrap(~sheet_name, ncol=3,  labeller=labeller(sheet_name=labels))

#by cause
data<-subset(dfall, dfall$sheet_name %in% cause & (per=="99.0%" |per=="1.0%"))
data$sheet_name=factor(data$sheet_name, levels=cause)
plot2<-ggplot(data, aes(x=year, y=allRRfit, group=per, color=per, fill=per))+
  geom_line()+geom_point()+
  coord_cartesian(ylim=c(0.8,3))+
  geom_ribbon(aes(ymin=allRRlow, ymax=allRRhigh, color=NULL), alpha=0.3)+
  scale_fill_manual(labels = c("Cold", "Heat"), values = c("blue", "red")) +
  scale_color_manual(labels = c("Cold", "Heat"), values = c("blue", "red")) +
  scale_x_continuous(breaks=round(seq(round(min(as.numeric(year))), round(max(as.numeric(year))+2), by=1),1))+
  labs(fill="", color="")+ xlab("Years")+ylab("Cumulative Relative Risk")+
  theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
        panel.grid.major = element_line(color = "gray70", linetype = "dashed", size=0.15),
        axis.text.y = element_text(color="gray40", size=5),
        title = element_text(color="gray10", size=7, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=7),
        axis.title.x = element_text(color="black", size=7, face="bold"),
        axis.title.y = element_text(color="black", size=7, face="bold"),
        legend.text=element_text(size=5),legend.title=element_text(size=7, face="bold"),  legend.position ="none",
        strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
        strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
  facet_wrap(~sheet_name, ncol=3,  labeller=labeller(sheet_name=labelsCause))

#All together
tiff(paste0(dateFig, "Fig4_cRR.tiff"), res = 250,height = 11, width=8, units = 'in',compression = "lzw")
grid.arrange(plot2, plot1, nrow=2, ncol=1,heights=c(0.15,0.85))
dev.off()


## FIGURE S10-11: Association between 99/1 percentiles of cRR and temperature
df1<-summaryBy(temp_mean ~ year, data=temp, FUN=function(x){c(mean=mean(x), min=min(x), quantile(x, c(0.99,0.01)))}) #Calculate annual temperature mean
df2<-subset(dfall, select=c(sheet_name, allRRfit, year, per) , per=="99.0%" | per=="1.0%") #obtain year, outcome group and mmt value from orignal mmt dataset
df<-merge(df2, df1, by=c("year"), all.x = T) # merge datasets 
colnames(df)<-gsub("%","",colnames(df))

#Define plotting function:
plotcRRtemp<-function(perc, name, color,ylim1, ylim2){
plot<-list() # empty list to save plots
for(i in 1:length(out)){
  pl<-subset(df, sheet_name==out[i])
  plot[[i]]<-ggplot(subset(pl,per==perc), aes(x=temp_mean.99, y=allRRfit))+
    geom_point(color=color, alpha=0.3, size=3)+ xlab("Annual  Temperature (°C)")+ ylab("MMT(°C)")+ggtitle(paste0(out[i]))+
    labs(title=paste0(labels[i]))+xlab("")+ylab("")+ylim(ylim1,ylim2)+
    geom_smooth(method="lm", fullrange = TRUE, alpha = 0.25, col="gray", fill="gray")+
    stat_cor(col=color,alpha=0.7,method = "pearson", label.y = 0.95, size=3)+
    stat_poly_eq(aes(label = after_stat(eq.label)),label.y = 0.23,col=color,alpha=0.7, size=3) +
    #stat_poly_eq(label.y = 0.85,col="darkorange2",alpha=0.7, size=3) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
          panel.grid.major = element_line(linetype = "dashed",colour = "gray80", size=0.4),
          axis.title.x = element_text(color="black", size=5, face="bold"),
          axis.title.y = element_text(color="black", size=5, face="bold",  family="sans"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=7),axis.text.y = element_text(size=7),
          legend.text=element_text(size=6),legend.title=element_text(size=7, face="bold"), legend.position="bottom",
          strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
          strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"), 
          plot.title=element_text(size = 10))
    #theme_bw()
}
tiff(paste0(dateFig, "/FigureS10_11_crr", name, "_Temp", name, ".tiff" ), units="in", width=11, height=10, res=900,compression = "lzw")
do.call("grid.arrange", c(plot, ncol=floor(sqrt(length(plot)))))
dev.off()
}
plotcRRtemp("99.0%", "99", "red",  0.85,1.5)
plotcRRtemp("1.0%", "1", "blue",  0.85,1.7)



##FIGURE S8. CRR FOR Education (not available with synthetic dataset)
#df<-subset(dfall, sheet_name %in% out3 & year>=2012)#subset data
#df$sheet_name=factor(df$sheet_name, levels=out3)
#tiff(paste0(dateFig, "/FigureS8_cRR_Education.tiff" ),units="in", res=800, height = 10, width = 8)
#plotRRannual(df, df$index, df$allRRfit,df$allRRhigh, df$allRRlow , df$year, "magma", labels3, "bottom") # B) With confidence intervals
#dev.off()



#4. TABLES
########################################################################################################################################################
tabpath<-paste0(getwd(),"/outputs/tables/", date,"/") 

#TABLE 1.
rr$year<-as.numeric(rr$year)
rrtable<-subset(rr, (rr$year==2000 | rr$year==2018) & (rr$per=="1.0%" | rr$per=="10.0%" |rr$per=="90.0%" | rr$per=="99.0%"), select=-c(file_name, index))
rrtable<-subset(rrtable,!is.na(rrtable$allRRfit))
wt<-subset(mmt, select=c("health", "WaldTest")); wt<-wt %>% distinct(health, .keep_all = TRUE)

rrtable1<-reshape(as.data.frame(rrtable), 
                  timevar="per",
                  idvar=c("sheet_name", "year"),
                  direction="wide")
rrtable<-merge(rrtable1, wt, by.y="health", by.x="sheet_name")
rrtable<-subset(rrtable, rrtable$sheet_name %in% out )
openxlsx::write.xlsx(rrtable, paste0(tabpath,"/Table1_RR10.90_table.xlsx"), colNames = TRUE,asTable = FALSE)

#mmt table
mmttable<-subset(mmt, (mmt$int==2000 | mmt$int==2018), select=-c(WaldTest, qaic))
mmttable<-subset(mmttable, mmttable$health %in% out )
openxlsx::write.xlsx(mmttable, paste0(tabpath,"/Table1_MMT_table.xlsx"), colNames = TRUE,asTable = FALSE)



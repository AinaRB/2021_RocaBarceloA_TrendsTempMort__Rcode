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
#This script contains the main functions to reproduce the plots in the manuscript and some data handling functions. 
#Depending on your input data, you may need tomodify some of the parameters, such as the ylim and xlim. 
#####################################################################################################

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
#              , destfile = "2017_tobias_Epidem_Rcodedata-master.zip") # Download folder from GitHub
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

#READ IN XLS SHEETS
read_sheets <- function(dir_path, file){
  xlsx_file <- paste0(dir_path, file)
  xlsx_file %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel, path = xlsx_file, .id = 'sheet_name') %>% 
    mutate(file_name = file) %>% 
    dplyr::select(file_name, sheet_name, everything())
}

# TIME SERIES PLOTS
timeseries<-function(data, x, y, color, YLAB,title){ 
  ggplot(data, aes(x=as.Date(x), y=y))+
    geom_point(size=0.03, col=color)+
    ggtitle(title)+
    xlab("Years")+ ylab(YLAB)+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",date_minor_breaks = "1 month")+
    theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.3, colour="black"),
          panel.grid.major = element_line(color = "gray70", linetype = "dashed", size=0.15),
          axis.text.y = element_text(color="gray40", size=4),
          title = element_text(color="gray10", size=4, face="bold"),
          axis.text.x = element_text(color="gray40", size=4, angle = 45, hjust = 1),
          axis.title.x = element_text(color="black", size=4, face="bold"),
          axis.title.y = element_text(color="black", size=4, face="bold"))
}

timeseriesGroups2<-function(vars,ylim, varsTime, valuesT, lbls, title, legend, sizetext){
  temporary<-reshape(subset(temp, select=varsTime), 
                     varying=vars, v.names="death", timevar="group", times=vars, direction="long")
  ggplot(temporary, aes(x=as.Date(temporary$date), color=factor(group)))+
    geom_point(aes(y=death),size=0.03)+    
    scale_color_manual(name="", breaks=vars, values=valuesT, labels=lbls)+
    ggtitle(title)+ylim(0,ylim)+ 
    xlab("Years")+ ylab("Count")+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year",date_minor_breaks = "1 month")+
    theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.3, colour="black"),
          panel.grid.major = element_line(color = "gray70", linetype = "dashed", size=0.15),
          axis.text.y = element_text(color="gray40", size=sizetext),
          title = element_text(color="gray10", size=sizetext, face="bold"),
          axis.text.x = element_text(color="gray40", size=sizetext,angle = 45, hjust = 1),
          legend.position = legend, legend.key=element_blank(),legend.text=element_text(color="gray40", size=sizetext),
          axis.title.x = element_text(color="black", size=sizetext, face="bold"),
          axis.title.y = element_text(color="black", size=sizetext, face="bold")) + guides(color = guide_legend(override.aes = list(size=sizetext)))
}


#1. MMT PLOTS
library(ggpubr); library(ggpmisc)
##1.1 
MMT_plot1<-function(data, x, y, ribbonmin, ribbonmax, ylimtop, ylimbottom, col1, col2, lbl, legend){
  ggplot(data, aes(x=int, col=y > value)) + 
    geom_linerange(aes(ymin=value, ymax=y), size=10) +
    geom_errorbar(aes(ymin=ribbonmin, ymax=ribbonmax), width=0.25, colour="gray30") +
    geom_point(aes(y=y), size=1,col="gray25")+
    geom_hline(aes(yintercept=value), col="gray70")+
    scale_y_continuous(breaks=seq(15,30, by=2),limits=c(15, 30))+
    scale_color_manual(values = c(col1, col2),labels = c("Reduced MMT", "Increased MMT"), "Absolute difference \n(ref. 2000)", na.translate = F)+
    xlab("Years") + ylab("Minimum Mortality Temperature (°C)")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"), panel.grid.major = element_line(linetype = "dashed",colour = "gray80", size=0.4),
          axis.title.x = element_text(color="black", size=8, face="bold"),
          axis.title.y = element_text(color="black", size=8, face="bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=7),axis.text.y = element_text(size=7),
          legend.text=element_text(size=6),legend.title=element_text(size=7, face="bold"), legend.position=legend,
          strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
          strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
    facet_wrap(~health, ncol=3,  labeller=labeller(health=lbl))
}
##1.2.
MMT_plot2<-function(data, x, y, ribbonmin, ribbonmax, ylimtop, ylimbottom, col1, col2, lbl, legend){
  ggplot(data, aes(x=as.numeric(int) ,y=mmt_constr)) + 
    geom_point(color="darkorange2", alpha=0.5, size=1.5)+ 
    # geom_errorbar(aes(ymin=ribbonmin, ymax=ribbonmax), width=0.25, colour="darkorange2") +
    geom_smooth(method="lm", fullrange = TRUE, alpha = 0.3, col="gray", fill="gray", size=0.6)+
    stat_cor(col="darkorange2",alpha=0.7,method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, label.x = 2000, label.y = 17.8, digits = 2, size=3)+
    stat_poly_eq(aes(label = after_stat(eq.label)),col="darkorange2",alpha=0.7,label.y = 0.23, size=3) +
    scale_y_continuous(breaks=seq(ylimbottom,ylimtop, by=2),limits=c(ylimbottom,ylimtop))+
    scale_x_continuous(breaks=seq(2000,2018, by=1),limits=c(2000,2018))+
    scale_color_manual(values = c(col1, col2),labels = c("Reduced MMT", "Increased MMT"), "Absolute difference \n(ref. 2000)", na.translate = F)+
    xlab("Years") + ylab("Minimum Mortality Temperature (°C)")+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"), panel.grid.major = element_line(linetype = "dashed",colour = "gray80", size=0.4),
          axis.title.x = element_text(color="black", size=8, face="bold"),
          axis.title.y = element_text(color="black", size=8, face="bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size=7),axis.text.y = element_text(size=7),
          legend.text=element_text(size=6),legend.title=element_text(size=7, face="bold"), legend.position=legend,
          strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
          strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
    facet_wrap(~health, ncol=3,  labeller=labeller(health=lbl))
}
##1.3. MMT plots for sensitivity analyses
MMTSAPlot<-function(dataset, nameList,title,  filename){
  pal <- brewer.pal(8,"BrBG")
  pal[3] <- "grey"
  tiff(paste0(figurespath, "/FigureS12_SA_mmtSA", filename, ".tiff" ), units="in", res=800, height =4, width = 5, compression = "lzw")
  print(ggplot(dataset, aes(x=int, y=mmt_constr, group=factor(SA),color=factor(SA)))+
          geom_point(position=position_dodge(0.7))+
          geom_errorbar(aes(ymin=mmt_constr_low, ymax=mmt_constr_high), width=0.2, position=position_dodge(0.7))+
          ylab("Minimum Mortality Temperature (°C)")+xlab("")+
          scale_color_manual(values = pal,name="", labels= nameList)+
          theme_bw()+#labs(fill = "Basis functions")+
          ggtitle(title)+ ylim(20,24)+
          theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
                axis.text.y = element_text(color="gray40", size=7),
                title = element_text(color="gray10", size=9, face="bold"),
                axis.text.x = element_text(color="gray40", size=7, angle = 45, hjust = 1),
                axis.title.x = element_text(color="black", size=9, face="bold"),
                axis.title.y = element_text(color="black", size=9, face="bold",  family="sans"),
                legend.text=element_text(size=7),legend.title=element_text(size=8, face="bold"),
                strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
                strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"),
                legend.position = "bottom", legend.direction = "horizontal")+guides(color=guide_legend(ncol=3)) )
  
  dev.off()
}
# 2. cRR curves
##2.1. Overall
plotRRannual<-function(data, x, y, cihigh, cilow,group, mycolor, lbl, legend){
  ggplot(data, aes(x=as.numeric(x), y=y, group=group, col=group, fill=as.numeric(group)))+
    geom_ribbon(aes(ymin = cilow, ymax = cihigh, group=group, fill=group), colour = NA,alpha=0.3, show.legend = F) +
    geom_line(size=0.8)+ 
    geom_vline(mapping=aes(xintercept=mmt_constr, color=group),data=df, show.legend = F, size=0.3,linetype="dashed")+
    geom_vline(xintercept=c(perc[2],perc[3], perc[7], perc[8]), color="gray60", linetype="dotted") + #add dotted lines for 10tha dn 90th percentiles
    xlab("Temperature (°C)")+ylab("Cumulative Relative Risk")+
    scale_x_continuous(breaks=round(seq(round(min(as.numeric(x))), round(max(as.numeric(x))+2), by=1),1))+
    coord_cartesian(ylim=c(-0.5,4.5))+ labs(col = "year")+
    geom_hline(yintercept=1, linetype="solid", color = "grey50")+ # add baseline df=1
    scale_color_viridis_c("Year", option=mycolor)+
    scale_fill_viridis_c("Year", option=mycolor)+
    theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
          axis.text.y = element_text(color="gray40", size=5),
          title = element_text(color="gray10", size=7, face="bold"),
          axis.text.x = element_text(color="gray40", size=5),
          axis.title.x = element_text(color="black", size=7, face="bold"),
          axis.title.y = element_text(color="black", size=7, face="bold"),
          legend.text=element_text(size=5),legend.title=element_text(size=7, face="bold"),  legend.position =legend,
          strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
          strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"))+
    facet_wrap(~sheet_name, ncol=3,  labeller=labeller(sheet_name=lbl))
}

## 2.2. Extreme heat
cRR99SAPlot<-function(dataset, nameList,title,  filename){
  pal <- brewer.pal(8,"BrBG")
  pal[3] <- "grey40"
  tiff(paste0(figurespath, "/FigureS12_SA_cRR99", filename, ".tiff" ), units="in", res=800, height =4, width = 5, compression = "lzw")
  print(ggplot(subset(dataset, per=="99.0%" ), aes(x=year, y=allRRfit, group=factor(SA),fill=factor(SA),color=factor(SA)))+
          geom_ribbon(aes(ymin=allRRlow, ymax=allRRhigh), alpha=0.3, color=NA)+
          geom_point()+geom_line()+
          ylab("Cumulative Relative Risk")+xlab("")+
          scale_color_manual(values = pal,name="", labels= nameList)+
          scale_fill_manual(values = pal,name="", labels= nameList)+
          theme_bw()+ylim(1.02,1.32)+
          ggtitle(title)+
          theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
                axis.text.y = element_text(color="gray40", size=7),
                title = element_text(color="gray10", size=9, face="bold"),
                axis.text.x = element_text(color="gray40", size=7, angle = 45, hjust = 1),
                axis.title.x = element_text(color="black", size=9, face="bold"),
                axis.title.y = element_text(color="black", size=9, face="bold",  family="sans"),
                legend.text=element_text(size=7),legend.title=element_text(size=8, face="bold"),
                strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
                strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"),
                legend.position = "bottom", legend.direction = "horizontal")+guides(color=guide_legend(ncol=3)) )
  
  dev.off()
}

## 2.3. Extreme cold
cRR1SAPlot<-function(dataset, nameList,title,  filename){
  pal <- brewer.pal(8,"BrBG")
  pal[3] <- "grey40"
  tiff(paste0(figurespath, "/FigureS12_SA_cRR1", filename, ".tiff" ), units="in", res=800, height =4, width = 5, compression = "lzw")
  print(ggplot(subset(dataset, per=="1.0%" ), aes(x=year, y=allRRfit, group=factor(SA),fill=factor(SA),color=factor(SA)))+
          geom_ribbon(aes(ymin=allRRlow, ymax=allRRhigh), alpha=0.3, color=NA)+
          geom_point()+geom_line()+
          ylab("Cumulative Relative Risk")+xlab("")+
          scale_color_manual(values = pal,name="", labels= nameList)+
          scale_fill_manual(values = pal,name="", labels= nameList)+
          theme_bw()+ylim(1.1,1.5)+
          ggtitle(title)+
          theme(panel.background = element_rect(fill = "white"), axis.line=element_line(size=0.5, colour="gray70"),
                axis.text.y = element_text(color="gray40", size=7),
                title = element_text(color="gray10", size=9, face="bold"),
                axis.text.x = element_text(color="gray40", size=7, angle = 45, hjust = 1),
                axis.title.x = element_text(color="black", size=9, face="bold"),
                axis.title.y = element_text(color="black", size=9, face="bold",  family="sans"),
                legend.text=element_text(size=7),legend.title=element_text(size=8, face="bold"),
                strip.text.x = element_text(size = 7, color = "black", face = "bold",hjust = 0), 
                strip.background = element_rect(color="white", fill="white", size=1, linetype = "solid"),
                legend.position = "bottom", legend.direction = "horizontal")+guides(color=guide_legend(ncol=3)) )
  
  dev.off()
}


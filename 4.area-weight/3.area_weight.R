#'''
#@author:xiyuchen
#@time:2022/10/19,19:54
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
library(xlsx)
library(sf)
library(lubridate)
library(trend)
library(ggplot2)
library(scales)
library(maptools)
library(plyr)
library(raster)
library(ggpubr)
options(scipen = 20) 
options(max.print=100000) 
memory.limit(size = 2000000)
setwd('D:/paper/R/4.area_weight/')
load("D:/paper/R/4.area_weight/Rdata/Pre/spatio-temporal.Rdata") 
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")

indexs<-c('CDD','CWD','R10','R20','R95p','R95pTOT','R99p','R99pTOT','Rx1day','Rx5',
          'SDII','R95_percentile','R99_percentile')
#OBS and CN05
Datas<-c('OBS','CN05','CN05all')#m

#calculate the time series of obs by anomaly and area-weighted
#'@x the grid_year format obs,@weight the weight for each grid
#'return the area-weighted obs time series
obs_area_weight<-function(x,weight){
  rowmean<-rowMeans(x,na.rm = T)
  x<-x-rowmean#calculate the anomaly
  afterweight<-x*weight#calculate area-weighted
  colsum<-colSums(afterweight,na.rm = T)
  return(colsum)
}

for (m in 1:2) {
  #m<-1
  #load weights
  if(m %in% c(1,2)){
    load("D:/paper/R/4.area_weight/Rdata/Pre/weight-years.Rdata") 
  }else if(m==3){
    load("D:/paper/R/4.area_weight/Rdata/Pre/weight-CN05all.Rdata") 
  }
  
  for (i in 1:length(indexs)) {
    #i<-1
    load(paste0('./Rdata/',Datas[m],'-index/',indexs[i],'.Rdata'))
    
    index <- index * tongjistationyear[gridindex,]
    finalweight<-obs_area_weight(index,weights[gridindex,])
    finalweight<-data.frame(c(1961:2020),finalweight)
    colnames(finalweight)<-c("year","value")
    
    Slope <- paste0("Slope=",round(sens.slope(finalweight[,2])$estimate,2))
    p<-sens.slope(finalweight[,2])$p.value
    p <- if(p < 0.01) sprintf("italic(p) < 0.01") else if(p < 0.05) sprintf("italic(p) < 0.05") else sprintf("italic(p)== %.2f", p)
    
    F1<-
      ggplot(finalweight,aes(x=year,y=value))+
      geom_point(size=1.8,shape=1)+
      geom_line(lwd=0.8)+
      stat_smooth(method = lm,lwd=0.8)+
      xlab('Year')+ylab('Anomalies')+
      annotate("text",x=mean(range(finalweight$year)),y=Inf,label=indexs[i],vjust=1.2,fontface = "bold",size=5)+
      annotate("text",x = 1985, y = -Inf,vjust=-1.2,label=Slope,size=4.5)+
      annotate("text",x = 1995, y = -Inf,vjust=-0.8,parse=TRUE,label=p,fontface = "bold",size=4.5,color ='#000000')+
      theme_bw()+
      theme(axis.title = element_text(size = rel(1), face = "bold"),
            axis.text = element_text(size = rel(1),face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    png(filename = paste0('./fig/',Datas[m],'/',indexs[i],'.png'), width = 6, height = 4, res = 500, units = 'in')
    plot(F1)
    dev.off() 
  }
}

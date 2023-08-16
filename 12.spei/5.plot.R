#'''
#@author:xiyuchen
#@time:2022/11/15,15:49
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(magrittr)
library(lubridate)
library(SPEI)
library(lmom)
library(zoo)
library(trend)
setwd('D:/paper/R/12.spei/')
source('./drought-features-function.R')

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

Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)

features<-c('drought_peak','drought_intensity','drought_duration')
fun <- list()
fun[[1]]<- drought_peak
fun[[2]]<-drought_intensity
fun[[3]]<-drought_duration

#weights
load("D:/paper/R/4.area_weight/Rdata/Pre/weight.Rdata")

Datas<-c('OBS','CN05')#m
pets<-c('Penman','Thornthwaite')#n
ngrid<-1449
nyear<-60

for (m in 1:length(Datas)) {
  for (n in 1:length(pets)) {
    #load data
    load(paste0('./Rdata/',Datas[m],'-daily-',pets[n],'/SPEI_daily_90days.Rdata'))
    for (k in 1:length(features)) {
      #k<-1
      
      drought_feature<-matrix(NA,nrow = ngrid,ncol = nyear)
      Date <- seq(from = as.Date('1961-01-01'), to = as.Date('2020-12-31'), by = 1) %>% year()
      for (i in 1:ngrid) {
        drought_feature[i,]<-tapply(SPEI_daily_90days[i,], Date, fun[[k]])
      }
      data <- drought_feature
      save(data,file = paste0('./Rdata/',Datas[m],'-daily-',pets[n],'/',features[k],'.Rdata'))
      
      finalweight<-obs_area_weight(drought_feature,na.omit(weights))#modify
      finalweight<-data.frame(c(1961:2020),finalweight)
      colnames(finalweight)<-c("year","value")
      
      Slope <- paste0("Slope=",round(sens.slope(finalweight[,2])$estimate,4))
      p<-sens.slope(finalweight[,2])$p.value
      p <- if(p > 0.05) sprintf("italic(p)== %.2f", p) else sprintf("italic(p) < 0.05")
      
      F1<-
        ggplot(finalweight,aes(x=year,y=value))+
        geom_point(size=1.8,shape=1)+
        geom_line(lwd=0.8)+
        stat_smooth(method = lm,lwd=0.8)+
        xlab('Year')+ylab('Anomalies')+
        annotate("text",x=mean(range(finalweight$year)),y=Inf,label=features[k],vjust=1.2,fontface = "bold",size=5)+#modify
        annotate("text",x = 1985, y = -Inf,vjust=-1.2,label=Slope,size=4.5)+
        annotate("text",x = 1995, y = -Inf,vjust=-0.8,parse=TRUE,label=p,fontface = "bold",size=4.5,color ='#000000')+
        theme_bw()+
        theme(axis.title = element_text(size = rel(1), face = "bold"),
              axis.text = element_text(size = rel(1),face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
      png(filename = paste0('./fig/',Datas[m],'-daily-',pets[n],'/',features[k],'.png'), width = 6, height = 4, res = 500, units = 'in')
      plot(F1)
      dev.off() 
    }
  }
}

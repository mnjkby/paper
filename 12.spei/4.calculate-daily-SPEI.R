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
source("./SPEI_daily.R")

Datas<-c('OBS','CN05')#m
#OBS and CN05
filePath<-c('D:/paper/R/3.grid/Rdata/','D:/paper/R/12.spei/Rdata/CN05data/')

pets<-c('Penman','Thornthwaite')#n

for (m in 1:length(filePath)) {
  
  #PRE
  load(paste0(filePath[m],'Pre/cantiquzhibiao.Rdata'))
  PRE<-grid %>% as.matrix()
  attr(PRE, 'dimnames') <- NULL
  rm(grid);gc()
  for (n in 1:length(pets)) {
    
    # -- a.calculate daily SPEI--------------------------------
    #PET
    load(paste0('./Rdata/',Datas[m],'-daily-',pets[n],'/PET.Rdata'))
    
    #global variable
    ngrid<-nrow(PET)
    ndays<-ncol(PET)
    nyear<-60
    date <- seq(from = as.Date('1961-01-01'), to = as.Date('2020-12-31'), by = 1) %>% as.character()
    
    #calculate daily SPEI,3 minutes
    SPEI_daily_90days<-matrix(NA,nrow = ngrid,ncol = ndays)
    for (i in 1:nrow(PET)) {#each grid
      #print(i)
      climate<-data.frame(date,WB=PRE[i,]-PET[i,])
      SPEI_daily_90days[i,] <- SPEI_daily(input = climate, scale = 90)[,3]
    }
    
    SPEI_daily_90days[which(is.infinite(SPEI_daily_90days))]<-NA#have -Inf,need solve
    min(SPEI_daily_90days,na.rm = T)
    
    save(SPEI_daily_90days,file =paste0('./Rdata/',Datas[m],'-daily-',pets[n],'/SPEI_daily_90days.Rdata'))
    rm(PET,climate);gc()
  }
}

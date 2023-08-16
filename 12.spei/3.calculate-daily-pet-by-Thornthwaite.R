#'''
#@author:xiyuchen
#@time:2022/11/18,9:45
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(lubridate)
library(magrittr)
library(stringr)
library(Hmisc)
setwd('D:/paper/R/12.spei/')

Thornthwaite <- function(Ta, I, day, a = 1.6, b = 6.75e-7, c = 7.71e-5, d = 1.79e-2, e = 0.49, conv = 10){
  exp.part <- (b * (I ^ 3)) - (c * (I ^ 2)) + (d * I) + e
  Ta.part <- (conv * Ta / I) ^ exp.part
  E <- a * Ta.part * (conv / day)
  E <- ifelse(is.nan(E), 0, E)
  E
}

#OBS and CN05
filePath<-c('D:/paper/R/3.grid/Rdata/','D:/paper/R/12.spei/Rdata/CN05data/')
Datas<-c('OBS','CN05')#n

for (n in 1:length(filePath)) {
  print(n)
  # -- a.prepare Tm,monthdays and I data---------------------
  
  #Tm
  load(paste0(filePath[n],'Tm/cantiquzhibiao.Rdata'))
  Tm<-as.matrix(grid)
  attr(Tm, 'dimnames') <- NULL
  rm(grid);gc()
  
  #day
  Dates <- seq(make_date(year = 1961,month = 1,day = 1),
               make_date(year = 2020,month = 12,day = 31), by = "day") 
  monthdays<-monthDays(Dates)  %>% as.numeric()
  
  #calculate I
  {
    #translate the format of year-month-day into year-month
    #'@x a vector of time series of year-month-day
    #'return a vector of time series of year-month
    year_month_daytoyear_month<-function(x){
      year_month<-c()
      for (i in 1:length(x)) {
        strs <- str_split(x[i],'-')
        year_month[i] <- as.numeric(paste(strs[[1]][1],strs[[1]][2],sep=''))
      }
      return(year_month)
    }
    year_month<-year_month_daytoyear_month(Dates)
    
    #global variable
    ngrid<-nrow(Tm)
    ndays<-ncol(Tm)
    #daily tem data translate into monthly tem data
    grid_month<-matrix(NA,nrow = ngrid,ncol = length(unique(year_month)))
    for (i in 1:ngrid) {
      #print(i)
      grid_month[i,]<-tapply(Tm[i,],year_month,mean)
    }
    
    annual_heat_index<-function(x){
      x<-na.omit(x)
      if(length(x)==12){
        return(sum((x/5)^1.514))
      }else{
        return(NA)
      }
    }
    
    yeardays<-rep(c(365,365,365,366),15)
    month<-rep(1:60,each=12)
    I<-matrix(NA,nrow = ngrid,ncol = ndays)
    for (i in 1:ngrid) {
      #print(i)
      test<-tapply(grid_month[i,],month,annual_heat_index)
      I[i,]<-rep(test,yeardays)
    }
  }
  
  # -- b.calculate PET by Thornthwaite---------------------------
  
  #calculate PET by Thornthwaite,1.6 minutes
  PET<-matrix(NA,nrow = nrow(Tm),ncol = ncol(Tm))
  for (i in 1:nrow(Tm)) {#each grid
    for (j in 1:ncol(Tm)) {#each day
      PET[i,j]<-Thornthwaite(Ta=Tm[i,j],I=I[i,j],day = monthdays[j])
    }
  }
  
  #save data
  save(PET,file = paste0('./Rdata/',Datas[n],'-daily-Thornthwaite/PET.Rdata'))
  
  rm(PET,I,Tm);gc()
}

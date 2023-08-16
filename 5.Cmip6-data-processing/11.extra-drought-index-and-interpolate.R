#'''
#@author:xiyuchen
#@time:2023/1/7:15:49
#@email:Xiyc@cug.edu.cn
#'''
#'calculate the index from the array
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(Ipaper)
library(SPEI)
library(lubridate)
library(doParallel)
library(Hmisc)
setwd("D:/paper/R/5.Cmip6-data-processing/")
source('./function.R')
source('D:/paper/R/12.spei/drought-features-function.R')

load("D:/paper/R/4.area_weight/Rdata/Pre/spatio-temporal.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")

scenarios <- c('historical','hist-nat','hist-aer','hist-GHG')
indexs <- c('drought_peak','drought_intensity','drought_mean')
models <- c('ACCESS-CM2','ACCESS-ESM1-5','BCC-CSM2-MR','CanESM5','FGOALS-g3','GFDL-ESM4','IPSL-CM6A-LR',
            'MIROC6','MRI-ESM2-0','NorESM2-LM')

ngrid<-1449
nyear<-60
nmonth<-720
date1<- seq(make_date(year = 1961,month = 1,day = 1),
            make_date(year = 2020,month = 12,day = 31), by = "month")
monthdays<-monthDays(date1)#days in each month

fun <- list()
fun[[1]]<-drought_peak
fun[[2]]<-drought_year_intensity
fun[[3]]<-drought_mean

for (k in 1:length(scenarios)) {
  #k<-1
  files<-dir(paste0('./model_remapbil_data/month/',scenarios[k]),full.names = TRUE)
  finaldata1<-list()
  finaldata1[[1]]<-list()
  finaldata1[[2]]<-list()
  finaldata1[[3]]<-list()
  for (m in 1:10) { #each model
    #m<-1
    print(files[m])
    load(files[m])
    data<-data*24*3600
    attr(data,'dimnames') <- NULL
    nlon <- dim(data)[1]
    nlat <- dim(data)[2]
    
    data <- array_3dTo2d(data)
    data <- t(t(data) * monthdays) 
    
    #calculate monthly SPI
    SPI<-matrix(NA,nrow = nrow(data),ncol = nmonth)
    for (i in 1:nrow(data)) {
      SPI[i,] <- spi(data[i,], 3,na.rm = T)$fitted#SPI3,modify
    }
    rm(data);gc()
    
    for (n in 1:3) {#each index
      #n<-1
      drought <- matrix(NA,nrow = nrow(SPI),ncol = nyear)
      group<-rep(1:60,each=12)
      for (i in 1:nrow(SPI)) {
        drought[i,]<-tapply(SPI[i,], group, fun[[n]])
      }
      #resample
      R <- matrix(drought[,1],nlon)
      r <- raster(t(R[, nlat:1]), xmn= -180, xmx= 180, ymn=-90, ymx= 90)
      s <- raster(nrow=77, ncol=130, xmn=71, xmx=136, ymn=16, ymx=54.5)
      s <- resample(r, s, method = 'bilinear')
      results <- as.array(s)
      for (i in 2:ncol(drought)) {
        R <- matrix(drought[,i],nlon)
        r <- raster(t(R[, nlat:1]), xmn= -180, xmx= 180, ymn=-90, ymx= 90)
        s <- raster(nrow=77, ncol=130, xmn=71, xmx=136, ymn=16, ymx=54.5)
        s <- resample(r, s, method = 'bilinear')
        results <- abind(results,as.array(s))
      }
      attr(results,'dimnames') <- NULL
      
      index<-c()
      for (i in 77:1) {
        for (j in 1:130) {
          index <- rbind(index,results[i,j,])
        }
      }
      index <- index * tongjistationyear #spatio-temporal mask
      finaldata1[[n]][[m]] <- index
    }
  }
  
  finaldata <- finaldata1[[1]] #multi-model mean
  save(finaldata,file = paste0('./Rdata/month/',indexs[1],'/',scenarios[k],'.Rdata'))
  
  finaldata <- finaldata1[[2]] #multi-model mean
  save(finaldata,file = paste0('./Rdata/month/',indexs[2],'/',scenarios[k],'.Rdata'))
  
  finaldata <- finaldata1[[3]] #multi-model mean
  save(finaldata,file = paste0('./Rdata/month/',indexs[3],'/',scenarios[k],'.Rdata'))
  
  rm(finaldata,finaldata1,drought,index,SPI);gc()
}

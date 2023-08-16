#'''
#@author:xiyuchen
#@time:2022/6/8:16:53
#@email:Xiyc@cug.edu.cn
#'''
#'calculate the index from the array
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(Ipaper)
library(lubridate)
library(doParallel)
setwd("D:/paper/R/5.Cmip6-data-processing/")
source('./function.R')
source('D:/paper/R/10.SPI/SPI-function.R')
source('D:/paper/R/12.spei/drought-features-function.R')

load("D:/paper/R/4.area_weight/Rdata/Pre/spatio-temporal.Rdata")
#load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
load("D:/paper/R/4.area_weight/Rdata/Pre/CN05-gridindex.Rdata")
gridindex<-gridindex[[5]]

scenarios <- c('historical','hist-nat','hist-aer','hist-GHG')
indexs <- c('drought_peak','drought_intensity','drought_duration')
models <- c('ACCESS-CM2','ACCESS-ESM1-5','BCC-CSM2-MR','CanESM5','FGOALS-g3','GFDL-ESM4','IPSL-CM6A-LR',
            'MIROC6','MRI-ESM2-0','NorESM2-LM')

ngrid<-10010
nyear<-60

fun <- list()
fun[[1]]<-drought_peak
fun[[2]]<-drought_intensity
fun[[3]]<-drought_duration

for (k in 1:length(scenarios)) {
  #k<-1
  files<-dir(paste0('./model_remapbil_data/',scenarios[k]),full.names = TRUE)
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
    
    data <- array_3dTo2d(data)
    data <- data[gridindex,] 
    pre <- data
    rm(data);gc()
    
    #calculate daily SPI,4 minutes
    if(length(date)==21900){
      cl <- makeCluster(10)
      registerDoParallel(cl)
      SPI_daily_90days <- foreach(i=1:nrow(pre), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel(i) #each grid
      stopCluster(cl)
      year<-year(date)
    }else if(length(date)==21915){
      cl <- makeCluster(10)
      registerDoParallel(cl)
      SPI_daily_90days <- foreach(i=1:nrow(pre), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel_delete(i) #each grid
      stopCluster(cl)
      year<-year(date)[-seq(1155,21915,(365*4+1))]
    }
    attr(SPI_daily_90days,'dimnames') <- NULL
    save(SPI_daily_90days,file = paste0('./Rdata/SPI_daily_90days/',scenarios[k],'_',models[m],'.Rdata'))
    rm(pre);gc()
    
    for (n in 1:3) {#each index
      #n<-1
      drought <- matrix(NA,nrow = nrow(SPI_daily_90days),ncol = nyear)
      for (i in 1:nrow(SPI_daily_90days)) {
        drought[i,]<-tapply(SPI_daily_90days[i,], year, fun[[n]])
      }
      index <- matrix(NA,nrow = ngrid,ncol = nyear)
      index[gridindex,] <- drought
      #index <- index * tongjistationyear #spatio-temporal mask
      #index<-drought
      
      finaldata1[[n]][[m]] <- index
    }
  }
  
  finaldata <- finaldata1[[1]] #multi-model mean
  save(finaldata,file = paste0('./Rdata/',indexs[1],'/',scenarios[k],'.Rdata'))
  
  finaldata <- finaldata1[[2]] #multi-model mean
  save(finaldata,file = paste0('./Rdata/',indexs[2],'/',scenarios[k],'.Rdata'))
  
  finaldata <- finaldata1[[3]] #multi-model mean
  save(finaldata,file = paste0('./Rdata/',indexs[3],'/',scenarios[k],'.Rdata'))
  
  rm(finaldata,finaldata1,drought,index,SPI_daily_90days);gc()
}

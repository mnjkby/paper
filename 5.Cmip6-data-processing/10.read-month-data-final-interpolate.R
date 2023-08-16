#'''
#@author:xiyuchen
#@time:2023/1/7,13:54
#@email:Xiyc@cug.edu.cn
#'''
rm(list=ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(Ipaper)
library(ncdf4)
library(stringr)
library(abind)
library(lubridate)
library(magrittr)
library(plyr)
setwd("G:/Cmip6_pr_month_data/pr/")
source('D:/paper/R/14.calculate-cmip6-pet/function.R')

models <- c('ACCESS-CM2','ACCESS-ESM1-5','BCC-CSM2-MR','CanESM5','FGOALS-g3','GFDL-ESM4','IPSL-CM6A-LR',
            'MIROC6','MRI-ESM2-0','NorESM2-LM')
scenarios <- c('historical','hist-GHG','hist-aer','hist-nat','ssp245')
startyear<-c(1961,1961,1961,1961,2015)
endyear<-c(2014,2020,2020,2020,2020)

#2.52h
start<-Sys.time()
for (m in 1:10) {#each model
  for (n in c(1,5)) {#each scenarios
    #m<-2;n<-1
    tryCatch({
      #file path
      file <- list.files(path = 'G:/Cmip6_pr_month_data/pr/',pattern = paste('pr','Amon',models[m],scenarios[n],sep='_'))
      
      data <- bind_GCMs(file=file,var = c('pr'))#read data
      
      #select data
      lon <- data$lon;lat <- data$lat
      Ord <- c(which(lon >= 180), which(lon < 180))#translate -180 - 180  
      min<-min(which(year(data$date) == startyear[n]))
      max<-max(which(year(data$date) == endyear[n]))
      date <- data[[2]][min:max]
      data <- data[[1]][Ord,,min:max]
      
      
      save(data,date,lon,lat,file = paste0('D:/paper/R/5.Cmip6-data-processing/model_remapbil_data/month/',scenarios[n],'/',models[m],'.Rdata'))
      
      rm(data,date);gc()
    },error=function(e){
      print(paste(models[m],scenarios[n],sep='_'))
    }
    )
  }
}
end<-Sys.time()
running<-end-start
cat(running)
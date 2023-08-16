#'''
#@author:xiyuchen
#@time:2022/12/8,19:54
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
setwd("E:/cmip6-pr-day-Resample/pr/")
source('D:/paper/R/14.calculate-cmip6-pet/function.R')

models <- c('ACCESS-CM2','ACCESS-ESM1-5','BCC-CSM2-MR','CanESM5','FGOALS-g3','GFDL-ESM4','IPSL-CM6A-LR',
            'MIROC6','MRI-ESM2-0','NorESM2-LM')
scenarios <- c('historical','hist-GHG','hist-aer','hist-nat','ssp245')
startyear<-c(1961,1961,1961,1961,2015)
endyear<-c(2014,2020,2020,2020,2020)

files<-c()
for (m in 1:10) {#each model
  for (n in 1:length(scenarios)) {#each scenarios
    file<-list.files(path = 'E:/cmip6-pr-day-Resample/pr/',pattern = paste('pr','day',models[m],scenarios[n],sep='_'))
    files<-c(files,select_files_GCMs(files = file, start_year = startyear[n], end_year = endyear[n]))#select files
  }
}
rm(file,files,m,n)

#2.52h
start<-Sys.time()
for (m in 1:10) {#each model
  for (n in 1:length(scenarios)) {#each scenarios
    #m<-8;n<-2
    tryCatch({
      #file path
      file <- list.files(path = 'E:/cmip6-pr-day-Resample/pr/',pattern = paste('pr','day',models[m],scenarios[n],sep='_'))
      file <- select_files_GCMs(files = file, start_year = startyear[n], end_year = endyear[n])#select files
      
      data <- bind_GCMs_remapbil(file=file,var = 'pr')#read data
      
      #select data
      min<-min(which(year(data$date) == startyear[n]))
      max<-max(which(year(data$date) == endyear[n]))
      date <- data[[2]][min:max]
      data <- data[[1]][,,min:max]
        
       
      save(data,date,file = paste0('D:/paper/R/5.Cmip6-data-processing/model_remapbil_data/',scenarios[n],'/',models[m],'.Rdata'))
      
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
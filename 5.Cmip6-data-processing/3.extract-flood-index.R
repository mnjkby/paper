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
setwd("D:/paper/R/5.Cmip6-data-processing/")
source('./function.R')

load("D:/paper/R/4.area_weight/Rdata/Pre/spatio-temporal.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")

scenarios <- c('historical','hist-nat','hist-aer','hist-GHG')
indexs <- c('Rx1day','R95p','R20')

fun <- list()
fun[[1]]<-Rx1day
fun[[2]]<-R95p
fun[[3]]<-R20

for (k in 1:length(scenarios)) {
  #k<-2
  files<-dir(paste0('./model_remapbil_data/',scenarios[k]),full.names = TRUE)
  
  for (m in 1:3) { #each index
    #m<-1
    finaldata<-list()
    for (n in 1:10) { #each model
      #n<-2
      print(files[n])
      load(files[n])
      data<-data*24*3600
      year<-year(date)
      
      #calculate index
      index <- apply_3d(data,FUN = fun[[m]],by=year) %>% array_3dTo2d()
      #index <- index * tongjistationyear #spatio-temporal mask
      
      finaldata[[n]] <- index
      rm(data);gc()
    }
    save(finaldata,file = paste0('./Rdata/',indexs[m],'/',scenarios[k],'.Rdata'))
  }
}

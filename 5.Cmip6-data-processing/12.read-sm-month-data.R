#'''
#@author:xiyuchen
#@time:2023/2/21,20:37
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
setwd("D:/paper/R/5.Cmip6-data-processing/")

files <- c('G:/resample/mrsol_Emon_NorESM2-LM_historical_r1i1p1f1_gn_196001-196912.nc',
           'G:/resample/mrsol_Emon_NorESM2-LM_historical_r1i1p1f1_gn_197001-197912.nc',
           'G:/resample/mrsol_Emon_NorESM2-LM_historical_r1i1p1f1_gn_198001-198912.nc',
           'G:/resample/mrsol_Emon_NorESM2-LM_historical_r1i1p1f1_gn_199001-199912.nc',
           'G:/resample/mrsol_Emon_NorESM2-LM_historical_r1i1p1f1_gn_200001-200912.nc',
           'G:/resample/mrsol_Emon_NorESM2-LM_historical_r1i1p1f1_gn_201001-201412.nc',
           'G:/resample/mrsol_Emon_NorESM2-LM_ssp245_r1i1p1f1_gn_201501-202012.nc'
           )
nc<-nc_open(files[1])
depth <- ncvar_get(nc,'depth')
depth
data1 <- ncvar_get(nc,'mrsol')
data1 <- data1[,,,13:120]

data2 <- ncvar_get(nc,'mrsol')
data2 <- data2[,,,1:72]

data3 <- ncvar_get(nc,'mrsol')
data3 <- data3[,,,1:72]
nc_close(nc)

data <- abind(data1,data2,data3,along = 4) 
attr(data,'dimnames') <- NULL

rm(data1,data2,data3);gc()

test<-data[,,,1]
test1<-test[,,1]
test2<-test[,,2]
test3<-test[,,3]
test4<-test[,,4]
rm(test,test1,test2,test3,test4);gc()

data <- data[,,9,]/depth[9]#transform 0-100cm
data <- data/(1000*1)#transform unit
image.plot(data[,,1])

save(data,file = './Rdata/SM/historical/NorESM2-LM.Rdata')

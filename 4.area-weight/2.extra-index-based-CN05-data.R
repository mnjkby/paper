#'''
#@auther:xiyuchen
#@time:2023/8/18:16:53
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(Ipaper)
library(fields)
library(ncdf4)
library(terra)
library(doParallel)
library(trend)
source('D:/paper/R/all-function.R')
source('D:/paper/R/10.SPI/SPI-function.R')
setwd('D:/paper/R/4.area_weight/')
load("D:/paper/R/4.area_weight/Rdata/Pre/CN05-gridindex.Rdata")
gridindex<-gridindex[[5]]

Years <- seq(make_date(year = 1961,month = 1),
             make_date(year = 2020,month = 12,day = 31), by = "day") %>% year()
fun<-list()
fun[[1]]<-drought_peak
fun[[2]]<-drought_intensity
fun[[3]]<-drought_duration
indexs <- c('drought_peak','drought_intensity','drought_duration')

# read nc data
file <- 'D:/pan/data/00-CN051-2021/1961-2021/resample_own/CN05.1_Pre_1961_2021_daily_025x025.nc'
ncdata<-ncvar_get(nc_open(file))
ncdata<-ncdata[,,1:21915]#1961-2020

# calculate flood index
index <- apply_3d(ncdata,FUN = Rx1day,by=Years) %>% array_3dTo2d()
save(index,file = 'D:/paper/R/4.area_weight/Rdata/CN05all-index/Rx1day.Rdata')
index <- apply_3d(ncdata,FUN = R95p,by=Years) %>% array_3dTo2d()
save(index,file = 'D:/paper/R/4.area_weight/Rdata/CN05all-index/R95p.Rdata')
index <- apply_3d(ncdata,FUN = R20,by=Years) %>% array_3dTo2d()
save(index,file = 'D:/paper/R/4.area_weight/Rdata/CN05all-index/R20.Rdata')

#calculate daily SPI,40 minutes
pre<-array_3dTo2d(ncdata)
pre<-pre[gridindex,]
rm(ncdata,index);gc()

cl <- makeCluster(10)
registerDoParallel(cl)
index <- foreach(i=1:nrow(pre), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel_delete(i) #each grid
stopCluster(cl)
year<-Years[-seq(1155,21915,(365*4+1))]

attr(index,'dimnames') <- NULL
SPI_daily_90days<-matrix(NA,10010,21900)
SPI_daily_90days[gridindex,]<-index
######need attention
#SPI_daily_90days[which(is.infinite(SPI_daily_90days))]<-NA#have -Inf,need solve
save(SPI_daily_90days,file = paste0('D:/paper/R/5.Cmip6-data-processing/Rdata/SPI_daily_90days/CN05.Rdata'))
rm(pre,index);gc()

for (n in 1:3) {#each drought index
  #n<-1
  index <- matrix(NA,nrow = nrow(SPI_daily_90days),ncol = 60)
  for (j in 1:nrow(SPI_daily_90days)) {
    index[j,]<-tapply(SPI_daily_90days[j,], year, fun[[n]])
  }

  save(index,file = paste0('D:/paper/R/4.area_weight/Rdata/CN05all-index/',indexs[n],'.Rdata'))
}

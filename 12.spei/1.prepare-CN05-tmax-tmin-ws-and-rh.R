#'''
#@author:xiyuchen
#@time:2022/11/14,16:49
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(ncdf4)
library(magrittr)
library(lubridate)
setwd('D:/paper/R/12.spei/')

load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
gridindex1<-gridindex

sanweitoerwei<-function(x){
  x
}

vars<-c('Rhu','Tmax','Tmin','Win','Pre','Tm')

#rowanmes
dates <- seq(make_date(year = 1961,month = 1),
             make_date(year = 2020,month = 12,day = 31), by = "day") %>% as.character()

for (n in 1:length(vars)) {
  print(n)
  file<-paste0('D:/pan/data/00-CN051-2021/1961-2021/resample_own/CN05.1_',vars[n],'_1961_2021_daily_025x025.nc')
  ncdata<-ncvar_get(nc_open(file))
  ncdata<-ncdata[,,1:21915]#1961-2020
  
  #translate three dimensions to two dimensions 
  grid1<-pbapply::pbapply(ncdata, 3, sanweitoerwei)
  rm(ncdata);gc()
  
  grid<-grid1
  save(grid,file = paste0('./Rdata/CN05data/',vars[n],'/0.5X0.5.Rdata'))
  
  #for all grids
  gridindex<-c()
  for (i in 1:nrow(grid1)) {
    if(length(na.omit(grid1[i,]))>0){
      gridindex<-c(gridindex,i)
    }
  }
  grid<-grid1[gridindex,] %>% as.data.frame()
  colnames(grid) <- dates
  save(gridindex,file = paste0('./Rdata/CN05data/',vars[n],'/gridindex.Rdata'))
  save(grid,file = paste0('./Rdata/CN05data/',vars[n],'/wanggeyouzhi.Rdata'))
  
  grid<-grid1[gridindex1,] %>% as.data.frame()
  colnames(grid) <- dates
  save(grid,file = paste0('./Rdata/CN05data/',vars[n],'/cantiquzhibiao.Rdata'))
  
  rm(grid,grid1);gc()
  
}

image.plot(matrix(grid[,10000],nrow=130))
test<-matrix(grid[,10000],nrow=130)

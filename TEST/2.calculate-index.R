#'''
#@author:xiyuchen
#@time:2022/10/19,19:54
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(magrittr)
library(lubridate)
library(trend)
setwd('D:/paper/R/4.area_weight/')
source('D:/paper/R/Function-for-indexs.R')

#global variable
indexs<-c('CDD','CWD','R10','R20','R95p','R95pTOT','R99p','R99pTOT','Rx1','Rx5',
          'SDII','R95_percentile','R99_percentile')#n
nyear<-60
nday<-21915
Date <- seq(from = as.Date('1961-01-01'), to = as.Date('2020-12-31'), by = 1) %>% year()
#OBS and CN05
filePath<-c('D:/paper/R/3.grid/Rdata/Pre/cantiquzhibiao.Rdata',
            'D:/paper/R/12.spei/Rdata/CN05data/Pre/cantiquzhibiao.Rdata',
            'D:/paper/R/12.spei/Rdata/CN05data/Pre/wanggeyouzhi.Rdata')
Datas<-c('OBS','CN05','CN05all')#m

#function
fun <- list()
fun[[1]]<-CDD
fun[[2]]<-CWD
fun[[3]]<-R10
fun[[4]]<-R20
fun[[5]]<-R95p
fun[[6]]<-R95pTOT
fun[[7]]<-R99p
fun[[8]]<-R99pTOT
fun[[9]]<-Rx1day
fun[[10]]<-Rx5day
fun[[11]]<-SDII
fun[[12]]<-R95_percentile
fun[[13]]<-R99_percentile

#Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)

for (m in 1:length(Datas)) {
  #m<-1
  #PRE
  load(filePath[m])
  grid<-as.matrix(grid)
  attr(grid, 'dimnames') <- NULL
  
  #gridxy
  # load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
  # load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
  # gridxy <- gridxy[gridindex,]
  
  for (n in 1:length(indexs)) {
    #n<-1
    index<-matrix(NA,nrow = nrow(grid),ncol = nyear)
    for (i in 1:nrow(grid)) {
      index[i,]<-tapply(grid[i,], Date, fun[[n]])
    }
    
    # trend <- apply(index, 1, Slope)
    # trend <- as.data.frame(cbind(gridxy,trend))
    
    save(index,file=paste0('./Rdata/',Datas[m],'-index/',indexs[n],'.Rdata'))
  }
}


rm(list = ls());gc()
library(ncdf4)
library(terra)
library(Ipaper)
library(rgdal)
setwd('D:/paper/R/4.area_weight')
#source('D:/paper/R/all-function.R')

nc<-nc_open('D:/pan/data/00-CN051-2021/1961-2021/resample_own/CN05.1_Pre_1961_2021_daily_025x025.nc')
ncdata<-ncvar_get(nc)
nc_close(nc)
ncdata<-array_3dTo2d(ncdata)

gridindex<-c()
for (i in 1:10010) {
    if(length(na.omit(ncdata[i,])) > 0){
      gridindex <- c(gridindex,i)
    }
}


range <- c(71, 136,16, 54.5)
grid <- get_grid(range, cellsize = 0.5)
shp <- readOGR('D:/pan/data/huatu/projection/2.shp',stringsAsFactors = FALSE,encoding = "UTF-8")
Id <- over(grid, shp)

test<-list()
for (i in 1:4) {#4 sub-zones
  #i<-1
  grid<-which(Id[,1] == i)
  test[[i]] <- intersect(grid,gridindex)
}

grid<-which(Id[,9] == 0)
test[[5]] <- intersect(grid,gridindex)#total area

gridindex <- test
save(gridindex,file = './Rdata/Pre/CN05-gridindex.Rdata')

rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(sf)
library(maptools)
library(raster)
library(lubridate)
setwd('D:/paper/R/4.area_weight/')

#------------1.make weight--------------------------
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
#load("D:/paper/R/15.GLDAS2/Rdata/Ids.Rdata")

grid <- SpatialPixelsDataFrame(points = gridxy, data = data.frame(id = 1:nrow(gridxy)),
                               proj4string = CRS('+proj=longlat +ellps=WGS84'))
area <- raster::area(raster(grid))# area weights
area <- area@data@values
matrix<-matrix(area,nrow = 77,byrow=T)
area<-c()
for (m in 77:1) {
  for (n in 1:130) {
    area<-c(area,matrix[m,n])
  }
}

Id <- setdiff(1:10010,gridindex)
area[Id] <- NA
weights <- area/sum(area, na.rm = T)
save(weights,file = './Rdata/Pre/weight.Rdata')

length(na.omit(weights))

#-------------2.make spatio-temporal mask------------
rm(list = ls());gc()
#判断每年的缺失数据情况,低于阈值：1;超过阈值：0
tongji<-function(x){
  oldlen<-length(x)
  x<-x[!is.na(x)]
  newlen<-length(x)
  if((oldlen-newlen)/oldlen<percent){
    return(1)
  }else{
    return(NA)
  }
}

load("D:/paper/R/3.grid/Rdata/Pre/0.5X0.5.Rdata")

year <- seq(make_date(year = 1961,month = 1),
            make_date(year = 2020,month = 12,day=31), by = "day") %>% year()

percent<-0.05#modify
tongjistationyear<-matrix(NA,nrow = nrow(grid),ncol = 60)
for (i in 1:nrow(grid)) {#each grid
  tongjicol<-tapply(grid[i,], year, tongji)
  tongjistationyear[i,]<-tongjicol
}

#2d
save(tongjistationyear,file ='./Rdata/Pre/spatio-temporal.Rdata')

#3d


#------------3.make 60 years weight------------------
rm(list = ls());gc()
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
load("D:/paper/R/4.area_weight/Rdata/Pre/spatio-temporal.Rdata")

grid <- SpatialPixelsDataFrame(points = gridxy, data = data.frame(id = 1:nrow(gridxy)),
                               proj4string = CRS('+proj=longlat +ellps=WGS84'))
area <- raster::area(raster(grid))# area weights
area <- area@data@values
matrix<-matrix(area,nrow = 77,byrow=T)
area<-c()
for (m in 77:1) {
  for (n in 1:130) {
    area<-c(area,matrix[m,n])
  }
}

area <- matrix(rep(area, 60), length(area))
area[is.na(tongjistationyear)] <- NA
weights <- apply(area, 2, function(x) x/sum(x, na.rm = T))

save(weights,file = './Rdata/Pre/weight-years.Rdata')

#'''
#@author:xiyuchen
#@time:2023/2/23,21:24
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(Ipaper)
library(SPEI)
library(lubridate)
library(doParallel)
library(Hmisc)
library(trend)
library(ncdf4)
library(plyr)
library(abind)
setwd("D:/paper/R/5.Cmip6-data-processing/")
source('./function.R')
source('D:/paper/R/12.spei/drought-features-function.R')

mean_new<-function(x){
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    return(mean(x,na.rm=T))
  }
}

#calculate the time series of obs by anomaly and area-weighted
#'@x the grid_year format obs,@weight the weight for each grid
#'return the area-weighted obs time series
obs_area_weight<-function(x,weight){
  rowmean<-rowMeans(x,na.rm = T)
  x<-x-rowmean#calculate the anomaly
  afterweight<-x*weight#calculate area-weighted
  colsum<-colSums(afterweight,na.rm = T)
  return(colsum)
}

models<-dir('./Rdata/SM/historical/',full.names = T)
indexs <- c('drought_peak','drought_intensity','drought_duration')
fun <- list()
fun[[1]]<-drought_peak
fun[[2]]<-drought_intensity
fun[[3]]<-drought_duration
grids <- c('pr-grid','all-grid')

j<-2;n<-4
finaldata<-list()
for (m in 1:length(models)) {
  #m<-1
  load(models[m])
  data <- array_3dTo2d(data)
    
  if(j==1){ #pr-grid
    load('D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata')
    data <- data[gridindex,] 
  }else if(j==2){ #all-grid
    load('D:/paper/R/15.GLDAS2/Rdata/Ids.Rdata')
    data <- data[Ids,]
  }
  #SM
  # {
  #   drought <- matrix(NA,nrow = nrow(data),ncol = 60)
  #   group<-rep(1:60,each=12)
  #   for (i in 1:nrow(data)) {
  #     drought[i,]<-tapply(data[i,], group, mean_new)
  #   }
  # }
  #SSI
  {
    #calculate monthly SSI
    SSI<-matrix(NA,nrow = nrow(data),ncol = ncol(data))
    for (i in 1:nrow(data)) {
      SSI[i,] <- spi(data[i,], 3,na.rm = T)$fitted#SPI3,modify
    }
    
    #n<-3
    drought <- matrix(NA,nrow = nrow(SSI),ncol = 60)
    group<-rep(1:60,each=12)
    for (i in 1:nrow(SSI)) {
      drought[i,]<-tapply(SSI[i,], group, fun[[n]])
    }
  }
  
  finaldata[[m]] <- drought
  rm(data,drought);gc()
}

Y <- do.call(cbind, finaldata)
Y <- array(Y, dim=c(dim(finaldata[[1]]), length(finaldata)))
multimodeldata <- apply(Y, c(1,2), mean, na.rm = TRUE)

#-----------1.spatial distribution---------------------
library(raster)
library(trend)
library(ggplot2)
library(scales)
library(rcolors)
library(maptools)
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)

Trends_data<-apply(multimodeldata, 1, Slope)
summary(Trends_data)
grid <- data.frame(gridxy[Ids,],Trends_data)

prj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
china <- readShapePoly('D:/paper/R/R/china/china.shp', proj4string = prj)
china <- fortify(china)

colors <- colors_group$meteoswiss$temp_diff_18lev[c(2, 3, 4, 5, 6, 10, 14,15,16,17,18)]
colors <- rev(colors)
L <- 0.025
Breaks <- Lables <- seq(-L, L, 0.005)
Lables[1] <- paste0('< ',Breaks[1])
Lables[length(Lables)] <- paste0('> ',Breaks[length(Lables)])
Lables

grid[,3][which(grid[,3] < -L)] <- -L
grid[,3][which(grid[,3] > L)] <- L

F1<-
  ggplot()+
  geom_polygon(data = china, aes(x = long, y = lat, group = group), 
               colour = 'gray88', fill = "gray88", size = 0.5)+
  geom_raster(data = grid, aes(x, y,fill = Trends_data))+
  geom_path(data = china, aes(x = long, y = lat, group = group), 
            colour = 'darkgray', size = 0.1)+
  annotate("text",x=mean(range(grid$x)),y=Inf,label='1961-2020 SM ALL Trend',vjust=1.5,size=4.5) + 
  scale_fill_gradientn(colours = colors, na.value = 'transparent',
                       breaks = Breaks, label = Lables, limits = c(-L, L))+
  guides(fill = guide_colorbar(barwidth = 0.6, barheight = 16))+
  labs(fill='trend')+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()
  )

png(filename = './fig/final/SM/1961-2020 SM Trend.png', width = 6, height = 4, res = 500, units = 'in')
F1
dev.off()

#-----------2.time series fig----------------------
load("D:/paper/R/15.GLDAS2/Rdata/weight.Rdata")
finalweight <- obs_area_weight(multimodeldata,na.omit(weights))

finalweight<-data.frame(c(1961:2020),finalweight)
colnames(finalweight)<-c("year","value")

Slope <- paste0("Slope=",round(sens.slope(finalweight[,2])$estimate,6))
p<-sens.slope(finalweight[,2])$p.value
p <- if(p < 0.01) sprintf("italic(p) < 0.01") else if(p < 0.05) sprintf("italic(p) < 0.05") else sprintf("italic(p)== %.2f", p)

F1<-
  ggplot(finalweight,aes(x=year,y=value))+
  geom_point(size=1.8,shape=1)+
  geom_line(lwd=0.8)+
  stat_smooth(method = lm,lwd=0.8)+
  xlab('Year')+ylab('Anomalies')+
  annotate("text",x=mean(range(finalweight$year)),y=Inf,label='ALL',vjust=1.2,fontface = "bold",size=5)+
  annotate("text",x = 1985, y = -Inf,vjust=-1.2,label=Slope,size=4.5)+
  annotate("text",x = 1995, y = -Inf,vjust=-0.8,parse=TRUE,label=p,fontface = "bold",size=4.5,color ='#000000')+
  theme_bw()+
  theme(axis.title = element_text(size = rel(1), face = "bold"),
        axis.text = element_text(size = rel(1),face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

png(filename = paste0('./fig/final/SM/1961-2020 SM.png'), width = 6, height = 4, res = 500, units = 'in')
plot(F1)
dev.off() 

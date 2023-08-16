#'''
#@author:xiyuchen
#@time:2023/3/4,17:18
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
# library(ncdf4)
# library(plyr)
# library(Ipaper)
# library(abind)
# library(SPEI)
# library(raster)
library(trend)
library(ggplot2)
library(scales)
library(rcolors)
library(maptools)
setwd('D:/paper/R/12.spei/')
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")

Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)
indexs <- c('drought_peak','drought_intensity','drought_duration')
#files <- dir('./Rdata/0-10cm/',full.names = T)
Ls <- c(3,4,3)

for (n in 1:length(indexs)) {
  #n<-3
  load(paste0('./Rdata/OBS-daily-Penman/',indexs[n],'.Rdata'))
  
  Trends_data<-apply(data, 1, Slope)
  summary(Trends_data)
  grid <- data.frame(gridxy[gridindex,],Trends_data)
  
  prj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  china <- readShapePoly('D:/paper/R/R/china/china.shp', proj4string = prj)
  china <- fortify(china)
  
  colors <- colors_group$meteoswiss$temp_diff_18lev[c(2, 3, 4, 5, 6, 10, 14,15,16,17,18)]
  colors <- rev(colors)
  L <- Ls[n]
  Breaks <- Lables <- seq(-L, L, L/5)
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
              colour = 'darkgray', size = 0.1)+#
    annotate("text",x=mean(range(grid$x)),y=Inf,label=indexs[n],vjust=1.5,size=4.5)+#modify 
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
  
  png(filename = paste0('./fig/OBS-daily-Penman/',indexs[n],'.png'), width = 6, height = 4, res = 500, units = 'in')
  F1
  dev.off()
}

#'''
#@auther:xiyuchen
#@time:2022/6/8:16:53
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(trend)
library(raster)
library(ggplot2)
library(rcolors)
library(maptools)
setwd("D:/paper/R/5.Cmip6-data-processing/")

load("D:/paper/R/4.area_weight/Rdata/Pre/spatio-temporal.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
indexs <- c('drought_peak','drought_intensity','drought_mean')
labels<-c('AER','GHG','NAT','ALL','OBS','CN05')

Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)
Ls <- c(0.02,0.1,0.1,0.01,1,1)
BREAK <- c(0.004,0.02,0.02,0.002,0.2,0.2)

for (k in 1:3) { #each index
  #k<-1
  files <- dir(paste0('./Rdata/month/',indexs[k]),full.names = T)
  grid <- list()
  for (m in 1:4) { #each scenarios
    #m<-1
    load(files[m])
    multimodeldata<-matrix(0,nrow = 10010,ncol = 60)
    for (n in 1:length(finaldata)) {
      multimodeldata<-multimodeldata+finaldata[[n]]
    }
    multimodeldata<-multimodeldata/n
    
    Trends_data<-apply(multimodeldata, 1, Slope)
    grid[[m]] <- data.frame(gridxy,Trends_data)
  }
  
  #OBS
  load(paste0('D:/paper/R/10.SPI/Rdata/OBS-monthly/',indexs[k],'.Rdata'))
  Trends_data<-apply(index, 1, Slope)
  grid[[5]] <- data.frame(gridxy[gridindex,],Trends_data)
  
  #CN05
  load(paste0('D:/paper/R/10.SPI/Rdata/CN05-monthly/',indexs[k],'.Rdata'))
  index <- index * tongjistationyear[gridindex,]
  Trends_data<-apply(index, 1, Slope)
  grid[[6]] <- data.frame(gridxy[gridindex,],Trends_data)
  
  names(grid)<-labels
  
  #plot
  china <- rgdal::readOGR ('D:/pan/data/huatu/china.shp')
  projectionchina<-function(x,china){
    x<-rasterFromXYZ(x)
    crs(x)<-'+proj=longlat +datum=WGS84 +no_defs'
    x<-projectRaster(x,crs = crs(china))
    x<-as.data.frame(x,xy=TRUE)
    return(x)
  }
  
  colors <- colors_group$meteoswiss$temp_diff_18lev[c(2, 3, 4, 5, 6, 10, 14,15,16,17,18)]
  p<-list()
  L <- 0.01
  Breaks <- Lables <- seq(-L, L, 0.002)
  Lables[1] <- paste0('< ',Breaks[1])
  Lables[length(Lables)] <- paste0('> ',Breaks[length(Lables)])
  Lables
  
  #get L
  for (n in 1:6) {
    grid[[n]]<-projectionchina(grid[[n]],china)
  }
  summary(grid[[1]][3])
  summary(grid[[2]][3])
  summary(grid[[3]][3])
  summary(grid[[4]][3])
  summary(grid[[5]][3])
  summary(grid[[6]][3])
  
  for (n in 1:6) {
    grid[[n]][[3]][which(grid[[n]][[3]] < -L)] <- -L
    grid[[n]][[3]][which(grid[[n]][[3]] > L)] <- L

    p[[n]]<-
      ggplot()+
      geom_polygon(data = china, aes(x = long, y = lat, group = group),
                   colour = 'gray88', fill = "gray88", size = 0.5)+
      geom_raster(data = grid[[n]], aes(x = x, y = y, fill = Trends_data))+
      geom_path(data = china, aes(x = long, y = lat, group = group),
                colour = 'darkgray', size = 0.1)+
      annotate("text",x=mean(range(grid[[n]]$x)),y=Inf,label=labels[n],vjust=2,size=4.5) +
      scale_fill_gradientn(colours = colors, na.value = 'transparent',
                           breaks = Breaks, label = Lables, limits = c(-L, L))+
      guides(fill = guide_colorbar(barwidth = 0.6, barheight = 16))+
      labs(fill='trend (mm/yr)')+
      theme_bw()+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank()
      )
  }
  
  nineline<- rgdal::readOGR('D:/pan/data/huatu/nineline.shp')
  Fnineline<-
    ggplot()+
    geom_path(data = nineline, aes(x = long, y = lat, group = group),
              colour = 'black', size = 0.2)+
    theme_bw()+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank()
    )
  
  p3 <- ggplotGrob(Fnineline)
  xmin = 1800000
  xmax = 3400000
  ymin = 1500000
  ymax = 3300000
  #add nineline
  F1<-p[[5]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F2<-p[[4]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F3<-p[[2]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F4<-p[[1]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F5<-p[[3]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F6<-p[[6]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  
  library(gridExtra)
  library(ggpubr)
  png(filename = paste0('./ͼƬ/final/month_',indexs[k],'+FCN05.png'),11,5, units = 'in', res = 800, family = "Arial")
  ggarrange(F1,F2,F3,F4,F5,F6, ncol = 3, nrow = 2,common.legend = T,legend = 'right')
  dev.off()
}
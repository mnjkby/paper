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
# library(rgdal)
# library(sf)
library(ggplot2)
# library(maptools)
# library(scales)
library(rcolors)
#library(reshape2)
library(maptools)
setwd("D:/paper/R/5.Cmip6-data-processing/")

load("D:/paper/R/4.area_weight/Rdata/Pre/spatio-temporal.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
load("D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata")
#scenarios<-c('histghg','histaer','histnat','historical')
indexs <- c('Rx1day','R95p','R20','drought_peak','drought_intensity','drought_duration')
labels<-c('AER','GHG','NAT','ALL','OBS','CN05')

Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)
Ls <- c(0.2,0.5,0.1,0.01,1,1)
BREAK <- c(0.05,0.1,0.02,0.002,0.2,0.2)

for (k in 1:6) { #each index
  #k<-6
  files <- dir(paste0('./Rdata/',indexs[k]),full.names = T)
  grid <- list()
  for (m in 1:4) { #each scenarios
    #m<-2
    load(files[m])
    multimodeldata<-matrix(0,nrow = 10010,ncol = 60)
    for (n in 1:length(finaldata)) {
      multimodeldata<-multimodeldata+finaldata[[n]]
    }
    multimodeldata<-multimodeldata/n
    
    finaldata <- multimodeldata * tongjistationyear
    Trends_data<-apply(finaldata, 1, Slope)
    grid[[m]] <- data.frame(gridxy,Trends_data)
  }
  
  #OBS
  load(paste0('D:/paper/R/4.area_weight/Rdata/OBS-index/',indexs[k],'.Rdata'))
  Trends_data<-apply(index, 1, Slope)
  grid[[5]] <- data.frame(gridxy[gridindex,],Trends_data)
  
  #CN05
  load(paste0('D:/paper/R/4.area_weight/Rdata/CN05-index/',indexs[k],'.Rdata'))
  index <- index * tongjistationyear[gridindex,]
  Trends_data<-apply(index, 1, Slope)
  grid[[6]] <- data.frame(gridxy[gridindex,],Trends_data)
  
  names(grid)<-labels
  
  colors <- colors_group$meteoswiss$temp_diff_18lev[c(2, 3, 4, 5, 6, 10, 14,15,16,17,18)]
  p<-list()
  #L <- 1
  L <- Ls[k]
  Breaks <- Lables <- seq(-L, L, L/5)
  Lables[1] <- paste0('< ',Breaks[1])
  Lables[length(Lables)] <- paste0('> ',Breaks[length(Lables)])
  Lables
  
  #plot by transform projection
  china <- rgdal::readOGR ('D:/pan/data/huatu/china.shp')
  
  projectionchina<-function(x,china){
    x<-rasterFromXYZ(x)
    crs(x)<-'+proj=longlat +datum=WGS84 +no_defs'
    x<-projectRaster(x,crs = crs(china))
    x<-as.data.frame(x,xy=TRUE)
    return(x)
  }
  
  for (n in 1:6) {
    grid[[n]]<-projectionchina(grid[[n]],china)
    grid[[n]][[3]][which(grid[[n]][[3]] < -L)] <- -L
    grid[[n]][[3]][which(grid[[n]][[3]] > L)] <- L

    p[[n]]<-
      ggplot()+
      geom_polygon(data = china, aes(x = long, y = lat, group = group),
                   colour = 'gray88', fill = "gray88", size = 0.5)+
      geom_path(data = china, aes(x = long, y = lat, group = group),
                colour = 'darkgray', size = 0.1)+
      geom_raster(data = grid[[n]], aes(x = x, y = y, fill = Trends_data))+
      annotate("text",x=mean(range(grid[[n]]$x)),y=Inf,label=labels[n],vjust=2,size=4.5) +
      scale_x_continuous(limits=c(-3000000,3000000))+
      scale_y_continuous(limits=c(1800000,6000000))+
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
  #summary(grid[[1]][3])
  
  #plot by not transform projection
  # prj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  # china <- readShapePoly('D:/paper/R/R/china/china.shp', proj4string = prj)
  # china <- fortify(china)
  # for (n in 1:6) {
  #   grid[[n]][[3]][which(grid[[n]][[3]] < -L)] <- -L
  #   grid[[n]][[3]][which(grid[[n]][[3]] > L)] <- L
  # 
  #   p[[n]]<-
  #     ggplot()+
  #     geom_polygon(data = china, aes(x = long, y = lat, group = group),
  #                  colour = 'gray88', fill = "gray88", size = 0.5)+
  #     geom_raster(data = grid[[n]], aes(x = x, y = y, fill = Trends_data))+
  #     geom_path(data = china, aes(x = long, y = lat, group = group),
  #               colour = 'darkgray', size = 0.1)+
  #     annotate("text",x=mean(range(grid[[n]]$x)),y=Inf,label=labels[n],vjust=2,size=4.5) +
  #     scale_fill_gradientn(colours = colors, na.value = 'transparent',
  #                          breaks = Breaks, label = Lables, limits = c(-L, L))+
  #     guides(fill = guide_colorbar(barwidth = 0.6, barheight = 16))+
  #     labs(fill='trend (mm/yr)')+
  #     theme_bw()+
  #     theme(panel.background = element_blank(),
  #           panel.grid = element_blank(),
  #           axis.text = element_blank(),
  #           axis.title = element_blank(),
  #           axis.ticks = element_blank(),
  #           panel.border = element_blank()
  #     )
  # }
  
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
  #add nine line
  F1<-p[[5]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F2<-p[[4]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F3<-p[[2]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F4<-p[[1]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F5<-p[[3]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  F6<-p[[6]]+annotation_custom(p3,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)
  
  library(gridExtra)
  library(ggpubr)
  png(filename = paste0('./fig/final/',indexs[k],'+FCN05.png'),11,5, units = 'in', res = 800, family = "Arial")
  ggarrange(F1,F2,F3,F4,F5,F6, ncol = 3, nrow = 2,common.legend = T,legend = 'right')
  dev.off()
}
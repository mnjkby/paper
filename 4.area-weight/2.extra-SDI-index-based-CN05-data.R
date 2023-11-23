#'''
#@auther:xiyuchen
#@time:2023/11/11:16:53
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(Ipaper)
library(fields)
library(ncdf4)
library(ggplot2)
library(rcolors)
library(raster)
library(maptools)
library(zoo)
library(data.table)
library(cowplot)
source('D:/paper/R/all-function.R')
setwd('D:/paper/R/4.area_weight/')
# load("D:/paper/R/4.area_weight/Rdata/Pre/CN05-gridindex.Rdata")
# gridindex<-gridindex[[5]]

#---------------------1.calculate drought index--------------------------
Years <- seq(make_date(year = 1961,month = 1),
             make_date(year = 2021,month = 12,day = 31), by = "day") %>% year()
Years<-Years[-seq(1155,21915,(365*4+1))]
fun<-list()
fun[[1]]<-drought_peak
fun[[2]]<-drought_intensity
fun[[3]]<-drought_duration
indexs <- c('drought_intensity','drought_severity','drought_duration')
scales <- c(31, 91)

#read data
file <- 'D:/pan/data/00-CN051-2021/1961-2021/resample_own/CN05.1_Pre_1961_2021_daily_025x025.nc'
ncdata <- nc3(file = file, varid = 'pre')
ncdata <- ncdata[[1]][, , (-seq(1155,21915,(365*4+1)))] %>% array_3dTo2d()

# data <- ncdata[c(4000,4500,5000),]
SDI <- function(data, scale = 7, frequency = 365) {
  if(length(data[!is.na(data)]) == 0) return(rep(NA, length(data)))
  movingAverage <- frollmean(data, scale, align = 'center') %>% matrix(nrow = frequency)
  mean <- apply(movingAverage, 1, mean, na.rm = T)
  sd <- apply(movingAverage, 1, sd, na.rm = T)
  return (movingAverage - mean) / sd %>% as.vector()
  # return (sd)
}

# {
#   #plot a boxplot of the sdi range
#   test<-apply(data, 1, SDI)
#   data_plot <- data.frame(value = c(as.vector(test), as.vector(test1)), 
#                           grid = factor(rep(rep(c('grid1', 'grid2', 'grid3'), 2), each = 22265)),
#                           index = rep(c('sdi1', 'sdi2'), each = 22265 * 3))
#   F1 <- ggplot(data_plot, aes(x = interaction(index, grid), y = value, fill = index)) + 
#     stat_boxplot(geom = "errorbar", width = 0.6) +
#     geom_boxplot() + 
#     ylab('sdi') + xlab('grid') +
#     theme_bw()
#   
#   write_fig({
#     plot(F1)
#   }, 'sdi.pdf', 10, 5)
#   
#   #plot a boxplot of the sd of the sdi range
#   data_plot <- data.frame(value = c(as.vector(test), as.vector(test1)), 
#                           grid = factor(rep(rep(c('grid1', 'grid2', 'grid3'), 2), each = 365)),
#                           index = rep(c('sdi1', 'sdi2'), each = 365 * 3))
#   F1 <- ggplot(data_plot, aes(x = interaction(index, grid), y = value, fill = index)) + 
#     stat_boxplot(geom = "errorbar", width = 0.6) +
#     geom_boxplot() + 
#     ylab('sd') + xlab('grid') +
#     theme_bw()
#   
#   write_fig({
#     plot(F1)
#   }, 'sd.pdf', 10, 5)
#   
#   #原始数据-420个值的平均然后除以标准偏差
#   SDI <- function(data, scale = 7, frequency = 365) {
#     if(length(data[!is.na(data)]) == 0) return(rep(NA, length(data)))
#     mean <- c()
#     sd <- c()
#     for (i in 1:365) {
#       #i <- 4
#       index <- c()
#       for (j in 1 : (length(data) / frequency)) {
#         index <- c(index, (i - (scale - 1) / 2 + frequency * (j - 1)) : (i + (scale - 1) / 2 + frequency * (j - 1)))
#       }
#       index <- index[index %in% 1:length(data)]
#       
#       mean <- c(mean, mean(data[index], na.rm = T))
#       sd <- c(sd, sd(data[index], na.rm = T))
#     }
#     data <- matrix(data, nrow = 365)
#     
#     return (data - mean) / sd %>% as.vector()
#     # return (sd)
#   }
#   
#   # test1<-apply(data, 1, SDI)
# }

for (m in scales) {
  print(m)
  sdi <- apply(ncdata, 1, function(data, scale = m, frequency = 365) {
    if(length(data[!is.na(data)]) == 0) return(rep(NA, length(data)))
    movingAverage <- frollmean(data, scale, align = 'center') %>% matrix(nrow = frequency)
    mean <- apply(movingAverage, 1, mean, na.rm = T)
    sd <- apply(movingAverage, 1, sd, na.rm = T)
    return (movingAverage - mean) / sd %>% as.vector()
    # return (sd)
  }) %>% t()
  save(sdi, file = paste0('./Rdata/SDI_', m, 'day.Rdata'))

  indexs <- list()
  for (n in 1:3) {
    print(n)
    index <- matrix(NA,nrow = nrow(sdi),ncol = 61)
    for (j in 1:nrow(sdi)) {
      index[j,]<-tapply(sdi[j,], Years, fun[[n]])
    }
    indexs[[n]] <- index
  }

  save(indexs,file = paste0('./Rdata/sdi_', m, '.Rdata'))
}

#---------------------2.plot------------------------------------------
rm(list = ls());gc()
source('D:/paper/R/all-function.R')
load('./Rdata/sdi_91.Rdata')
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
load("D:/paper/R/4.area_weight/Rdata/Pre/weight-CN05all.Rdata") 

Fig <- list()
Time <- list()
# Ls<-c(0.03,2.0,0.8)
# Ls<-c(0.01,2.0,0.8)
Ls<-c(0.03,1.4,2.0)
Ls<-c(0.07,3.0,14.0)
labels <- c('(a) drought intensity', '(b) drought severity', '(c) drought duration')

for (n in 1:3) {#each drought index
  #n <- 1
  index <- indexs[[n]]
  # -- a. plot time series
  finalweight<-obs_area_weight(index,weights)
  finalweight<-data.frame(c(1961:2021),finalweight)
  colnames(finalweight)<-c("year","value")
  
  slope <- paste0("Slope=",round(sens.slope(finalweight[,2])$estimate,5))
  p <- sens.slope(finalweight[,2])$p.value
  p <- if(p < 0.01) sprintf("italic(p) < 0.01") else if(p < 0.05) sprintf("italic(p) < 0.05") else sprintf("italic(p)== %.2f", p)
  
  Time[[n]] <-
    ggplot(finalweight,aes(x=year,y=value))+
    geom_point(size=1.8,shape=1)+
    geom_line(lwd=0.8)+
    stat_smooth(method = lm,lwd=0.8)+
    xlab('Year')+ylab('Anomalies')+
    annotate("text",x=mean(range(finalweight$year)),y=Inf,label=labels[n],vjust=1.2,fontface = "bold",size=5)+
    annotate("text",x = 1985, y = -Inf,vjust=-1.2,label=slope,size=4.5)+
    annotate("text",x = 1995, y = -Inf,vjust=-0.8,parse=TRUE,label=p,fontface = "bold",size=4.5,color ='#000000')+
    theme_bw()+
    theme(axis.title = element_text(size = rel(1), face = "bold"),
          axis.text = element_text(size = rel(1),face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # png(filename = paste0('./fig/', indexs[n], '_time.png'), width = 6, height = 4, res = 500, units = 'in')
  # plot(F2)
  # dev.off() 
  
  # -- b. plot spatial pattern
  Trends_data= apply(index, 1, Slope)
  pVale_data = apply(index, 1, pValue)

  pValueGrid <- gridxy[pVale_data < 0.05 & !is.na(pVale_data),]
  grid <- data.frame(gridxy,Trends_data= Trends_data)
  summary(grid$Trends_data)

  colors <- colors_group$meteoswiss$temp_diff_18lev[c(2, 3, 4, 5, 6, 10, 14,15,16,17,18)]
  L <- Ls[n]
  Breaks <- Lables <- signif(seq(-L, L, L/5), digits = 3)
  Lables[1] <- paste0('< ',Breaks[1])
  Lables[length(Lables)] <- paste0('> ',Breaks[length(Lables)])
  # Lables

  #plot
  china <- readShapePoly('D:/pan/data/chinalihui/Chinacaijian.shp')
  china <- fortify(china)

  grid[[3]][which(grid[[3]] < -L)] <- -L
  grid[[3]][which(grid[[3]] > L)] <- L

  P <-
    ggplot()+
    geom_polygon(data = china, aes(x = long, y = lat, group = group), colour = 'gray88', fill = "gray88", size = 0.5)+
    geom_raster(data = grid, aes(x = x, y = y, fill = Trends_data))+
    geom_point(data = pValueGrid, aes(x = x, y = y), size = 0.01, shape = 16)+
    geom_path(data = china, aes(x = long, y = lat, group = group),
              colour = 'darkgray', size = 0.1)+
    annotate("text",x=80,y=Inf,label=labels[n],vjust=2,size=4.5) +
    scale_fill_gradientn(colours = colors, na.value = 'transparent',
                         breaks = Breaks, label = Lables, limits = c(-L, L))+
    guides(fill = guide_colorbar(barwidth = 0.6, barheight = 16))+
    # labs(fill='trend (mm/yr)')+
    labs(fill='trend')+
    theme_bw()+
    scale_x_continuous(breaks = seq(70, 140,20),labels = c("70°E",'90°E','110°E',"130°E"))+
    scale_y_continuous(breaks = seq(20, 50, 10),labels = c("20°N","30°N","40°N","50°N"))+
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          # axis.text = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = rel(0.8)),
          # axis.ticks = element_blank(),
    )

  nineline<- rgdal::readOGR('D:/pan/data/chinalihui/nineline_hebing.shp')
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

  Fnineline <- ggplotGrob(Fnineline)
  xmin = 123.5
  xmax = 138.5
  ymin = 15
  ymax = 30
  #add nine line
  P <- P+annotation_custom(Fnineline,xmin = xmin,xmax = xmax,ymin = ymin,ymax = ymax)

  name1 <- c("SD", "D", "I", "SI")
  y <- c(mean(Trends_data < 0 & pVale_data < 0.05, na.rm = TRUE),
         mean(Trends_data < 0 & pVale_data >= 0.05, na.rm = TRUE),
         mean(Trends_data > 0 & pVale_data >= 0.05, na.rm = TRUE),
         mean(Trends_data > 0 & pVale_data < 0.05, na.rm = TRUE))
  per <- data.frame(name1, y = y*100)
  per$name1 <- factor(per$name1, levels = name1, ordered = TRUE)

  ps <- ggplot(per, aes(x = 1:4, y = y, fill = name1))+
    geom_bar(stat = 'identity',width = 0.45, position = 'dodge')+
    theme_cowplot()+
    # theme_bw()+
    scale_fill_manual(values = c("#2400D8","#3D87FF","#FF3D3D","#A50021"))+
    scale_y_continuous(limits = c(0,70),expand = c(0,0))+
    scale_x_continuous(breaks = c(1,2,3,4),expand = c(0,0.01),labels = c("SD", "D", "I", "SI"),limits = c(0,5))+
    ylab('Percentage (%)')+
    theme(panel.grid = element_blank(),
          axis.line = element_line(arrow = arrow(length = unit(0.15, "cm"), type = "closed")),
          axis.title.x = element_blank(),
          legend.position = 'none',
          axis.text = element_text(face = "bold", size = rel(0.55)),
          axis.title.y = element_text(face = "bold",size = rel(0.55)))+
    coord_fixed(ratio = 1/14)

  Fig[[n]] <- ggdraw()+
    draw_plot(P)+
    draw_plot(ps, scale = 0.4, x = -0.27, y = -0.25)

  # png(filename = paste0('./fig/', indexs[n] ,'.png'),6,4, units = 'in', res = 800, family = "Arial")
  # plot(F1)
  # dev.off()
}

#combination graph
library(gridExtra)
library(ggpubr)
png(filename = paste0('./fig/spatial_pattern91.png'),5,11, units = 'in', res = 800, family = "Arial")
ggarrange(Fig[[1]],Fig[[2]],Fig[[3]], ncol = 1, nrow = 3)
dev.off()

png(filename = paste0('./fig/time_series91.png'),8,11, units = 'in', res = 800, family = "Arial")
ggarrange(Time[[1]],Time[[2]],Time[[3]], ncol = 1, nrow = 3)
dev.off()

#'''
#@author:xiyuchen
#@time:2023/2/21,20:37
#@email:Xiyc@cug.edu.cn
#'''
rm(list=ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
setwd("D:/paper/R/5.Cmip6-data-processing/")
files <- dir('./model_remapbil_data/month-mrsos/historical/',full.names = T)

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

grid <- list()
for (m in 1:9) { #each scenarios
  #m<-1
  load(files[m])
  attr(data,'dimension') <- NULL
  data <- array_3dTo2d(data)
  data <- data/(1000*0.1)#transform unit
  
  drought <- matrix(NA,nrow = nrow(data),ncol = 60)
  group<-rep(1:60,each=12)
  for (i in 1:nrow(data)) {
    drought[i,]<-tapply(data[i,], group, mean_new)
  }
  
  load('D:/paper/R/15.GLDAS2/Rdata/gridindex.Rdata')
  drought <- drought[gridindex,]
  
  grid[[m]] <- drought
}

Y <- do.call(cbind, grid)
Y <- array(Y, dim=c(dim(grid[[1]]), length(grid)))
multimodeldata <- apply(Y, c(1,2), mean, na.rm = TRUE)

load("D:/paper/R/15.GLDAS2/Rdata/weight.Rdata")
index <- obs_area_weight(multimodeldata,weights[gridindex])

finalweight<-data.frame(c(1961:2020),index)
colnames(finalweight)<-c("year","value")

Slope <- paste0("Slope=",round(sens.slope(finalweight[,2])$estimate,5))
p<-sens.slope(finalweight[,2])$p.value
p <- if(p > 0.05) sprintf("italic(p)== %.2f", p) else sprintf("italic(p) < 0.05")

F1<-
  ggplot(finalweight,aes(x=year,y=value))+
  geom_point(size=1.8,shape=1)+
  geom_line(lwd=0.8)+
  stat_smooth(method = lm,lwd=0.8)+
  xlab('Year')+ylab('Anomalies')+
  annotate("text",x=mean(range(finalweight$year)),y=Inf,label='ALL',vjust=1.2,fontface = "bold",size=5)+#modify
  annotate("text",x = 1985, y = -Inf,vjust=-1.2,label=Slope,size=4.5)+
  annotate("text",x = 1995, y = -Inf,vjust=-0.8,parse=TRUE,label=p,fontface = "bold",size=4.5,color ='#000000')+
  theme_bw()+
  theme(axis.title = element_text(size = rel(1), face = "bold"),
        axis.text = element_text(size = rel(1),face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

png(filename = paste0('./fig/final/SM/0-10cm/ALL.png'), width = 6, height = 4, res = 500, units = 'in')
plot(F1)
dev.off()

#--------------3.plot spatial--------------------------
data<-multimodeldata
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")

Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)

Trends_data<-apply(data, 1, Slope)
summary(Trends_data)
grid <- data.frame(gridxy[gridindex,],Trends_data)

prj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
china <- readShapePoly('D:/paper/R/R/china/china.shp', proj4string = prj)
china <- fortify(china)

colors <- colors_group$meteoswiss$temp_diff_18lev[c(2, 3, 4, 5, 6, 10, 14,15,16,17,18)]
colors <- rev(colors)
L <- 0.0003
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
  annotate("text",x=mean(range(grid$x)),y=Inf,label='ALL',vjust=1.5,size=4.5)+#modify 
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

png(filename = paste0('./fig/final/SM/0-10cm/ALL trend.png'), width = 6, height = 4, res = 500, units = 'in')
F1
dev.off()

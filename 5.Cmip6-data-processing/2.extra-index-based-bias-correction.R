rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(Ipaper)
library(fields)
library(ncdf4)
library(terra)
library(doParallel)
library(trend)
source('D:/paper/R/all-function.R')
source('D:/paper/R/10.SPI/SPI-function.R')
setwd('D:/paper/R/18.bias-correction/')
# load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
# load("D:/paper/R/4.area_weight/Rdata/Pre/weight-CN05all.Rdata")
# weights[which(weights>0)]<-1

files <- dir(path = 'F:/CMIP6_China_pr/historical_adj/',full.names = F)
models <- extract_model_name(files = files)
files <- paste0('F:/CMIP6_China_pr/historical_adj/',files)
fun<-list()
fun[[1]]<-drought_peak
fun[[2]]<-drought_intensity
fun[[3]]<-drought_duration
Rx1days<-list()
R95ps<-list()
R20s<-list()
droughts<-list(drought_peaks=list(),drought_intensitys=list(),drought_durations=list())
for (i in 1:18) {#each model
  #i<-1
  print(i)
  tryCatch({
    # read nc data
    nc<-nc_open(files[i])
    date<-nc_date(nc) 
    Years <- str_sub(date, 1, 4) %>% as.numeric()
    min<-min(which(Years==1961))
    max<-max(which(Years==2014))
    nlon<-nc$dim$lon$len
    nlat<-nc$dim$lat$len
    ncdata<-ncvar_get(nc,start = c(1,1,min),count = c(nlon,nlat,(max-min+1)))
    Years<-Years[min:max]
    nc_close(nc)
    print(summary(as.vector(ncdata)))
    #Inf to 0
    # ncdata[which(is.infinite(ncdata))] <- 0
    ncdata[which(ncdata<0)] <- 0
    # ncdata[which(ncdata>5000)] <- 0
    print(summary(as.vector(ncdata)))
    # ncdata<-array_3dTo2d(ncdata)
    # which(ncdata==457649408)
    
    if(length(which(ncdata<0))){
      next
    }else{
      # calculate flood index
      index <- apply_3d(ncdata,FUN = Rx1day,by=Years) %>% array_3dTo2d()
      Rx1days[[i]]<-index
      index <- apply_3d(ncdata,FUN = R95p,by=Years) %>% array_3dTo2d()
      R95ps[[i]]<-index
      index <- apply_3d(ncdata,FUN = R20,by=Years) %>% array_3dTo2d()
      R20s[[i]]<-index

      #calculate daily SPI,40 minutes
      pre<-array_3dTo2d(ncdata)
      gridindex<-rowMeans(index,na.rm = T)
      gridindex<-which(!is.na(gridindex))
      pre<-pre[gridindex,]
      rm(ncdata,index);gc()
      if(length(Years)==19710){
        cl <- makeCluster(10)
        registerDoParallel(cl)
        index <- foreach(i=1:nrow(pre), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel(i) #each grid
        stopCluster(cl)
      }else if(length(Years)==19723){
        cl <- makeCluster(10)
        registerDoParallel(cl)
        index <- foreach(i=1:nrow(pre), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel_delete(i) #each grid
        stopCluster(cl)
        Years<-Years[-seq(1155,21915,(365*4+1))]
      }

      attr(index,'dimnames') <- NULL
      SPI_daily_90days<-matrix(NA,10010,19710)
      SPI_daily_90days[gridindex,]<-index
      #SPI_daily_90days[which(is.infinite(SPI_daily_90days))]<-NA#have -Inf,need solve
      save(SPI_daily_90days,file = paste0('./Rdata/ALL/SPI_daily_90days/',models[i],'.Rdata'))
      rm(pre,index);gc()

      for (n in 1:3) {#each drought index
        #n<-1
        index <- matrix(NA,nrow = nrow(SPI_daily_90days),ncol = 54)
        for (j in 1:nrow(SPI_daily_90days)) {
          index[j,]<-tapply(SPI_daily_90days[j,], Years, fun[[n]])
        }

        droughts[[n]][[i]] <- index
      }
      rm(index,SPI_daily_90days);gc()
    }
  },error=function(e){
    print(-i)
  }
  )
}

data<-Rx1days
save(data,file = './Rdata/ALL/Rx1day.Rdata')
data<-R95ps
save(data,file = './Rdata/ALL/R95p.Rdata')
data<-R20s
save(data,file = './Rdata/ALL/R20.Rdata')
data<-droughts[[1]]
save(data,file = './Rdata/ALL/drought_peak.Rdata')
data<-droughts[[2]]
save(data,file = './Rdata/ALL/drought_intensity.Rdata')
data<-droughts[[3]]
save(data,file = './Rdata/ALL/drought_duration.Rdata')

#----------------CN05-------------------------------
rm(list = ls());gc()
files <- "D:/paper/R/18.bias-correction/CN05.1_Pre_1961_2021_daily_G050.nc"

fun<-list()
fun[[1]]<-drought_peak
fun[[2]]<-drought_intensity
fun[[3]]<-drought_duration
Rx1days<-list()
R95ps<-list()
R20s<-list()
droughts<-list(drought_peaks=list(),drought_intensitys=list(),drought_durations=list())

i<-1

nc<-nc_open(files[i])
date<-nc_date(nc) 
Years <- str_sub(date, 1, 4) %>% as.numeric()
min<-min(which(Years==1961))
max<-max(which(Years==2014))
nlon<-nc$dim$lon$len
nlat<-nc$dim$lat$len
ncdata<-ncvar_get(nc,start = c(1,1,min),count = c(nlon,nlat,(max-min+1)))
Years<-Years[min:max]
nc_close(nc)

# calculate flood index
index <- apply_3d(ncdata,FUN = Rx1day,by=Years) %>% array_3dTo2d()
Rx1days[[i]]<-index
index <- apply_3d(ncdata,FUN = R95p,by=Years) %>% array_3dTo2d()
R95ps[[i]]<-index
index <- apply_3d(ncdata,FUN = R20,by=Years) %>% array_3dTo2d()
R20s[[i]]<-index

#calculate daily SPI,40 minutes
pre<-array_3dTo2d(ncdata)
gridindex<-rowMeans(index,na.rm = T)
gridindex<-which(!is.na(gridindex))
pre<-pre[gridindex,]
rm(ncdata,index);gc()
if(length(Years)==19710){
  cl <- makeCluster(10)
  registerDoParallel(cl)
  index <- foreach(i=1:nrow(pre), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel(i) #each grid
  stopCluster(cl)
}else if(length(Years)==19723){
  cl <- makeCluster(10)
  registerDoParallel(cl)
  index <- foreach(i=1:nrow(pre), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel_delete(i) #each grid
  stopCluster(cl)
  Years<-Years[-seq(1155,21915,(365*4+1))]
}

attr(index,'dimnames') <- NULL
SPI_daily_90days<-matrix(NA,11200,19710)
SPI_daily_90days[gridindex,]<-index
save(SPI_daily_90days,file = './Rdata/obs/SPI_daily_90days.Rdata')
rm(pre,index);gc()

for (n in 1:3) {#each drought index
  #n<-1
  index <- matrix(NA,nrow = nrow(SPI_daily_90days),ncol = 54)
  for (j in 1:nrow(SPI_daily_90days)) {
    index[j,]<-tapply(SPI_daily_90days[j,], Years, fun[[n]])
  }
  
  droughts[[n]][[i]] <- index
}
rm(index,SPI_daily_90days);gc()

data<-Rx1days
save(data,file = './Rdata/obs/Rx1day.Rdata')
data<-R95ps
save(data,file = './Rdata/obs/R95p.Rdata')
data<-R20s
save(data,file = './Rdata/obs/R20.Rdata')
data<-droughts[[1]]
save(data,file = './Rdata/obs/drought_peak.Rdata')
data<-droughts[[2]]
save(data,file = './Rdata/obs/drought_intensity.Rdata')
data<-droughts[[3]]
save(data,file = './Rdata/obs/drought_duration.Rdata')

#-----------------------2.plot---------------
rm(list = ls());gc()
library(ggplot2)
library(rcolors)
library(raster)
library(maptools)
load("D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata")
Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)
labels<-c('CN05','ALL')
indexs<-c('Rx1day','R95p','R20','drought_peak','drought_intensity','drought_duration')
Ls<-c(0.2,0.5,0.1,0.01,1,1)
model<-c(1,2,5,6,7,8,17,18,30,32,33)

for (m in 1:6) {
  #m<-1
  grid<-list()
  #CN05
  #load("D:/paper/R/18.bias-correction/Rdata/obs/droughts.Rdata")
  load(paste0('D:/paper/R/18.bias-correction/Rdata/obs/',indexs[m],'.Rdata'))
  data <- array(as.vector(data[[1]]),c(140,80,54))
  lon <- seq(71.25,135.75,0.5)
  lat <- seq(16.25,54.25,0.5)
  index_lon<-which(seq(70.25,139.75,0.5) %in% lon)
  index_lat<-which(seq(15.25,54.75,0.5) %in% lat)
  CN05<-data[index_lon,index_lat,] %>% array_3dTo2d()
  
  #ALL
  load(paste0('D:/paper/R/18.bias-correction/Rdata/ALL/',indexs[m],'.Rdata'))
  data<-data[model]
  #data<-data1[8]
  Y <- do.call(cbind, data)
  Y <- array(Y, dim=c(dim(data[[1]]), (ncol(Y)/54)))
  multimodeldata <- apply(Y, c(1,2), mean, na.rm = TRUE)
  
  grid[[1]] <- data.frame(gridxy,Trends_data= apply(CN05,1,Slope))
  grid[[2]] <- data.frame(gridxy,Trends_data= apply(multimodeldata, 1, Slope))
  names(grid) <- labels
  summary(grid$CN05$Trends_data)
  summary(grid$ALL$Trends_data)
  
  colors <- colors_group$meteoswiss$temp_diff_18lev[c(2, 3, 4, 5, 6, 10, 14,15,16,17,18)]
  p<-list()
  L <- Ls[m]
  #L <- 0.1
  Breaks <- Lables <- seq(-L, L, L/5)
  Lables[1] <- paste0('< ',Breaks[1])
  Lables[length(Lables)] <- paste0('> ',Breaks[length(Lables)])
  Lables
  
  #plot
  prj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  china <- readShapePoly('D:/paper/R/R/china/china.shp', proj4string = prj)
  china <- fortify(china)
  for (n in 1:2) {
    #n<-2
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
  
  # png(filename = paste0('./fig/CESM2.png'),6,4, units = 'in', res = 800, family = "Arial")
  # p[[2]]
  # dev.off()
  
  library(gridExtra)
  library(ggpubr)
  png(filename = paste0('./fig/',indexs[m],'.png'),11,5, units = 'in', res = 800, family = "Arial")
  plot(ggarrange(p[[1]],p[[2]], ncol = 2, nrow = 1,common.legend = T,legend = 'right'))
  dev.off()
}


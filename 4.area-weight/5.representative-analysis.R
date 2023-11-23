#'''
#@auther:xiyuchen
#@time:2023/11/13:16:53
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(ggplot2)
library(Ipaper)
library(fields)
library(ncdf4)
library(ggplot2)
library(rcolors)
library(raster)
library(maptools)
library(zoo)
library(data.table)
setwd('D:/paper/R/4.area_weight/')
source('D:/paper/R/all-function.R')
load('./Rdata/sdi1.Rdata')
indexs1 <- indexs
load('./Rdata/sdi2.Rdata')

Trends_data1 <- llply(indexs1, function(x) {
  apply(x, 1, Slope)
})

pVale_data1 <- llply(indexs1, function(x) {
  apply(x, 1, pValue)
})

Trends_data2 <- llply(indexs, function(x) {
  apply(x, 1, Slope)
})

pVale_data2 <- llply(indexs, function(x) {
  apply(x, 1, pValue)
})


#显著增加，显著减少，一增一减，不显著
#一个显著增加，另一个没有明显变化
index <- which(Trends_data1[[1]] > 0 & pVale_data1[[1]] < 0.05 & Trends_data2[[1]] == 0)[1]
Trends_data1[[1]][1884]
pVale_data1[[1]][1884]
Trends_data2[[1]][1884]

#------------------plot variation of drought intensity in sdi1 and sdi2---------------------------------
data <- data.frame(value = c(indexs1[[1]][index, ], indexs[[1]][index, ]),
                   year = rep(1961 : 2021, 2), 
                   group = rep(c('sdi1_drought_intensity', 'sdi2_drought_intensity'), each = 61))
Time <-
  ggplot(data,aes(x=year,y=value, colour = group))+
  geom_point(size=1.8,shape=1)+
  geom_line(lwd=0.5)+
  stat_smooth(method = lm,lwd=0.8)+
  xlab('year')+ylab('value')+
  theme_bw()+
  theme(axis.title = element_text(size = rel(1), face = "bold"),
        axis.text = element_text(size = rel(1),face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

png(filename = paste0('./fig/time_trend.png'), width = 6, height = 4, res = 500, units = 'in')
plot(Time)
dev.off()

#---------------------plot sdi1, sdi2 and pre change--------------------------------------
load('./Rdata/SDI_7day1.Rdata')
SDI1 <- sdi
load('./Rdata/SDI_7day2.Rdata')
SDI2 <- sdi

file <- 'D:/pan/data/00-CN051-2021/1961-2021/resample_own/CN05.1_Pre_1961_2021_daily_025x025.nc'
ncdata <- nc3(file = file, varid = 'pre')
ncdata <- ncdata[[1]][, , (-seq(1155,21915,(365*4+1)))] %>% array_3dTo2d()

n <- 4
numbers <- 250
data <- data.frame(value = c(SDI1[index, ][(1 + (n-1) * numbers) : (n * numbers)], 
                             SDI2[index, ][(1 + (n-1) * numbers) : (n * numbers)], 
                             ncdata[index, ][(1 + (n-1) * numbers) : (n * numbers)]),
                   year = rep(1 : numbers, 3), group = rep(c('sdi1', 'sdi2', 'pre'), each = numbers))
Time <-
  ggplot(data,aes(x=year,y=value, colour = group))+
  geom_point(size=1.8,shape=1)+
  geom_line(lwd=0.1)+
  # stat_smooth(method = lm,lwd=0.8)+
  geom_hline(yintercept = (-0.8), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  geom_hline(yintercept = (-4), linetype = "dashed", color = "red") +
  xlab('day')+ylab('value')+
  theme_bw()+
  theme(axis.title = element_text(size = rel(1), face = "bold"),
        axis.text = element_text(size = rel(1),face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

png(filename = paste0('./fig/time.png'), width = 11, height = 4, res = 500, units = 'in')
plot(Time)
dev.off()

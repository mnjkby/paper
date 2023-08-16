#'''
#@author:xiyuchen
#@time:2022/11/12,9:49
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(elevatr)
library(magrittr)
library(lubridate)
library(parallel)
library(doParallel)
setwd('D:/paper/R/12.spei/')

Rn <- function(Tmax,Tmin,RH,Date,Z,lat){
  #   Date <- rownames(Data[2,])
  first=as.Date(paste(substr(Date,1,4), "01","01",sep="/"),format="%Y/%m/%d")
  j<-julian(Date,first) %>% as.numeric()
  j <- j+1 #the day of the year
  #calculate short wave radiation
  Gsc <- 0.0820
  dr <- 1+0.033*cos(2*pi/365*j)
  x <- pi/180*lat
  y <- 0.409*sin(2*pi/365*j-1.39)
  ws <- acos(-tan(x)*tan(y))
  Ra <- 24*60/pi*Gsc*dr*(ws*sin(x)*sin(y)+cos(x)*cos(y)*sin(ws))
  Rs <-0.5496*Ra
  Rns <- (1-0.23)*Rs
  #calculate long wave radiation
  ea <- RH/100*(0.6108*exp(17.27*Tmax/(Tmax+273.3))+0.6108*exp(17.27*Tmin/(Tmin+273.3)))/2
  Rso <- (0.75+2*10^(-5)*Z)*Ra
  Rnl <- 4.903*10^(-9)*((Tmax+273.16)^4+(Tmin+273.16)^4)/2*(0.34-0.14*ea^(0.5))*(1.35*Rs/Rso-0.35)
  Rn <- Rns-Rnl
  return(Rn)
}

#need Tmax,Tmin,WS,RH,Date,Z,lat
Penman <- function(Tmax,Tmin,WS,RH,Rn,Z){
  Tm <- (Tmax + Tmin)/2
  delta <- 4098*(0.6108*exp(17.27*Tm/(Tm+237.3)))/(Tm+237.3)^2
  gama <- 0.000665*101.3*((293-0.0065*Z)/293)^5.26
  es <- (0.6108*exp(17.27*Tmax/(Tmax+237.3))+0.6108*exp(17.27*Tmin/(Tmin+237.3)))/2
  ea <- RH/100*(0.6108*exp(17.27*Tmax/(Tmax+237.3))+0.6108*exp(17.27*Tmin/(Tmin+237.3)))/2
  PET <- (0.408*delta*Rn+gama*900/(Tm+273)*WS*(es-ea))/(delta+gama*(1+0.34*WS))
  return(PET)
}

#parallel foreach
row_parallel<-function(i){
  pet<-matrix(NA,nrow = 1,ncol = ncol(Tmax))
  for (j in 1:ncol(Tmax)) {#each day
    Rn0<-Rn(Tmax = Tmax[i,j],Tmin = Tmin[i,j],RH=RH[i,j],Date = Dates[j],Z=z[i],lat = lat[i])
    pet[1,j]<-Penman(Tmax = Tmax[i,j],Tmin = Tmin[i,j],WS=WS[i,j],RH=RH[i,j],Rn=Rn0,Z=z[i])
  }
  pet
}

#gridxy
load('D:/paper/R/3.grid/Rdata/Pre/gridxy.Rdata')
load('D:/paper/R/3.grid/Rdata/Pre/gridindex.Rdata')
gridxy<-gridxy[gridindex,]

#calculate Z
z <- get_elev_point(gridxy, prj = "EPSG:4326", src = "aws") %>% as.data.frame()
z<-z[,1]

#lat
lat<-gridxy[,2]

#Date
Dates <- seq(make_date(year = 1961,month = 1,day = 1),
             make_date(year = 2020,month = 12,day = 31), by = "day") 

#OBS and CN05
filePath<-c('D:/paper/R/3.grid/Rdata/','D:/paper/R/12.spei/Rdata/CN05data/')
Datas<-c('OBS','CN05')#n

for (n in 1:length(filePath)) {
  print(n)
  
  # -- a.prepare Tmax,Tmin,WS and RH data---------------------
  #tmax
  load(paste0(filePath[n],'Tmax/cantiquzhibiao.Rdata'))
  Tmax<-as.matrix(grid)
  attr(Tmax, 'dimnames') <- NULL
  #tmin
  load(paste0(filePath[n],'Tmin/cantiquzhibiao.Rdata'))
  Tmin<-as.matrix(grid)
  attr(Tmin, 'dimnames') <- NULL
  #ws
  load(paste0(filePath[n],'Win/cantiquzhibiao.Rdata'))
  WS<-as.matrix(grid)
  attr(WS, 'dimnames') <- NULL
  #RH
  load(paste0(filePath[n],'Rhu/cantiquzhibiao.Rdata'))
  RH<-as.matrix(grid)
  attr(RH, 'dimnames') <- NULL
  rm(grid)
  
  # -- b.calculate PET by penman formula,parallel,6 minutes
  cl <- makeCluster(10)
  registerDoParallel(cl)
  PET <- foreach(i=1:nrow(Tmax), .combine='rbind',.packages = 'magrittr') %dopar% row_parallel(i) #each grid
  stopCluster(cl)
  
  # -- c.save data
  save(PET,file = paste0('./Rdata/',Datas[n],'-daily-Penman/PET.Rdata'))
  
  rm(cl,PET,RH,Tmax,Tmin,WS);gc()
}



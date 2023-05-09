#'''
#@author:xiyuchen
#@time:2022/12/9,19:18
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(ncdf4)
library(terra)
setwd('F:/add2/')
files<-dir('./')

#judge
y<-rep(NA,length(files))

start<-Sys.time()

for (i in 1:length(files)) {
  tryCatch({
      nc<-nc_open(files[i])
      len<-nc$dim$time$len
      nlon<-nc$dim$lon$len
      nlat<-nc$dim$lat$len
      #ncdata<-ncvar_get(nc,var = 'pr')
      ncdata<-ncvar_get(nc,start = c(nlon,nlat,1),count = c(1,1,len))
      nc_close(nc)
      y[i]<-i
      rm(ncdata);gc()
    },error=function(e){
      nc_close(nc)
      print(i)
    }
  )
}

end<-Sys.time()
running<-end-start
cat(running)

index<-which(is.na(y))
test<-as.data.frame(index)

#remove error files
file.remove(files[index])

#'''
#@author:xiyuchen
#@time:2022/6/7:16:53
#@email:Xiyc@cug.edu.cn
#'''
rm(list = ls());gc()
options(scipen = 200) 
options(max.print=100000) 
memory.limit(size = 2000000)
library(stringr)
library(abind)
setwd("D:/paper/R/5.Cmip6-data-processing/model_remapbil_data/")

#extra the model name from the Rdata
#'@files dirs for all Rdata files
#'return the model name
extra_Rdata_model_name<-function(files){
  model_name <- character()
  for (k in 1:length(files)) {
    strs<-str_split(files[k],'/') 
    strs<-str_split(strs[[1]][3],'[.]')
    model_name[k] <- strs[[1]][1]
  }
  return(model_name)
}

file_s1 <- dir("./historical61-14/",full.names = TRUE)
file_s2 <- dir('./ssp245',full.names = TRUE)
model<-intersect(extra_Rdata_model_name(file_s1),extra_Rdata_model_name(file_s2))

for (i in 1:length(model)) {
  print(model[i])
  load(paste0('./historical61-14/',model[i],'.Rdata',''))
  data1 <- data
  date1 <- date
  
  load(paste0('./ssp245/',model[i],'.Rdata',''))
  data<-abind(data1,data)
  attr(data,'dimension') <- NULL
  date<-c(date1,date)
  save(data,date,file = paste0('./historical/',model[i],'.Rdata',''))
  
  rm(data,data1,date,date1);gc()
}

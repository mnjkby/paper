library(matrixStats)
library(lubridate)
library(stringr)
library(PCICt)
library(trend)

mean_new<-function(x){
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    return(mean(x,na.rm=T))
  }
}

sum_new<-function(x){
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    return(sum(x,na.rm=T))
  }
}

#determine the annual absence of data,<threshold:1,>threshold:0
tongji<-function(x){
  oldlen<-length(x)
  x<-x[!is.na(x)]
  newlen<-length(x)
  if((oldlen-newlen)/oldlen<percent){
    return(1)
  }else{
    return(0)
  }
}

#determine how much data is missing each year
yearyouzhinum<-function(x){
  oldlen<-length(x)
  x<-x[!is.na(x)]
  newlen<-length(x)
  return(oldlen-newlen)
}

station_to_grid<-function(x){
  tianjia<-c()
  empty_matrix<-matrix(NA,nrow = 1,ncol = nday)
  for (j in 1:nstation) {#each station
    if(x==b[nday+1,j]){
      if(length(tianjia)==0){
        tianjia<-b[1:nday,j]
      }else{
        tianjia<-cbind(tianjia,b[1:nday,j])
      }
    }
  }
  if(length(tianjia)==0){#the grid don't have station
    return(empty_matrix)
  }else if(length(tianjia)==nday){#the grid have one station
    return(tianjia)
  }else{#the grid have at least two stations
    return(t(rowMeans(tianjia,na.rm = T)))#need consider NaN
  }
}

panduanweizhi<-function(x,y,xmin=71.25,xmax=135.75,ymin=16.25,ymax=54.25){
  v1<-x%/%0.5+1-xmin%/%0.5#col
  v2<-y%/%0.5+1-ymin%/%0.5#row
  return(v1+(v2-1)*(xmax%/%0.5-xmin%/%0.5+1))
}

#convert degrees and minutes into degrees
lonzhuanhuandu<-function(x){
  str<-as.data.frame(strsplit(as.character(x),""))
  if(length(str[,1])==5){
    str1<-as.numeric(paste(str[1,1],str[2,1],str[3,1],sep = "")) 
    str2<-as.numeric(paste(str[4,1],str[5,1],sep = ""))
  }else if(length(str[,1])==4){
    str1<-as.numeric(paste(str[1,1],str[2,1],sep = "")) 
    str2<-as.numeric(paste(str[3,1],str[4,1],sep = ""))
  }
  return(str1+str2/60)
}

latzhuanhuandu<-function(x){
  str<-as.data.frame(strsplit(as.character(x),""))
  str1<-as.numeric(paste(str[1,1],str[2,1],sep = "")) 
  str2<-as.numeric(paste(str[3,1],str[4,1],sep = ""))
  return(str1+str2/60)
}

#judge the year is contious
#'@year start_year and end_year of all files of one model
#'return TRUE or FALSE
judge_fileyear_contious<-function(year){
  year<-as.numeric(year)
  if(length(year)==2){#only one nc file
    return(T)
  }else{#at least two nc files
    for (k in 1:(length(year)/2-1)) {
      if(year[2*k]!=year[2*k+1]-1){
        return(F)
      }
    }
    return(T)
  }
}

#judge the year is contious
#'@year start_year and end_year of all files of one model
#'return TRUE or FALSE
# judge_fileyear_contious<-function(year){
  # year<-as.numeric(year)
  # if(length(year)==2){#only one nc file
    # return(T)
  # }else{#at least two nc files
    # for (k in 1:(length(year)/2-1)) {
      # if(year[2*k]!=year[2*k+1]-1){
        # return(F)
      # }
    # }
    # return(T)
  # }
# }

#select the files between start_year and end_year from GCMs and judge the model
#has complete time series through the model name 
#'@files dirs for all GCMs
#'@start_year,@end_year the beginning and end of study time
#'return the needed files of correct model
select_files_GCMs <- function(files,start_year,end_year){
  # -- a.select the files between start_year and end_year from GCMs
  model_name <- character()
  startyears <- numeric()
  endyears <- numeric()
  subn <- numeric()
  for (k in 1:length(files)) {
    strs <- str_split(files[k],'_')
    model_name[k] <- strs[[1]][3]#through the model name
    startyears[k] <- substr(strs[[1]][7],1,4) %>% as.numeric()
    endyears[k] <- substr(strs[[1]][7],10,13) %>% as.numeric()
    if ((startyears[k] <= start_year & endyears[k] >= start_year)|(startyears[k] >= start_year & endyears[k] <= end_year)|(startyears[k] <= end_year & endyears[k] >= end_year))
    {
      subn[k] <- k
    }
    else {
      subn[k] <- NA
    }
  }
  # -- b.judge the model has complete time series
  subn<-na.omit(subn)
  model<-model_name[subn[1]]
  index<-c()
  year<-c()
  lengthsubn<-length(subn)
  for (k in 1:lengthsubn){
    #k<-13
    year<-c(year,startyears[subn[k]],endyears[subn[k]])
    index<-c(index,k)
    if(k==lengthsubn){#last nc file
      if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        print(model)
      }else{
        print(1)
        subn[index]<-NA
      }
    }else if(model_name[subn[k+1]]!=model){#judge if model has changed
      if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        print(model)
      }else{
        print(1)
        subn[index]<-NA
      }
      index<-c()
      year<-c()
      model<-model_name[subn[k+1]]
    }
  }
  subn <- na.omit(subn) #remove don't use files
  return(files[subn])
}

#select the files between start_year and end_year from GCMs and judge the model
#has complete time series through the model name and ensemble members.
#'@files dirs for all GCMs
#'@start_year,@end_year the beginning and end of study time
#'return the needed files of correct model
select_files_GCMs_multiensembers <- function(files,start_year,end_year){
  # -- a.select the files between start_year and end_year from GCMs
  model_name_member <- character()
  startyears <- numeric()
  endyears <- numeric()
  subn <- numeric()
  for (k in 1:length(files)) {
    #k<-1
    strs <- str_split(files[k],'_')
    model_name_member[k] <- paste(strs[[1]][3],strs[[1]][5],strs[[1]][6],sep='_')#through the model name and ensemble members
    startyears[k] <- substr(strs[[1]][7],1,4)
    endyears[k] <- substr(strs[[1]][7],10,13)
    if ((startyears[k] <= start_year& endyears[k] >= start_year)|(startyears[k] >= start_year & endyears[k] <= end_year)|(startyears[k] <= end_year & endyears[k] >= end_year))
    {
      subn[k] <- k
    }
    else {
      subn[k] <- NA
    }
  }
  # -- b.judge the model has complete time series
  subn<-na.omit(subn)
  model<-model_name_member[subn[1]]
  index<-c()
  year<-c()
  lengthsubn<-length(subn)
  for (k in 1:lengthsubn){
    #k<-13
    year<-c(year,startyears[subn[k]],endyears[subn[k]])
    index<-c(index,k)
    if(k==lengthsubn){#last nc file
      if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        print(model)
      }else{
        print(1)
        subn[index]<-NA
      }
    }else if(model_name_member[subn[k+1]]!=model){#judge if model has changed
      if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        print(model)
      }else{
        print(1)
        subn[index]<-NA
      }
      index<-c()
      year<-c()
      model<-model_name_member[subn[k+1]]
    }
  }
  subn <- na.omit(subn) #remove don't use files
  return(files[subn])
}

#select the files between start_year and end_year from GCMs after remapbil and splityear processing using cdo
#and judge the model has complete time series through the model name and ensemble members.
#'@files dirs for all GCMs
#'@start_year,@end_year the beginning and end of study time
#'return the needed files of correct model
select_files_GCMs_resamplesplite_multiensembers <- function(files,start_year,end_year){
  # -- a.select the files between start_year and end_year from GCMs
  model_name_member <- character()
  startyears <- numeric()
  endyears <- numeric()
  subn <- numeric()
  for (k in 1:length(files)) {
    #k<-1
    strs <- str_split(files[k],'_')
    model_name_member[k] <- paste(strs[[1]][3],strs[[1]][5],strs[[1]][6],sep='_')#through the model name and ensemble members
    startyears[k] <- substr(strs[[1]][7],1,4)
    endyears[k] <- substr(strs[[1]][7],1,4)
    if ((startyears[k] <= start_year& endyears[k] >= start_year)|(startyears[k] >= start_year & endyears[k] <= end_year)|(startyears[k] <= end_year & endyears[k] >= end_year))
    {
      subn[k] <- k
    }
    else {
      subn[k] <- NA
    }
  }
  # -- b.judge the model has complete time series
  subn<-na.omit(subn)
  model<-model_name_member[subn[1]]
  index<-c()
  year<-c()
  lengthsubn<-length(subn)
  for (k in 1:lengthsubn){
    #k<-13
    year<-c(year,startyears[subn[k]],endyears[subn[k]])
    index<-c(index,k)
    if(k==lengthsubn){#last nc file
      if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        print(model)
      }else{
        print(1)
        subn[index]<-NA
      }
    }else if(model_name_member[subn[k+1]]!=model){#judge if model has changed
      if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        print(model)
      }else{
        print(1)
        subn[index]<-NA
      }
      index<-c()
      year<-c()
      model<-model_name_member[subn[k+1]]
    }
  }
  subn <- na.omit(subn) #remove don't use files
  return(files[subn])
}

#select the files between start_year and end_year from GCMs and judge the model
#has complete time series through the model name 
#'@files dirs for all GCMs
#'@start_year,@end_year the beginning and end of study time
#'return the needed files of correct model
# select_files_GCMs <- function(files,start_year,end_year){
  # # -- a.select the files between start_year and end_year from GCMs
  # model_name <- character()
  # startyears <- numeric()
  # endyears <- numeric()
  # subn <- numeric()
  # for (k in 1:length(files)) {
    # strs <- str_split(files[k],'_')
    # model_name[k] <- strs[[1]][3]#through the model name
    # startyears[k] <- substr(strs[[1]][7],1,4) %>% as.numeric()
    # endyears[k] <- substr(strs[[1]][7],10,13) %>% as.numeric()
    # if ((startyears[k] <= start_year & endyears[k] >= start_year)|(startyears[k] >= start_year & endyears[k] <= end_year)|(startyears[k] <= end_year & endyears[k] >= end_year))
    # {
      # subn[k] <- k
    # }
    # else {
      # subn[k] <- NA
    # }
  # }
  # # -- b.judge the model has complete time series
  # subn<-na.omit(subn)
  # model<-model_name[subn[1]]
  # index<-c()
  # year<-c()
  # lengthsubn<-length(subn)
  # for (k in 1:lengthsubn){
    # #k<-13
    # year<-c(year,startyears[subn[k]],endyears[subn[k]])
    # index<-c(index,k)
    # if(k==lengthsubn){#last nc file
      # if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        # print(model)
      # }else{
        # print(1)
        # subn[index]<-NA
      # }
    # }else if(model_name[subn[k+1]]!=model){#judge if model has changed
      # if((year[1]<=start_year)&(year[length(year)]>=end_year)&(judge_fileyear_contious(year))){
        # print(model)
      # }else{
        # print(1)
        # subn[index]<-NA
      # }
      # index<-c()
      # year<-c()
      # model<-model_name[subn[k+1]]
    # }
  # }
  # subn <- na.omit(subn) #remove don't use files
  # return(files[subn])
# }

#extra the model name from GCMs
#'@files dirs for all GCMs
#'return the model name of all files
extract_model_name<-function(files){
  model_name <- character()
  for (k in 1:length(files)) {
    strs <- str_split(files[k],'_')
    model_name[k] <- strs[[1]][3]
  }
  return(model_name)
}

#extra the time series from nc file
#'@x nc file
#'return the time of nc
nctimeseries<-function(x){
  Datenc<-names(x)
  for (i in 1:length(Datenc)) {
    str<-str_split(Datenc[i],'')
    str1<-paste(paste(str[[1]][2],str[[1]][3],str[[1]][4],str[[1]][5],sep = ''),
                paste(str[[1]][7],str[[1]][8],sep = ''),
                paste(str[[1]][10],str[[1]][11],sep = ''),sep = '-')
    Datenc[i]<-str1
  }
  return(Datenc)
}

#consecutive dry days,< 1mm
CDD<-function(x){
  if(length(x[!is.na(x)])==0){#if it's all NA, return NA
    return(NA)
  }else{
    x<-x[!is.na(x)]#remove NA
    n<-0#duration of days
    m<-0#matrix position
    maxdays<-matrix()
    for (item in x) {
      m<-m+1
      if(item<1){
        n<-n+1
        maxdays[m]<-n
      }else{
        maxdays[m]<-0
        n<-0
      }
    }
    return(max(maxdays))
  }
}

#CWD降水量连续大于等于1mm的最长天数
CWD<-function(x){
  if(length(x[!is.na(x)])==0){#如果全是NA,则返回NA
    return(NA)
  }else{
    x<-x[!is.na(x)]#把NA值去掉
    n<-0#持续天数
    m<-0#矩阵位置
    maxdays<-matrix()
    for (item in x) {
      m<-m+1
      if(item>=1){
        n<-n+1
        maxdays[m]<-n
      }else{
        maxdays[m]<-0
        n<-0
      }
    }
    return(max(maxdays,na.rm = TRUE))
  }
}

#Max 1 day precipitation amount
Rx1day<-function(x){
  if(length(x[!is.na(x)])==0){#if it's all NA, return NA
    return(NA)
  }else{
    return(max(x,na.rm = T))
  }
}

#Rx5day,每月连续5日最大降水量，na设为0,这里暂且算的是年尺度的
Rx5day<-function(x){
  if(length(x[!is.na(x)])==0){#如果全是NA,则返回NA
    return(NA)
  }else{
    x<-x[!is.na(x)]
    m<-0
    j<-length(x)-4
    for (n in 1:j) {
      if((x[n]+x[n+1]+x[n+2]+x[n+3]+x[n+4])>=m){
        m<-x[n]+x[n+1]+x[n+2]+x[n+3]+x[n+4]
      }
    }
    return(m)
  }
}     

#annual total precipitation form days >95th percentile
R95p<-function(x){
  if(length(x[!is.na(x)])==0){#if it's all NA, return NA
    return(NA)
  }else{
    x<-x[which(x>=1)]
    p95<-quantile(x, c(0.95))
    m<-0
    for (item in x) {
      if(item>p95){
        m<-m+item
      }
    }
    return(m)
  }
}

#大于95%分位数的总的降雨量/年总降水量
R95pTOT<-function(x){
  if(length(x[!is.na(x)])==0){#如果全是NA,则返回NA
    return(NA)
  }else{
    x<-x[which(x>=1)]
    p95<-quantile(x, c(0.95))
    m<-0#R95p
    n<-0#total pre
    for (item in x) {
      if(item>p95){
        m<-m+item
      }
      n<-n+item
    }
    if(n==0){
      return(0)
    }else{
      return(m/n)
    }
  }
}

#总的降水量(日降水大于99%分位数时)
R99p<-function(x){
  if(length(x[!is.na(x)])==0){#如果全是NA,则返回NA
    return(NA)
  }else{
    x<-x[which(x>=1)]
    p99<-quantile(x, c(0.99))
    m<-0
    for (item in x) {
      if(item>p99){
        m<-m+item
      }
    }
    return(m)
  }
}

#大于99%分位数的总的降雨量/年总降水量
R99pTOT<-function(x){
  if(length(x[!is.na(x)])==0){#如果全是NA,则返回NA
    return(NA)
  }else{
    x<-x[which(x>=1)]
    p99<-quantile(x, c(0.99))
    m<-0#R99p
    n<-0#total pre
    for (item in x) {
      if(item>p99){
        m<-m+item
      }
      n<-n+item
    }
    if(n==0){
      return(0)
    }else{
      return(m/n)
    }
  }
}

#日降水强度(大多数1mm)
SDII<- function(x) {
  if(length(x[!is.na(x)])==0){#如果全是NA,则返回NA
    return(NA)
  }else{
    n<-0#wetdays
    m<-0#totalpre
    for (item in x) {
      if(is.na(item)){
        next
      }else if(item>=1){
        m<-m+item
        n<-n+1
      }
    }
    if(m==0){#整年都没有降水
      return(0)
    }else{
      return(m/n)
    }
  }
}

#Annual total precipitation from days ≥ 1 mm
PRCPTOT<- function(x){
  if(length(x[!is.na(x)])==0){#if it's all NA, return NA
    return(NA)
  }else{
    return(sum(x[which(x>=1)]))
  }
}

#中雨日数，the days of P >=10mm/day in the year，na设为0
R10<-function(x){
  if(length(x[!is.na(x)])==0){#如果全是NA,则返回NA
    return(NA)
  }else{
    n<-0#wetdays 
    for (item in x) {
      if(is.na(item)){
        next
      }else if(item>=10){
        n<-n+1
      }
    }
    return(n)
  }
} 

#the days of P >=20mm/day in the year
R20<-function(x){
  if(length(x[!is.na(x)])==0){#if it's all NA, return NA
    return(NA)
  }else{
    n<-0#wetdays 
    for (item in x) {
      if(is.na(item)){
        next
      }else if(item>=20){
        n<-n+1
      }
    }
    return(n)
  }
}           

#降雨1%分位数
R01_percentile<-function(x){
  a<-quantile(x, c(0.01),na.rm = TRUE)
  return(a)
}

#降雨5%分位数
R05_percentile<-function(x){
  a<-quantile(x, c(0.05),na.rm = TRUE)
  return(a)
}

#降雨25%分位数
R25_percentile<-function(x){
  a<-quantile(x, c(0.25),na.rm = TRUE)
  return(a)
}

R05_percentile_rm<-function(x){
  x<-x[which(x>0)]
  a<-quantile(x, c(0.05),na.rm = TRUE)
  return(a)
}

R25_percentile_rm<-function(x){
  x<-x[which(x>0)]
  a<-quantile(x, c(0.25),na.rm = TRUE)
  return(a)
}

#降雨95%分位数
R95_percentile<-function(x){
  a<-quantile(x, c(0.95),na.rm = TRUE)
  return(a)
}

#降雨99%分位数
R99_percentile<-function(x){
  a<-quantile(x, c(0.99),na.rm = TRUE)
  return(a)
}

#'return var,date,lon,lat
bind_GCMs <- function(file,var){
  data <- llply(1:length(file), function(i){
    data1 <- nc3(file = file[i], varid = c(var,'lon','lat'))
    names(data1) <- c('E','lon','lat')
    date <- nc_date(file[i])
    date <- str_sub(date, 1, 10) %>% as.character()
    data1$date <- date
    data1
  }, .progress = 'text')
  var_value <- llply(data, function(x) x$E) %>% abind(., along = 3)
  attr(var_value, 'dimnames') <- NULL
  
  date <- llply(data, function(x) x$date) %>% abind(., along = 1)
  attr(date, 'dimnames') <- NULL
  
  return(list(var_value = var_value,date = date,lon = data[[1]]$lon, lat = data[[1]]$lat))
  rm(data,var_value); gc()
}

#'return var,date
bind_GCMs_remapbil <- function(file,var){
  data <- llply(1:length(file), function(i){
    data1 <- nc3(file = file[i], varid = c(var))
    names(data1) <- c('E')
    date <- nc_date(file[i])
    date <- str_sub(date, 1, 10) %>% as.character()
    data1$date <- date
    data1
  }, .progress = 'text')
  var_value <- llply(data, function(x) x$E) %>% abind(., along = 3)
  attr(var_value, 'dimnames') <- NULL
  
  date <- llply(data, function(x) x$date) %>% abind(., along = 1)
  attr(date, 'dimnames') <- NULL
  
  return(list(var_value = var_value,date = date))
  rm(data,var_value); gc()
}

#'return var,date,lon,lat,but lon:-180-180
bind_GCMs_PET <- function(file,var){
  data <- llply(1:length(file), function(i){
    data1 <- nc3(file = file[i], varid = c(var,'lon','lat'))
    names(data1) <- c('E','lon','lat')
    date <- nc_date(file[i])
    date <- str_sub(date, 1, 10) %>% as.character()
    data1$date <- date
    data1
  }, .progress = 'text')
  var_value <- llply(data, function(x) x$E) %>% abind(., along = 3)
  attr(var_value, 'dimnames') <- NULL
  
  date <- llply(data, function(x) x$date) %>% abind(., along = 1)
  
  lon <- data[[1]]$lon
  lon <- ifelse(lon > 180,lon-360,lon)
  
  lat <- data[[1]]$lat
  
  return(list(var_value = var_value,lon = lon,lat = lat,date = date))
  rm(data,var_value); gc()
}

#return var of year,date
bind_GCMs_piControl_flood <- function(files1, name, fun){
  nums <- which(str_detect(files1, paste(name, '_', sep = '')))
  #nums <- nums[1:4]
  data <- llply(nums, function(i){
    data1 <- nc3(file = files1[i], varid = c('pr'))
    names(data1) <- c('E')
    Years <- nc_date(files1[i])
    Years <- str_sub(Years, 1, 4) %>% as.numeric()
    data1$E <- data1$E*3600*24#transform units
    E <- apply_3d(data1$E, dim = 3, FUN = fun, by = Years)
    data1$E <- E
    data1$year <- unique(Years)
    data1
  }, .progress = 'text')
  var_value <- llply(data, function(x) x$E) %>% abind(., along = 3)
  attr(var_value, 'dimnames') <- NULL
  
  Date <- llply(data, function(x) x$year) %>% abind(., along = 1)
  attr(Date, 'dimnames') <- NULL
  
  return(list(Date = Date, E = var_value))
  rm(data); gc()
}


nc_date <- function(fid, ntime = -1, unit = NULL, to_char = FALSE){
  if (class(fid)[1] != "ncdf4") {
    fid <- nc_open(fid)
    on.exit(nc_close(fid))
  }
  
  ctime    <- fid$dim$time # time class
  origin   <- ctime$units %>% str_extract("\\d{2,4}-\\d{1,2}-\\d{1,2}")
  calendar <- ctime$calendar
  
  if (is.null(calendar)) calendar <- "gregorian"
  nslice <- fid$dim$time$len
  if (ntime == -1) ntime <- nslice
  
  if (is.null(unit)) unit = fid$dim$time$units %>% str_extract(".*(?= since)")
  secs <- switch(unit, 
                 "minutes" = 60,
                 "hours" = 3600, 
                 "days"  = 86400)
  date <- {as.PCICt(origin, cal=calendar) + (fid$dim$time$vals[1:ntime])*secs}
  if (to_char) date %<>% format(DATE_FORMAT)
  date
}

bind_GCMs_flood <- function(files, fun){
  data <- llply(1:length(files), function(i){
    data1 <- nc3(file = files[i], varid = c('pr'))
    names(data1) <- c('E')
    Years <- nc_date(files[i])
    Years <- str_sub(Years, 1, 4) %>% as.numeric()
    data1$E <- data1$E*3600*24#transform units
    E <- apply_3d(data1$E, dim = 3, FUN = fun, by = Years)
    data1$E <- E
    data1$year <- unique(Years)
    data1
  }, .progress = 'text')
  var_value <- llply(data, function(x) x$E) %>% abind(., along = 3)
  attr(var_value, 'dimnames') <- NULL
  
  Date <- llply(data, function(x) x$year) %>% abind(., along = 1)
  attr(Date, 'dimnames') <- NULL
  
  return(list(Date = Date, E = var_value))
  rm(data); gc()
}

split_GCMs<-function(file,startyear,endyear,splityears){
  Years <- nc_date(file)
  Years <- str_sub(Years, 1, 4) %>% as.numeric()
  year <- unique(Years)[unique(Years) %in% c(startyear:endyear)]#selete years
  if(length(year)>splityears){
    cut <- cut2(x = year, m = splityears, onlycuts = TRUE,minmax=F)#splite years
    index <- c()
    for (i in 1:length(cut)) {
      index <- c(index,min(which(Years == cut[i])))
    }
    index1<-c()
    for (i in 1:(length(cut)-1)) {
      index1 <- rbind(index1,c(index[i],(index[i+1]-1)))
    }
    index1<-rbind(index1,c(max(index),max(which(Years == max(cut)))))
  }else{
    index1<-cbind(min(which(Years == min(year))),max(which(Years == max(year))))
  }
  return(index1)
}

# prj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# world <- readShapePoly('F:/Research/17. DTR attribution/Results/Regions/SREX_delete_Africa.shp', proj4string = prj)
#'@nc3 read the lon lat and var of ncdata
nc3 <- function(file, varid){
  var_value <- llply(varid, function(x){
    nc <-  nc_open(file)
    on.exit(nc_close(nc))
    ncvar_get(nc, x)
  })
  return(var_value)
}

# prj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# world <- readShapePoly('F:/Research/17. DTR attribution/Results/Regions/SREX_delete_Africa.shp', proj4string = prj)
#'@nc3 read the lon lat and var of ncdata
nc3_select_time <- function(file, varid, start, count){
  nc <-  nc_open(file)
  on.exit(nc_close(nc))
  ncdata <- ncvar_get(nc, varid, start = start, count = count)
  return(ncdata)
}

Penman_future <- function(Tmax,Tmin,WS,RH, SH, LH, Z){
  Tm <- (Tmax+Tmin)/2
  Rn <- SH + LH  # sensible heat and Latent heat flux 
  delta <- 4098*(0.6108*exp(17.27*Tm/(Tm+237.3)))/(Tm+237.3)^2
  gama <- 0.000665*101.3*((293-0.0065*Z)/293)^5.26
  es <- (0.6108*exp(17.27*Tmax/(Tmax+237.3))+0.6108*exp(17.27*Tmin/(Tmin+237.3)))/2
  ea <- RH/100*(0.6108*exp(17.27*Tmax/(Tmax+237.3))+0.6108*exp(17.27*Tmin/(Tmin+237.3)))/2
  PET <- (0.408*delta*Rn+gama*900/(Tm+273)*WS*(es-ea))/(delta+gama*(1+0.34*WS))
  return(PET)
}

judge_model<-function(file,models){
  strs <- str_split(file,'_')
  model <- strs[[1]][3]
  return(which(models == model))
}

judge_scenario<-function(file,scenarios){
  strs <- str_split(file,'_')
  scenario <- strs[[1]][4]
  return(which(scenarios == scenario))
}

#extra the model name from GCMs
#'@files dirs for all GCMs
#'return the model name and ensemble members of all files
extract_model_time<-function(files){
  model_name <- character()
  for (k in 1:length(files)) {
    strs <- str_split(files[k],'_')
    model_name[k] <- paste(strs[[1]][3],strs[[1]][4],strs[[1]][5],strs[[1]][6],strs[[1]][7],sep='_')
  }
  return(model_name)
}

threshold<-(-0.5)

#calculate drought peak
drought_peak<-function(x){
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    x<-x[!is.na(x)]
    if(min(x)>(-0.5)){#modify drought threshold
      return(NA)
    }else{
      return(min(x)) 
    }
  }
}

#calculate drought intensity from one event
# drought_intensity<-function(x){
#   if(length(x[!is.na(x)])==0){
#     return(NA)
#   }else{
#     x<-x[!is.na(x)]
#     n<-0
#     m<-0#matrix location
#     maxdays<-matrix()
#     for (item in x) {
#       m<-m+1
#       if(item<(-0.5)){#modify drought threshold
#         n<-n+item
#         maxdays[m]<-n
#       }else{
#         maxdays[m]<-NA
#         n<-0
#       }
#     }
#     if(length(na.omit(maxdays))==0){
#       return(NA)
#     }else{
#       return(min(maxdays,na.rm = T))
#     }
#   }
# }

#calculate drought intensity all the year
drought_intensity<-function(x){
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    x<-x[!is.na(x)]
    intensity<-0
    for (item in x) {
      if(item<(-0.5)){#modify drought threshold
        intensity<-intensity+item
      }
    }
    if(intensity==0){
      return(NA)
    }else{
      return(intensity)
    }
  }
}

#calculate drought duration all the year,is meaningless for monthly
drought_duration<-function(x){
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    x<-x[!is.na(x)]
    duration<-0
    for (item in x) {
      if(item<(-0.5)){#modify drought threshold
        duration<-duration+1
      }
    }
    if(duration==0){
      return(NA)
    }else{
      return(duration)
    }
  }
}

#calculate drought index mean
# drought_index_mean<-function(x){
#   if(length(x[!is.na(x)])==0){
#     return(NA)
#   }else{
#     return(mean(x,na.rm=T))
#   }
# }

#calculate drought mean,mean of data which less than -0.5
drought_mean<-function(x){
  if(length(x[!is.na(x)])==0){
    return(NA)
  }else{
    x <- x[which(x < (-0.5))]
    return(mean(x,na.rm=T))
  }
}

#calculate drought frequency,need modify
# drought_frequency<-function(x){
#   if(length(x[!is.na(x)])==0){
#     return(NA)
#   }else{
#     x<-x[!is.na(x)]
#     frequency <- 0
#     for (item in x) {
#       if(item )
#     }
#     
#   }
# }

#extra the model name and member from GCMs,e.g., ACCESS-CM2_r1i1p1f1_gn
#'@files dirs for all GCMs
#'return the model name and ensemble members of all files
extract_model_name_members<-function(files){
  model_name <- character()
  for (k in 1:length(files)) {
    strs <- str_split(files[k],'_')
    model_name[k] <- paste(strs[[1]][3],strs[[1]][5],strs[[1]][6],sep='_')
  }
  return(model_name)
}

#extra the model name from GCMs,e.g., ACCESS-CM2_historical_r1i1p1f1_gn
#'@files dirs for all GCMs
#'return the model name and ensemble members of all files
extract_model_name_scenario_members<-function(files){
  model_name <- character()
  for (k in 1:length(files)) {
    strs <- str_split(files[k],'_')
    model_name[k] <- paste(strs[[1]][3],strs[[1]][4],strs[[1]][5],strs[[1]][6],sep='_')
  }
  return(model_name)
}

#'@by if is NULL,FUN should only be row applied function;
#'    if isn't NULL,FUN can be modified according to own needs.
apply_3d <- function(array, dim = 3, FUN, by = NULL, scale = 1, na.rm = TRUE, ...) {
  # TODO: add by at here
  dims <- dim(array)
  ndim <- length(dims) # dimensions
  
  I_dims     <- setdiff(1:ndim, dim) # dimensions order
  dims_head  <- dims[I_dims]         # header dimensions 
  
  # move grouped dim to the last
  if (dim != ndim){
    array %<>% aperm(c(I_dims, dim)) 
  } 
  
  mat <- array_3dTo2d(array)
  
  if (is.null(by)) {
    ans <- FUN(mat, ..., na.rm = na.rm)    
    dim_new <- dims_head
  } else {
    dim_new <- c(dims_head, length(unique(by)))
    ans <- apply_row(mat, by, FUN, scale = scale)
  }
  dim(ans) <- dim_new
  ans
}

apply_row <- function(mat, by, FUN, scale = 1, ...) {
  if (length(by) != ncol(mat)) {
    stop('Length of by is not equal to ncol of mat')
  }
  if (length(scale) == 1) scale = rep(scale, length(by))
  grps <- unique(by) %>% sort()
  
  ans <- lapply(grps, function(grp) {
    I <- which(by == grp)
    factor = scale[I][1]
    apply(mat[, I, drop = FALSE] * factor,1,FUN)
  }) %>% do.call(cbind, .)
  
  if (!is.matrix(ans)) ans <- as.matrix(ans)
  ans %>% set_colnames(grps) %>% 
    set_rownames(rownames(mat))
}

##calculate daily SPI by parallel
row_parallel<-function(i){
  spi(pre[i,],90,na.rm = T)$fitted %>% as.vector()
}

row_parallel_delete<-function(i){
  spi(pre[i,][-seq(1155,21915,(365*4+1))],90,na.rm = T)$fitted %>% as.vector()
}

drought_parallel<-function(i){
  tapply(SPI_daily_90days[i,], year(Date), fun[[n]])
}

cluster_init <- function(ncluster = 12, variables, package){
  cl <- makeCluster(ncluster, type = "SOCK", outfile = "log.txt")
  worker_init <- function(package){
    for (i in package){
      library(i, character.only = TRUE)
    }
  }
  clusterCall(cl, worker_init, package)
  clusterExport(cl, variables, envir = environment())
  assign("cl", cl, envir = .GlobalEnv)
}

# 4. Calculate the area weights for each grid, except the grids in removed Id
#'@lonlat data.frame(lon, lat),lon belong to -180:180
#'@Id the locations of the removed grids, such as Ocean (measuread by ID_Ocean_Ice)
weight_Grid <- function(lonlat, Id){
  grid <- SpatialPixelsDataFrame(points = lonlat, data = data.frame(id = 1:nrow(lonlat)),
                                 proj4string = CRS('+proj=longlat +ellps=WGS84'))
  area <- raster::area(raster(grid))# area weights
  area <- area@data@values
  area[Id] <- NA
  weights <- area/sum(area, na.rm = T)
  return(weights)
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

sifenwei<-function(x,percentile_min=0.25, percentile_max=0.75,plot_mean=F,plot_median=F,
                   scenario=c('AER','GHG','NAT','ALL')){
  data<-c()
  for (i in 1:length(x)) {
    sifenweimin<-apply(x[[i]],2,function(x){return(quantile(x,percentile_min))})
    sifenweimax<-apply(x[[i]],2,function(x){return(quantile(x,percentile_max))})
    if(plot_median){
      sifenwei2<-apply(x[[i]],2,function(x){return(quantile(x,c(0.5)))})
    }else if(plot_mean){
      sifenwei2<-apply(x[[i]],2,function(x){return(mean(x))})
    }
    data<-rbind(data,data.frame(sifenweimin,sifenwei2,sifenweimax,c(1:length(sifenweimin)),
                                rep(scenario[i],length(sifenweimin))))
  }
  return(data)
}

## Fit anchor regression for given \lambda and \gamma value:
# following Rothenhäusler, D., Meinshausen, N., Bühlmann, P. and Peters, J., 2021. 
# Anchor regression: Heterogeneous data meet causality. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 83(2), pp.215-246.
# fits anchor regression directly
#'@X_sc 
fit.anchor_s <- function(X_sc, Y_sc, A, lambda = 100, gamma = 5, ret.Loss = F) {
  
  X_ct = X_sc
  Y_ct = Y_sc
  
  n = length(A)
  p = dim(X_ct)[2]
  
  PA = A %*% solve(t(A) %*% A) %*% t(A)
  PAc = diag(n) - PA
  
  D_l = lambda * diag(p) + gamma * t(X_ct) %*% PA %*% X_ct + t(X_ct) %*% PAc %*% X_ct
  d = (-gamma * t(PA %*% Y_ct) %*% X_ct - t(PAc %*% Y_ct) %*% X_ct) / (-1) 
  
  beta_lg = solve(D_l) %*% t(d)
  
  if (ret.Loss == F) {
    return(c(beta_lg))
  } else if (ret.Loss == T) {
    ret.list = list()
    ret.list$beta = beta_lg
    ret.list$PA = PA  
    ret.list$Loss = 
      c(perf = sum(c((diag(n) - PA) %*% (Y_ct - X_ct %*% beta_lg))^2),
        anchor = gamma * sum(( PA %*% (Y_ct - X_ct %*% beta_lg) )^2),
        ridge = lambda * sqrt(sum(beta_lg^2)))
    # sum(perf + anchor + ridge)
    return(ret.list$Loss)
  }
}

#need modify
Interp_DTR <- function(array, lon,lat){
  r <- raster(array[,,1], xmn= min(lon), xmx= max(lon), ymn=min(lat), ymx= max(lat))
  s <- raster(nrow=130, ncol=77,xmn=71.25, xmx=135.75, ymn=16.25, ymx=54.25)
  s <- resample(r, s, method = 'bilinear')
  results <- as.array(s)
  for (i in 2:dim(array)[[3]]) {
    r <- raster(array[,,i], xmn= min(lon), xmx= max(lon), ymn=min(lat), ymx= max(lat))
    s <- raster(nrow=130, ncol=77,xmn=71.25, xmx=135.75, ymn=16.25, ymx=54.25)
    s <- resample(r, s, method = 'bilinear')
    results <- abind(results,as.array(s))
  }
  attr(results, 'dimnames') <- NULL 
  return(results)
}

Slope<- function(x) ifelse(sum(!is.na(x)) < 5, NA, sens.slope(na.omit(x))$estimates)
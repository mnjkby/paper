#'''
#@author:xiyuchen
#@time:2022/11/12,9:49
#@email:Xiyc@cug.edu.cn
#'''

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
setwd('D:/paper/R/4.area_weight/')


sdi <- c(-0.08, 0.72, -2.12, -1.75, -1.08, 0.88, 1.73, 0.52, -0.89, -1.42, 
         -0.64, -0.22, -0.54, -0.86, -1.02, -1.50, -1.07, -0.72, 0.24, 0.85,
         1.59, 0.65, -1.55, -0.45, 0.20, 0.45, 0.65, -0.25, -0.55, -0.35)
data <- data.frame(c(1:30),sdi)
colnames(data)<-c("day","value")
Time <-
  ggplot(data,aes(x=day,y=value))+
    # geom_point(size=1.8,shape=1)+
    geom_line(lwd=0.8)+
    xlab('day')+ylab('sdi')+
    geom_hline(yintercept = (-0.8), linetype = "dashed", color = "red") +
    theme_bw()+
    theme(axis.title = element_text(size = rel(1), face = "bold"),
          axis.text = element_text(size = rel(1),face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

png(filename = paste0('./fig/feature.png'), width = 8, height = 4, res = 500, units = 'in')
plot(Time)
dev.off()

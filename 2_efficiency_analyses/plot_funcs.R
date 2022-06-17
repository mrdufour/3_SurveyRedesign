# Wed Jun 15 17:02:47 2022 ------------------------------

## plotting function for efficiency analysis - percent error
plot_func_perc_er <- function(data.x,title,edsu_size){

  ggplot(eff_out, aes(x=EDSUs, y=-perc_er))+
    geom_point(size =2, shape = 16, alpha=0.75, aes(color = as.factor(Year_labs)))+
    labs(title=paste(title),
         x=paste0("Number of EDSU (",edsu_size,")"), y = "Percent error abs((S-T)/T)*100") +
    theme_bw()  +
    scale_color_grey(start = 0.0, end = 0.5) +
    guides(col=guide_legend("Year")) +
    geom_hline(yintercept = -5, color = "red", lty=2)
  
}

  
## plotting function for efficiency analysis - residual squared error (rse)
plot_func_rse <- function(data.x, title, edsu_size){
  
  ggplot(eff_out, aes(x=EDSUs, y=rse))+
    geom_point(size =2, shape = 16, alpha=0.75, aes(color=as.factor(Year_labs)))+
    labs(title=paste(title),
         x=paste0("Number of EDSU (",edsu_size,")"), y = "RSE") +
    theme_bw() +
    scale_color_grey(start = 0.0, end = 0.5) +
    guides(col=guide_legend("Year")) +
    geom_hline(yintercept = 15, color = "red", lty=2)
  
}
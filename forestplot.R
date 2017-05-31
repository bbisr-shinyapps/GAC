forestplot <- function(CI_Data, lf_label, left_label, right_label, fontsize, ID_pos2, scaledata, legendfontsize)
{
## Points to plot on the log scale
scaledata <-data.frame(ID=0,PointEstimate=c(as.numeric(unlist(strsplit(scaledata, ",")))))

LegendLabels<-data.frame(x=c(rep(0.3,times=2)),
                         y=c(0.3,1.2),
                         lab=c(left_label,right_label))

p <- ggplot(CI_Data,aes(factor(ID)))+ labs(x=NULL, y=NULL)
p <- p + geom_text(data=CI_Data,aes(factor(ID),0.1,label=CI_Data[,2],vjust=0.5, hjust=ID_pos2, size=fontsize))  # gene names
p <- p + geom_hline(aes(yintercept=1),linetype=2, size=0.5) #vertical line at 1
p <- p + geom_errorbar(data=CI_Data,aes(x=ID,ymin =CI_lower, ymax=CI_higher), color = CI_Data$boxcolor,  width=0.5) #errorbars 
p <- p + geom_point(data=CI_Data,aes(x=ID, y=PointEstimate),shape=22,size=5,fill=CI_Data$boxcolor, colour=CI_Data$boxcolor)   #HR
p <- p + scale_y_log10() 
p <- p + coord_flip() 
p <- p + geom_text(data=scaledata,aes(0,scaledata$PointEstimate,label=PointEstimate, vjust=-0.05, size=6))  # point location on X=0
p <- p + geom_text(data=LegendLabels,aes(x,y,label=lab, fontface="bold"),hjust=0, vjust=-0.5, size=legendfontsize)  # legend labels
p <- p + geom_point(data=scaledata,aes(0.25,PointEstimate),shape=3,size=3)  # + at X=0
p <- p + geom_point(aes(0.25,10),shape=3,alpha=0)  
p <- p + geom_segment(aes(x = 0.25, y = 0, xend = 0.25, yend = max(CI_Data[, 3:4])+0.5)) # horizontal line   
p <- p + geom_segment(aes(x = 0.25, y = 1, xend = 0.25, yend = 5),arrow=arrow(length = unit(0.02, "npc")),linetype=1,size=1) # right arrow 
p <- p + geom_segment(aes(x = 0.25, y = 1, xend = 0.25, yend = 0.2),arrow=arrow(length = unit(0.02, "npc")),linetype=1,size=1) #left arrow
p <- p + theme(axis.line = element_blank(), 
               axis.text.x = element_blank(), 
               axis.text.y = element_blank(),
               axis.ticks = element_blank(), 
               axis.title.x = element_blank(), 
               axis.title.y = element_blank(), 
               axis.ticks.length = unit(0.0001, "mm"),
               axis.text = element_text(margin=margin(0,0,0,-2,"lines")), 
               legend.position = "none", 
               panel.background = element_rect(fill = "transparent"),  
               panel.border = element_blank(),
               panel.spacing = unit(c(-0.1,-0.1,-0.5,-0.5), "mm"),
               plot.margin = unit(c(10,-2.5,0,10), "mm"),
               panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank())

plot(p)

}
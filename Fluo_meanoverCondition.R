list_meanOVERCondition_fluo<-lapply(list_Fluorescence_parameters, function(i){i%>%
    group_by(`Measuring Date`,Condition)%>%
    summarise(Mean=mean(value, na.rm=T),SD=sd(value, na.rm=T))%>%mutate(Name=i$Genotype[1])%>%
    mutate(Index=i$variable[1])})


path_plotaverage<-"plots/plots_meanFluo/"


list_plots_meanOVERCondition_fluo<-lapply(list_meanOVERCondition_fluo, function(p)
{plot<-p%>%ggplot(aes(x=as.Date(`Measuring Date`, format="%Y-%m-%d"),y=Mean,color=Condition))+
  ylab(p$Index[1])+xlab("Date")+labs(title=p$Index[1])+
  geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),
                ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_point(size=0.1)+geom_line()+theme_bw(base_size = 10)+
  geom_errorbar(aes(ymin=Mean - SD, ymax=Mean+SD),
                
                width=.2, position=position_dodge(0.05))+
  scale_color_manual(values = c("Control"="turquoise3", 
                                "Drought stress"="coral"))+
  theme(axis.text.x = element_text(angle=75, hjust=1.2, size = 9),
        axis.text.y = element_text(size=9),
        legend.text=element_text(size=7),
        legend.title = element_text(size=13),
        axis.title.x = element_text(hjust=1, size = 7),
        axis.text=element_text(size=13),axis.title.y=element_text(size=10))
ggsave(file=paste0(path_plotaverage, paste0(p$Index[1], "_average.pdf")))



save_this<-paste0(path_plotaverage, paste0(p$Index[1], "_average.png"))
 if(file.exists(save_this)){

} else {


ggsave(plot,file=paste0(path_plotaverage, paste0(p$Index[1], "_average.png")))
 }
return(plot)})
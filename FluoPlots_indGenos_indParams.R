dir.create("plots/plots_Genotypes_Fluo")


path_plotaverage<-"plots/plots_Genotypes_Fluo/"

list_plots_allFc<-list()
list_mean_sd_Fluo<-list()
list_plots_FluoANDmean<-list()


for(i in list_fluo){
  list_mean_sd_Fluo[[length(list_mean_sd_Fluo)+1]]<-i%>%
    group_by(`Measuring Date`,Condition)%>%
    summarise(Mean=mean(value, na.rm=T),SD=sd(value, na.rm=T))%>%mutate(Name=i$Genotype[1])%>%
    mutate(Index=i$variable[1])}


for(i in list_fluo){list_plots_allFc[[length(list_plots_allFc)+1]]<-
  ggplot(i, aes(x=as.Date(`Measuring Date`, format="%Y-%m-%d"), y=value,group=`Plant Name`, 
                color=Condition))+
  #geom_smooth(aes(color=Condition, group=Condition))+#kommt eventuell weg
  ylab(i$variable[1])+xlab("Date")+labs(title=i$Genotype[1])+
  geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_point(size=0.5)+theme_bw(base_size = 5)+
  scale_color_manual(values = c("Control"="turquoise3", 
                                "Drought stress"="coral"))+
  theme(axis.text.x = element_text(angle=75, hjust=1.2),plot.title = element_text(size=16), 
        legend.text=element_text(size=12),legend.title = element_text(size=13), 
        axis.title.x = element_text(hjust=1),
        axis.text=element_text(size=13),axis.title=element_text(size=14))
}



for(i in 1:length(list_mean_sd_Fluo)){
  list_plots_FluoANDmean[[length(list_plots_FluoANDmean)+1]]<-
    list_plots_allFc[[i]]+geom_line(data=list_mean_sd_Fluo[[i]], 
                                    aes(x=as.Date(`Measuring Date`, format="%Y-%m-%d"),
                                        y=Mean,color=Condition, group=Condition))
  save_this<-paste0(path_plotaverage, paste0(list_mean_sd_Fluo[[i]]$Name[1], paste0(list_mean_sd_Fluo[[i]]$Index[1], ".png")))
  if(file.exists(save_this)){
    
  } else {ggsave(file=paste0(path_plotaverage, paste0(list_mean_sd_Fluo[[i]]$Name[1], paste0(list_mean_sd_Fluo[[i]]$Index[1], ".png"))))}
}
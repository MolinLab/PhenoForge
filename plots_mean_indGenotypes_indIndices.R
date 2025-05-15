
dir.create("plots/plots_mean_indGenotypes_indIndices", recursive = TRUE)

path_plotaverage<-"plots/plots_mean_indGenotypes_indIndices/"


list_mean_sd_INDICES<-list()
list_plots_ALL_HC<-list()
list_plots_IndicesANDmean<-list()


#calculating mean per df (one index, one genotype df all dates)
for(i in list_indices){
  list_mean_sd_INDICES[[length(list_mean_sd_INDICES)+1]]<-i%>%
    group_by(`Measuring Date`,Condition)%>%
    summarise(Mean=mean(value, na.rm=T),SD=sd(value, na.rm=T))%>%mutate(Name=i$Genotype[1])%>%
    mutate(Index=i$variable[1])}

#plotting 
for(i in list_indices){list_plots_ALL_HC[[length(list_plots_ALL_HC)+1]]<-
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
        axis.text=element_text(size=13),axis.title=element_text(size=14))}


#adding the mean to plot
for(i in 1:length(list_mean_sd_INDICES)){
  list_plots_IndicesANDmean[[length(list_plots_IndicesANDmean)+1]]<-
    list_plots_ALL_HC[[i]]+geom_line(data=list_mean_sd_INDICES[[i]], 
                                     aes(x=as.Date(`Measuring Date`, format="%Y-%m-%d"),
                                         y=Mean,color=Condition, group=Condition))
  save_this<-paste0(path_plotaverage, paste0(list_mean_sd_INDICES[[i]]$Name[1], paste0(list_mean_sd_INDICES[[i]]$Index[1], ".png")))
  if(file.exists(save_this)){
    
  } else {ggsave(file=paste0(path_plotaverage, paste0(list_mean_sd_INDICES[[i]]$Name[1], paste0(list_mean_sd_INDICES[[i]]$Index[1], ".png"))))}
  
  
}


list_real<-list()
real_names<-names(list_indices)
for(i in real_names){string_name<-str_extract(i, ".+(?=.$)")
list_real[length(list_real)+1]<-string_name
}
unlist(list_real)


names(list_plots_IndicesANDmean)<-list_real
genolist<-split(list_plots_IndicesANDmean,f=names(list_plots_IndicesANDmean))

names(genolist[[1]])<-list_indices_Hc
names(genolist[[2]])<-list_indices_Hc

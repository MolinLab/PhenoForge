
dir.create("plots/plots_mean_Condition")

path_plotaverage<-"plots/plots_mean_Condition/"
############replacement code für redundant code unten:
#neue source Liste: list_indices_byloop

#same as list_mean_sd_INDICES!!!
list_meanOVERCondition<-lapply(list_indices_by_loop, function(i){i%>%
    group_by(`Measuring Date`,Condition)%>%
    summarise(Mean=mean(value, na.rm=T),SD=sd(value, na.rm=T))%>%mutate(Name=i$Genotype[1])%>%
    mutate(Index=i$variable[1])})

list_plots_meanOVERCondition<-lapply(list_meanOVERCondition, function(p)
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
        axis.title.x = element_text(hjust=1, size = 12),
        axis.text=element_text(size=13),axis.title.y=element_text(size=10))
ggsave(file=paste0(path_plotaverage, paste0(p$Index[1], "_average.pdf")))

return(plot)

save_this<-paste0(path_plotaverage, paste0(p$Index[1], "_average.png"))
if(file.exists(save_this)){
  
} else {
  
  
  ggsave(file=paste0(path_plotaverage, paste0(p$Index[1], "_average.png")))
}
})

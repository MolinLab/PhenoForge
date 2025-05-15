
dir.create("plots/plots_Genotypes_Index")


path_plotaverage<-"plots/plots_Genotypes_Index/"

list_allgenos<-lapply(list_indices_by_loop, function(i){p<-ggplot(i, aes(x=as.Date(`Measuring Date`, format="%Y-%m-%d"), y=value, color=Condition))+
  ylab(paste0(i$variable[1]))+xlab("Date")+labs(title=paste0(i$variable[1])," all Genotypes")+
  geom_point(size=0.1)+
  geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.02,color="lightgrey")+
  theme_bw(base_size = 10)+
  scale_color_manual(values = c("Control"="turquoise3", 
                                "Drought stress"="coral"))+
  theme(axis.text.x = element_text(angle=75, vjust=0.5, size=10), axis.text.y = element_text(size=10), legend.text=element_text(size=7),
        legend.title = element_text(size=5),
        legend.key.size = unit(0.1, "cm"))+
  geom_smooth(aes(color=Condition, group=Condition))+geom_point(size=0.5)
ggsave(filename = paste0(path_plotaverage,paste0(i$variable[1]),"_allGenotypes.pdf"))

return(p)


save_this<-paste0(path_plotaverage,paste0(i$variable[1]),"_allGenotypes.png")
if(file.exists(save_this)){
  
} else {
  
  
  ggsave(file=paste0(path_plotaverage,paste0(i$variable[1]),"_allGenotypes.png"))
}
})


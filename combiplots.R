combi_graphs<-list()


for(i in seq(1:8)){p<-plot_grid(list_allgenos[[i]]+xlab(" ")+theme(legend.position = "none"), list_plots_meanOVERCondition[[i]]+labs(title= " ")+theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()), nrow=1,rel_widths = c(1,1.1)) 
combi_graphs[[length(combi_graphs)+1]]<-p

}
names(combi_graphs)<-list_indices_Hc
dir.create("plots/combis")
pathcombi<-"plots/combis/"

for(i in 1:length(combi_graphs)){
  name<-paste0(pathcombi,paste0(names(combi_graphs[i]), "_combi.png"))
  
  ggsave(file=name, plot=combi_graphs[[i]])
  }


###fluocombi

combi_graphs_fluo<-list()


for(i in seq(1:length(list_Fluorescence_parameters))){p<-plot_grid(list_allgenos_fluo[[i]]+xlab(" ")+theme(legend.position = "none"), list_plots_meanOVERCondition_fluo[[i]]+labs(title= " ")+theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()), nrow=1,rel_widths = c(1,1.3)) 
combi_graphs_fluo[[length(combi_graphs_fluo)+1]]<-p

}
names(combi_graphs_fluo)<-list_indices_Fc
dir.create("plots/combis_fluo")
pathcombi<-"plots/combis_fluo/"

for(i in 1:length(combi_graphs_fluo)){
  name<-paste0(pathcombi,paste0(names(combi_graphs_fluo[i]), "_combi.png"))
  
  ggsave(file=name, plot=combi_graphs_fluo[[i]])
  }


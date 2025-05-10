#code composite plots


dir.create("plots/correlograms/thesis")
path_plotsDownstreamThesis<-"plots/correlograms/thesis/"


testrunscatter<-lapply(CamData_Bonitur_TG_cut, function(i){
  column_names<-colnames(i)
  
  # if(str_contains(column_names, "QY_max")==TRUE & str_contains(column_names, "LeafNumber")==TRUE){
  
  #newdf<-dplyr::select(i, Condition, ends_with("avg"), QY_max, LeafNumber, TillerNumber,TG)
  # ploti<-ggpairs(newdf, mapping=ggplot2::aes(colour=Condition),
  #    lower=list(continous=wrap("smooth", alpha=0.05, size=0.1)),
  #    diag = list(discrete="barDiag",continuous = wrap("densityDiag", alpha=0.5 )),
  #      cardinality_threshold = 40, title=paste0("Indices,Bonitur,", i$`Measuring Date`[1]), legend=1)+
  #  theme(panel.grid.major = element_blank())+
  # scale_color_manual(values = c("Control"="turquoise3", 
  #   "Drought stress"="coral"))+
  # scale_fill_manual(values = c("Control"="turquoise3", 
  #  "Drought stress"="coral"))
  
  # ggsave(filename = paste0(path_plotsDownstream2,paste0(i$`Measuring Date`[1]),".png"), width=20, height=20)
  # return(ploti)}
  
  if(str_contains(column_names, "LeafNumber")==TRUE){
    pi<-dplyr::select(i, Condition, ends_with("avg"), LeafNumber, TillerNumber,TG)
    piplot<-ggpairs(pi, mapping=ggplot2::aes(colour=Condition),
                    lower=list(continous=wrap("smooth", alpha=0.05, size=0.5)),
                    upper=list(continous=wrap("cor", size=8)),
                    diag = list(discrete="barDiag",continuous = wrap("densityDiag", alpha=0.5)),
                    cardinality_threshold = 40, title=paste0("Indices,Bonitur,", i$`Measuring Date`[1]), legend=1)+
      theme(panel.grid.major = element_blank())+
 
      scale_fill_manual(values = c("Control"="turquoise3", 
                                   "Drought stress"="coral"))+
      scale_color_manual(values = c("Control"="turquoise3", 
                                    "Drought stress"="coral"))
    return(piplot)}
  else{
    pim<-dplyr::select(i, Condition, ends_with("avg"), TG)
    pimlot<-ggpairs(pim, mapping=ggplot2::aes(colour=Condition),
                    lower=list(continous=wrap("smooth", alpha=0.05, size=0.4)),
                    upper=list(continous=wrap("cor", size=8)),
                    
                    diag = list(discrete="barDiag",continuous = wrap("densityDiag", alpha=0.5 )),
                    cardinality_threshold = 40, title=paste0("Indices,Bonitur,", i$`Measuring Date`[1]), legend=1)+
      theme(panel.grid.major = element_blank())+

      scale_fill_manual(values = c("Control"="turquoise3", 
                                   "Drought stress"="coral"))+
      scale_color_manual(values = c("Control"="turquoise3", 
                                          "Drought stress"="coral"))
    return(pimlot)}
}
)
names(testrunscatter)<-dates

#path_plotsDownstreamThesis<-"~/Dokumente/Masterarbeit_AIT/Writing_TexStudio/Thesis_WheatVIZ/upload_plots/corrrelo"

#lapply(testrunscatter, function(x){
  
 # save_this<-paste0(path_plotsDownstreamThesis,paste0(x$title ,".png"))
 # if(file.exists(save_this)){
    
  #} else {
 #   ggsave(x,filename = paste0(path_plotsDownstreamThesis,paste0(x$title ,".png")), width=15, height=15)
  #}})


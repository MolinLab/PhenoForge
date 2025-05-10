
#code posthoc NDVI


dir.create("plots/posthoc/new")

posthoc_path<-"plots/posthoc/new/"

#erstmal NDVI:

lapply(list_anovas_NDVI_allPosthocs, function(z){
  
  df_filtered <- z %>%dplyr::filter(Condition=="Drought stress")%>%
    group_by(.group) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if(length(unique(df_filtered$.group))>1){
    
    
    stat_test<-df_filtered%>%group_by(".group")%>%t_test(emmean~.group, p.adjust.method = "holm", var.equal = FALSE)%>%mutate(y.position = max(z$emmean))
    
    one<-ggplot(z, aes(x=.group, y=emmean, color=Condition))+geom_boxplot()+labs(title= z$Date[1],x="group")+
      stat_pvalue_manual(stat_test, label = "p", y.position = "y.position", step.increase = .1)+
      scale_colour_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))+theme_bw()
    
    
    ggsave(one, filename=paste0(posthoc_path, paste0(z$Date[1]), "posthocNDVI.png"))
    
    return(one)
    
  } else{
    
    
    two<-ggplot(z, aes(x=.group, y=emmean, color=Condition))+geom_boxplot()+labs(title= z$Date[1], x="group")+
      scale_colour_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))+theme_bw()
    
    
    
    ggsave(two, filename=paste0(posthoc_path, paste0(z$Date[1]),"posthocNDVI.png"))
    
    return(two)
    
  }
  
})
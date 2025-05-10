#code boxplots


dir.create("plots/boxplots/index")

boxplots_sepIndices<-"plots/boxplots/index/"

boxes_TGscore_CondsamePlots_NDVI<-
  lapply(CamData_HConly_melt_cut, function(yi){
    if(!is.null(yi)){
      
      
      ya<-yi%>%dplyr::filter(variable=="NDVI-avg")
      
      boxa<-ggplot(ya, aes(x=TG_score, y=value,fill=Condition))+geom_boxplot()+ylim(min(ya$value), max(ya$value))+
        labs(title=paste0("NDVI ", yi$`Measuring Date`[1]))+
        theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
        scale_fill_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))
      
      
      
      # ggsave(boxa, filename=paste0(boxplots_sepIndices, paste0( yi$`Measuring Date`[1],"_NDVI.png" )))
      
      
      return(boxa)}})

names(boxes_TGscore_CondsamePlots_NDVI)<-dates

################################################################
#OSAVI




boxes_TGscore_CondsamePlots_SIPI<-
  lapply(CamData_HConly_melt, function(yi){
    if(!is.null(yi)){
      
      
      ya<-yi%>%dplyr::filter(variable=="SIPI-avg")
      
      boxa<-ggplot(ya, aes(x=TG_score, y=value,fill=Condition))+geom_boxplot()+ylim(min(ya$value), max(ya$value))+
        labs(title=paste0("SIPI ", yi$`Measuring Date`[1]))+
        theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
        scale_fill_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))
      
      
      
      # ggsave(boxa, filename=paste0(boxplots_sepIndices, paste0( yi$`Measuring Date`[1],"_OSAVI.png" )))
      
      
      return(boxa)}})

names(boxes_TGscore_CondsamePlots_SIPI)<-dates
##############################################################



#OSAVI




boxes_TGscore_CondsamePlots_OSAVI<-
  lapply(CamData_HConly_melt, function(yi){
    if(!is.null(yi)){
      
      
      ya<-yi%>%dplyr::filter(variable=="OSAVI-avg")
      
      boxa<-ggplot(ya, aes(x=TG_score, y=value,fill=Condition))+geom_boxplot()+ylim(min(ya$value), max(ya$value))+
        labs(title=paste0("OSAVI ", yi$`Measuring Date`[1]))+
        theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
        scale_fill_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))
      
      
      
     # ggsave(boxa, filename=paste0(boxplots_sepIndices, paste0( yi$`Measuring Date`[1],"_OSAVI.png" )))
      
      
      return(boxa)}})


names(boxes_TGscore_CondsamePlots_OSAVI)<-dates
###################################################################
#MCARI1




boxes_TGscore_CondsamePlots_MCARI1<-
  lapply(CamData_HConly_melt, function(yi){
    if(!is.null(yi)){
      
      
      ya<-yi%>%dplyr::filter(variable=="MCARI1-avg")
      
      boxa<-ggplot(ya, aes(x=TG_score, y=value,fill=Condition))+geom_boxplot()+ylim(min(ya$value), max(ya$value))+
        labs(title=paste0("MCARI1", yi$`Measuring Date`[1]))+
        theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
        scale_fill_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))
      
      
      
     # ggsave(boxa, filename=paste0(boxplots_sepIndices, paste0( yi$`Measuring Date`[1],"_MCARI1.png" )))
      
      
      return(boxa)}})

names(boxes_TGscore_CondsamePlots_MCARI1)<-dates

#########################################################################
#NDVI2



#boxes_TGscore_CondsamePlots<-
 # lapply(CamData_HConly_melt, function(yi){
   # if(!is.null(yi)){
   #   
   #   ya<-yi%>%dplyr::filter(variable=="NDVI2-avg" )
      
      
   #   boxa<-ggplot(ya, aes(x=variable, y=value,fill=Condition))+geom_boxplot()+ylim(min(ya$value), max(ya$value))+
   #     labs(title= yi$`Measuring Date`[1])+
   #    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
    #    scale_fill_manual(values = c("Control"="turquoise3", 
      #                               "Drought stress"="coral"))
    #  
      
      
      
      
      
    #  ggsave(boxa, filename=paste0(boxplots_sepIndices, paste0( yi$`Measuring Date`[1],"_NDVI2.png" )))
      
      
    #  return(boxa)}})



############################################################################
#PRI


boxes_TGscore_CondsamePlots_PRI<-
  lapply(CamData_HConly_melt, function(yi){
    if(!is.null(yi)){
      
      ya<-yi%>%dplyr::filter(variable=="PRI-avg" )
      
      
      boxa<-ggplot(ya, aes(x=TG_score, y=value,fill=Condition))+geom_boxplot()+ylim(min(ya$value), max(ya$value))+
        labs(title=paste0("PRI ", yi$`Measuring Date`[1]))+
        theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
        scale_fill_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))
      
      
      
      
      
      
     # ggsave(boxa, filename=paste0(boxplots_sepIndices, paste0( yi$`Measuring Date`[1],"_PRI.png" )))
      
      
      return(boxa)}})
names(boxes_TGscore_CondsamePlots_PRI)<-dates
##############################################################################



#############################################################################
#PSRI


boxes_TGscore_CondsamePlots_PSRI<-
  lapply(CamData_HConly_melt, function(yi){
    if(!is.null(yi)){
      
      ya<-yi%>%dplyr::filter(variable=="PSRI-avg" )
      
      
      boxa<-ggplot(ya, aes(x=TG_score, y=value,fill=Condition))+geom_boxplot()+ylim(min(ya$value), max(ya$value))+
        labs(title=paste0("PSRI ", yi$`Measuring Date`[1]))+
        theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
        scale_fill_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))
      
      
      
      
      
      
   #   ggsave(boxa, filename=paste0(boxplots_sepIndices, paste0( yi$`Measuring Date`[1],"_PSRI.png" )))
      
      
      return(boxa)}})


names(boxes_TGscore_CondsamePlots_PSRI)<-dates
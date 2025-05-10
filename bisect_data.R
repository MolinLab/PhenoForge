
CamData_TG<-lapply(list_cutHc, function(x){
  table1<-x%>%select(- ends_with("median"))%>%
    inner_join(metatable_slim, by="Plant ID")%>%
    relocate(Genotype, Condition, .after = "Plant ID")%>%
    inner_join(dplyr::select(weizenernte, `Plant ID`, TG),
               by="Plant ID")
  return(table1)
})
CamData_Bonitur_TG<-lapply(CamData_TG, 
                           fuse_me_Bonitur, Bonitur_growthCHamber)%>%
  unlist(recursive = F)
dates<-unique(list_indices[[1]]$`Measuring Date`)
names(CamData_Bonitur_TG)<-dates


CamData_Bonitur_TG_cut<-lapply(CamData_Bonitur_TG, function(cut){
  
  if(cut$`Measuring Date`[1] <= "2024-02-20"){
    
    cut_new<-cut%>%dplyr::select(-`OSAVI-avg`, -`MCARI1-avg`, -`PRI-avg`)
    return(cut_new)
  }else{
    return(cut)
  }
})


CamData_Bonitur_TG_allscored<-lapply(CamData_Bonitur_TG_cut, function(pi){
  inner_join(pi, dplyr::select(weizenernteMean_sorted, Genotype, TG_score), by="Genotype")%>%
    dplyr::relocate(TG_score, .after = Condition)})


CamData_HConly_melt<-lapply(CamData_Bonitur_TG_allscored, function(melt_me){
  
  me_melt<-melt(melt_me, id.var=c("Plant ID","Genotype", "Measuring Date","Condition", "TG_score"))
  return(me_melt)
  
})


CamData_HConly_melt_cut<-lapply(CamData_HConly_melt, function(xix){
  
  if(xix$`Measuring Date`[1] <= "2024-02-20"){
    
    xix_new<-xix%>%dplyr::filter(variable!= "MCARI1-avg")%>%
      dplyr::filter(variable!= "OSAVI-avg")%>%
      dplyr::filter(variable!= "PRI-avg")
    return(xix_new)
    
  }else{
    return(xix)
  }
  
})


##################################
#adding RGB data
################

testfuseRGBHC<-lapply(CamData_Bonitur_TG_cut, fuse_me2)%>%unlist(recursive = F)
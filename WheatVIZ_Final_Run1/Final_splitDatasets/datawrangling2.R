list_cutHC_splitDataset<- lapply(list_cutHc, function(cut){
  
  if(cut$`Measuring Date`[1] <= "2024-02-20"){
    
    cut_new<-cut%>%dplyr::select(-`OSAVI-avg`, -`MCARI1-avg`)
    return(cut_new)
  }else{
    return(cut)
  }
})

list_wholeDataset_HC<-list_cutHc

list_cutHc<-list_cutHC_splitDataset
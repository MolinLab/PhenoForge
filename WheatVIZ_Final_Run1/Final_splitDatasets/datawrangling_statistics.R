
#trennen des grossen wide dfs "Genotype_allHC_Indexcolumns" nach Datum und individuelles Zusammenfügen basierend auf den vorhandenen Dataframes Dates mit Fc, RGB Boni data -> für date-specific PCAs, kmeans, etc
#->list mit unterschiedlichen Indices,RGB, etc dfs (wide) per Datum

new_split_dataframe<-split_dataframe(Genotype_allHC_Indexcolumns, "Measuring Date")


wideDFs_Datesplit_joined<-lapply(new_split_dataframe, fuse_me)

wideDFs_Datesplit_joined_unlisted<-unlist(wideDFs_Datesplit_joined, recursive = FALSE)

DFs_joined_allCamData<-lapply(wideDFs_Datesplit_joined_unlisted, fuse_me2)
DFS_joined_allCamData_unlisted<-unlist(DFs_joined_allCamData, recursive = FALSE)



bonitur_allCamdata_list<-lapply(DFS_joined_allCamData_unlisted, fuse_me_Bonitur, Bonitur_growthCHamber)

boni_unlisted<-unlist(bonitur_allCamdata_list, recursive = FALSE)

CamData_Bonitur<-lapply(boni_unlisted, function(z){
  checking_content<-colnames(z)
  if(str_contains(checking_content, "LeafNumber")==TRUE){
    zi<-z%>%dplyr::select(-Tray, -GenotypeNr, -GeneralImpression)
    return(zi)}
  else{return(z)}
})

CamData_Bonitur_TG<-lapply(CamData_Bonitur, function(p){p%>%inner_join(metatable_slim,by=c("Plant ID", "Genotype", "Condition"))%>%dplyr::relocate(Condition, .after = `Measuring Date`)})

list_dates<-lapply(names(CamData_Bonitur_TG), function(date){
  
  input_string<-date
  date_pattern <- sub("^([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", input_string)
  return(date_pattern)
})%>%unlist(recursive=F)

names(CamData_Bonitur_TG)<-list_dates

CamData_Bonitur_TG_DS<-lapply(CamData_Bonitur, function(p){p%>%inner_join(metatable_slim,by=c("Plant ID", "Genotype", "Condition"))%>%dplyr::relocate(Condition, .after = `Measuring Date`)%>%dplyr::filter(Condition=="Drought stress")})


camData_Bonitur_TG_means<-lapply(CamData_Bonitur_TG, function(x){
  mean_df<-x%>%dplyr::filter(Condition=="Drought stress")%>%group_by(Genotype)%>%summarise(across(where(is.numeric), mean)%>%mutate(Date=x$`Measuring Date`[1]))
  return(mean_df)
})

camData_Bonitur_TG_DSmeans<-lapply(CamData_Bonitur_TG, function(x){
  mean_df<-x%>%dplyr::filter(Condition=="Drought stress")%>%group_by(Genotype)%>%summarise(across(where(is.numeric), mean)%>%mutate(Date=x$`Measuring Date`[1]))
  return(mean_df)
})


#wie mit Eva besprochen, die Einteilung der TG in 3 sparten, diese als Info für die boxplots:
#erstmal der dataframe, jede Gruppe (DS und C) bekommt ein e3igene Einteilung:


#wie mit Eva besprochen, die Einteilung der TG in 3 sparten, diese als Info für die boxplots:
#erstmal der dataframe:

CamData_Bonitur_TG_allscored<-lapply(CamData_Bonitur_TG, function(pi){inner_join(pi, dplyr::select(weizenernteMean_sorted, Genotype, TG_score), by="Genotype")%>%dplyr::relocate(TG_score, .after = Condition)})


CamData_Bonitur_TG_DSscored<-lapply(CamData_Bonitur_TG_DS, function(pi){inner_join(pi, dplyr::select(weizenernte_sorted_DS, `Plant ID`, TG_score), by="Plant ID")})

new_split_dataframe_TGscore<-lapply(new_split_dataframe,function(pi){inner_join(pi, dplyr::select(weizenernteMean_sorted, Genotype, TG_score), by="Genotype")%>%dplyr::relocate(TG_score, .after = `Measuring Date`)%>%inner_join(metatable_slim, by=c("Plant ID", "Genotype", "Condition"))%>%dplyr::select(-`Size-avg`, -TG)})

CamData_HConly_melt<-lapply(new_split_dataframe_TGscore, function(melt_me){
  
  me_melt<-melt(melt_me, id.var=c("Plant ID","Genotype", "Measuring Date","Condition", "TG_score"))
  return(me_melt)
  
})


#contingency tables TG_score and cluster
#for(i in Dataframes_withClusterinfo_TGscore){
# if(!is.null(i)==TRUE){
#  print(i$`Measuring Date`[1])
#  print(table(i[, (ncol(i)-1):ncol(i)]))}}

#nur um zu ziegen.dass es geht, das darszustellen, mal schauen, was man da so machen kann:

#hist(weizenernte$TG, breaks=20, labels=weizenernte$`Plant ID`)

CamData_Bonitur_TG_allscored_melt<-lapply(CamData_Bonitur_TG_allscored, function(me){
  me_melt<-melt(me, id.var=c("Plant ID","Genotype", "Measuring Date","Condition","TG_score"))
  return(me_melt)
})

###



#####

list_melt_wholeHC<-
  lapply(list_wholeDataset_HC, function(m)
                          {melt(m,measure.vars=3:ncol(m))})


########################################

#vollständigen HC dataframe list holen und benennen

new_test<-do.call("rbind", list_wholeDataset_HC)

new_test_named<-
  inner_join(new_test,dplyr::select(metatable, 
                                         `Plant ID`, Genotype, Condition), by="Plant ID")%>%
  dplyr::relocate(Genotype, Condition, .after = `Measuring Date`)%>%
  dplyr::select(-ends_with("median"))%>%
  inner_join(dplyr::select(weizenernte, `Plant ID`, TG),
             by="Plant ID")


#nach Datum splitten

new_split_dataframe<-split_dataframe(new_test_named, "Measuring Date")

#mit FC data fusen
wideDFs_Datesplit_joined<-lapply(new_split_dataframe, fuse_me)


wideDFs_Datesplit_joined_unlisted<-unlist(wideDFs_Datesplit_joined, recursive = FALSE)

#mit RGB data fusen
DFs_joined_allCamData<-lapply(wideDFs_Datesplit_joined_unlisted, fuse_me2)
DFS_joined_allCamData_unlisted<-unlist(DFs_joined_allCamData, recursive = FALSE)


#mit Bonitur fusen
bonitur_allCamdata_list<-lapply(DFS_joined_allCamData_unlisted, fuse_me_Bonitur, Bonitur_growthCHamber)

boni_unlisted<-unlist(bonitur_allCamdata_list, recursive = FALSE)


#zurecht schneiden der dfs
CamData_Bonitur_TG<-lapply(boni_unlisted, function(z){
  checking_content<-colnames(z)
  if(str_contains(checking_content, "LeafNumber")==TRUE){
    zi<-z%>%dplyr::select(-Tray, -GenotypeNr, -GeneralImpression)
    return(zi)}
  else{return(z)}
})

#benennen der dfs nach Datum-> für Übersicht nachhher sehr wichtig bei ANOVA
list_dates<-lapply(names(CamData_Bonitur_TG), function(date){
  
  input_string<-date
  date_pattern <- sub("^([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", input_string)
  return(date_pattern)
})%>%unlist(recursive=F)

names(CamData_Bonitur_TG)<-list_dates


###############
#mean berechnen


camData_Bonitur_TG_means<-lapply(CamData_Bonitur_TG, function(x){
  mean_df<-x%>%group_by(Genotype)%>%summarise(across(where(is.numeric), mean)%>%mutate(Date=x$`Measuring Date`[1]))
  return(mean_df)
})


#######
#weizenernte wrngling: score des TG etc


weizenernte_sorted<-weizenernte[order(weizenernte$TG),]


weizenernte_sorted_C<-weizenernte_sorted%>%inner_join(metatable_slim, by="Plant ID")%>%dplyr::filter(Condition=="Control")
weizenernte_sorted_DS<-weizenernte_sorted%>%inner_join(metatable_slim, by="Plant ID")%>%dplyr::filter(Condition=="Drought stress")

splitsize<-nrow(weizenernte_sorted_DS)%/% 3


weizenernte_sorted_DS$TG_score<-c(rep("low", splitsize), rep("medium", splitsize), rep("high", splitsize))


weizenernte_sorted_C$TG_score<-c(rep("low", splitsize), rep("medium", splitsize), rep("high", splitsize))

weizenernte_all_TGscored<-rbind(weizenernte_sorted_C, weizenernte_sorted_DS)


CamData_Bonitur_TG_allscored<-lapply(CamData_Bonitur_TG, function(pi){inner_join(pi, dplyr::select(weizenernte_all_TGscored, `Plant ID`, TG_score), by="Plant ID")%>%dplyr::relocate(TG_score, .after = Condition)})



#################
#Mean


weizenernte_meanTG<-weizenernte_all_TGscored%>%
  dplyr::select(-TG_score)%>%dplyr::filter(Condition=="Drought stress")%>%group_by(Genotype)%>%summarise(Mean_TG=mean(TG))

weizenernteMean_sorted<-weizenernte_meanTG[order(weizenernte_meanTG$Mean_TG),]

splitsize_mean<-nrow(weizenernte_meanTG)%/% 3


weizenernteMean_sorted$TG_score<-c(rep("low", splitsize_mean), rep("medium", splitsize_mean), rep("high", splitsize_mean+2))



########################################
#einzelne dfs in liste nach Datum gesplittet gescored

new_split_dataframe_TGscore<-lapply(new_split_dataframe,function(pi){inner_join(pi, dplyr::select(weizenernteMean_sorted, Genotype, TG_score), by="Genotype")%>%dplyr::relocate(TG_score, .after = `Measuring Date`)%>%inner_join(metatable_slim, by=c("Plant ID", "Genotype", "Condition"))%>%dplyr::select(-`Size-avg`, -TG)})

CamData_HConly_melt<-lapply(new_split_dataframe_TGscore, function(melt_me){
  
  me_melt<-melt(melt_me, id.var=c("Plant ID","Genotype", "Measuring Date","Condition", "TG_score"))
  return(me_melt)
  
})


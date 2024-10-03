#Dates
#Genotype_allHc_Datescolumns

#random forest 1 :TG depending on all Dates of 1 Index:

dataNDVI_DS<-Genotype_allHc_Datescolumns%>%
  inner_join(metatable_slim, by=c("Genotype", "Plant ID"))%>%
  dplyr::filter(Index=="NDVI-avg" & Condition=="Drought stress")%>%
  relocate(Condition,.after = Genotype)%>%as.data.frame()

selected_IndicesDatecolumns<-lapply(list_indices_Hc, function(x){
  dataIndex<-Genotype_allHc_Datescolumns%>%
    inner_join(metatable_slim, by=c("Genotype", "Plant ID"))%>%
    dplyr::filter(Index==x)%>%
    relocate(Condition,.after = Genotype)%>%as.data.frame()%>%
    mutate(Genotype=as.factor(Genotype), Condition=as.factor(Condition))
  
  df_name<-paste0("data",gsub("-", "",x)) 
  assign(df_name, dataIndex, envir = .GlobalEnv)
})


#Indices
#Genotype_allHC_Indexcolumns




selected_GenotypesIndexcolumns<-lapply(list_of_names, function(x){
  dataIndex<-Genotype_allHC_Indexcolumns%>%
    inner_join(metatable_slim, by=c("Genotype", "Plant ID"))%>%
    dplyr::filter(Genotype==x)%>%
    relocate(Condition,.after = Genotype)%>%as.data.frame()%>%
    mutate(Genotype=as.factor(Genotype), Condition=as.factor(Condition),
           `Measuring Date`=as.factor(`Measuring Date`), `Plant ID`=as.factor(`Plant ID`))
  
  df_name<-paste0("data",gsub("-", "",x)) 
  assign(df_name, dataIndex, envir = .GlobalEnv)
})

names(selected_GenotypesIndexcolumns)<-list_of_names

Genotype_allHC_Indexcolumns_Condition<-Genotype_allHC_Indexcolumns%>%
  inner_join(metatable_slim, by=c("Genotype", "Plant ID"))%>%
  relocate(Condition, .after = Genotype)%>%as.data.frame()%>%
  mutate(Genotype=as.factor(Genotype), Condition=as.factor(Condition),
         `Measuring Date`=as.factor(`Measuring Date`), `Plant ID`=as.factor(`Plant ID`))
  
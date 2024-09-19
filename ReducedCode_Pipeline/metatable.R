metatable<-cbind(list_Hc_Plant[[4]]$'Plant ID',list_Hc_Plant[[4]]$'Tray Info', list_Hc_Plant[[4]]$'Plant Name')
colnames(metatable)<-c("Plant ID", "Tray Info","Plant Name")
metatable<-as_tibble(metatable)
metatable<-metatable %>% mutate(Condition = if_else(grepl("*DS", metatable$`Tray Info`), "Drought stress", "Control" ))


placeholdertable<-data.frame(Plant.Name="none")

for(i in metatable$'Plant Name'){modified_string <- gsub("^.*?_", "", i)
placeholdertable<-rbind(placeholdertable, modified_string)}
placeholdertable<-placeholdertable[-1,]
metatable<-metatable%>%mutate(Genotype=placeholdertable)%>%function_make_trayINfo()
metatable

metatable_slim<-metatable%>%dplyr::select(-`Tray Info`, -`Plant Name`, -Tray)

#also:jetzt habe ich das folgende Konstrukt:
#eine Liste mit 5 elements, wo jedes element eine eigene Liste ist (die Messarten sind die elemente dieser Liste, jedes einzelne Messdatum ist eine Liste in dieser Liste)


list_cutDFs<-list(list_cutFc, list_cutHc, list_cutKin, list_cutRgb_C, list_cutRgb_M)


names_listcutDFs<-c()
for(i in list_cutDFs){string_name<-str_extract(names(i[1]), "[^/]+$")
names_listcutDFs[length(names_listcutDFs)+1]<-string_name}
names(list_cutDFs)<-names_listcutDFs




list_melt_dfs<-lapply(list_cutDFs, function(i){lapply(i, function(m){melt(m,measure.vars=3:ncol(m))})})


names_listmeltDFs<-c()
for(i in list_melt_dfs){string_name<-str_extract(names(i[1]), "[^/]+$")
names_listmeltDFs[length(names_listmeltDFs)+1]<-string_name}
names(list_melt_dfs)<-names_listmeltDFs



#aufgeteilt nach Messart in  eigene Listen:
list_meltFC<-list_melt_dfs[[1]]
list_meltHC<-list_melt_dfs[[2]]
list_melt_Kin<-list_melt_dfs[[3]]
list_meltRGBC<-list_melt_dfs[[4]]
list_meltRGBM<-list_melt_dfs[[5]]


#unlisted, alle Messdaten und Messarten als Elememnte dieser Liste


#long format
list_meltTables_all<-unlist(list_melt_dfs, recursive=FALSE)

#wide format
list_cutTables_all<-unlist(list_cutDFs, recursive=FALSE)

names_listmeltDFs<-c()
for(i in names(list_meltTables_all)){string_name<-str_extract(i, "[^/]+$")
names_listmeltDFs[length(names_listmeltDFs)+1]<-string_name}
names(list_meltTables_all)<-names_listmeltDFs
anyNA(list_meltTables_all, recursive = T)




#measn uns sd über die replicates dieser unlisted List:
list_sd_andMEAN_allFiles<-lapply(list_meltTables_all,join_and_calc)

#verbindet alle meltTables mit Genotyp und anderer metaInformation
list_meltTables_Named<-lapply(list_meltTables_all, merge_and_select)
#vormals list_meltAndNames_2

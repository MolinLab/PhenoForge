
#Lists I need:



list_cutDFs<-list(list_cutFc, list_cutHc, list_cutKin, list_cutRGB_C_hexcodes, list_cutRgb_M)
#anyNA(list_cutDFs, recursive = T)
####hier kommt noch Code, if anyNA() == TRUE, then ..... which() und so



##################
#neue Lösung, um zusätzliche merge NAs zu umgehen: und:keep it simple...

list_melt_dfs<-lapply(list_cutDFs, function(i){lapply(i, function(m){melt(m,measure.vars=3:ncol(m))})})
#list_names<-lapply(names(list_melt_dfs),function(n){string<-str_extract(n[1], "[^/]+$")
#retur

#names_listmeltDFs<-c()
#
#for(i in list_melt_dfs){string_name<-str_extract(names(i[1]), "[^/]+$")
#names_listmeltDFs[length(names_listmeltDFs)+1]<-string_name}
#names(list_melt_dfs)<-names_listmeltDFs




#aufgeteilt nach Messart in  eigene Listen:
list_meltFC<-list_melt_dfs[[1]]
list_meltHC<-list_melt_dfs[[2]]
list_melt_Kin<-list_melt_dfs[[3]]
list_meltRGBC<-list_melt_dfs[[4]]
list_meltRGBM<-list_melt_dfs[[5]]


list_meltTables_all<-unlist(list_melt_dfs, recursive=FALSE) 

#wide format
list_cutTables_all<-unlist(list_cutDFs, recursive=FALSE)

names_listmeltDFs<-c()
for(i in names(list_meltTables_all)){string_name<-str_extract(i, "[^/]+$")
names_listmeltDFs[length(names_listmeltDFs)+1]<-string_name}
names(list_meltTables_all)<-names_listmeltDFs
anyNA(list_meltTables_all, recursive = T)



#measn uns sd über die replicates dieser unlisted List:
#list_sd_andMEAN_allFiles<-lapply(list_meltTables_all,join_and_calc, metatable)

#verbindet alle meltTables mit Genotyp und anderer metaInformation
list_meltTables_Named<-lapply(list_meltTables_all, merge_and_select, metatable)
#vormals list_meltAndNames_2


list_indices_Fc<-colnames(list_Fc_Plant[[1]][,13:ncol(list_Fc_Plant[[1]])])

list_Fluorescence_parameters<-lapply(list_indices_Fc, indices.to.list, y="Fc_Plant")
names(list_Fluorescence_parameters)<-list_indices_Fc#für FC hab ich grade nur 1 Index

list_RGB_C_hexcodes<-lapply(list_cutRgb_C, triplet_to_colour)



#names_measurements<-list(unique(names(list_meltTables_all)))

list_of_names<-unique(metatable$Genotype)
list_indices_Hc<-colnames(dplyr::select(list_Hc_Plant[[1]],  ends_with("avg")))
#list_other-measurement-types

#die folgende Funktion looped über die Indices der Messarten, weist die erstellten dfs einer Liste zu, die einen dynaimisch erstellten Namen hat und zurück gegeben wird


list_indices_by_loop<-lapply(list_indices_Hc, indices.to.list, y="Hc_Plant")
names(list_indices_by_loop)<-list_indices_Hc


########################################################################


###Listen erstellen der Indices per Names: aber loop wär hier noch gut

#list_NDVI_splitNames<-lapply(list_of_names, function(x){list_indices_by_loop[["NDVI-avg"]]%>%dplyr::slice(grep(x, Genotype))})

########################################################################################
####################continue here tomorrow 11.09.: 
#für alle Indices alle Genotypen splitten (LOOP???)

#naming the dfs in the list of HC indices
list_INDICES_splitBYName<-lapply(list_of_names, function(b){lapply(list_indices_Hc, function(q){
  list_index<-indices.to.list(q,"Hc_Plant")
  genotype_list<-testing_fun(list_index,b)
  return(genotype_list)}
)})
names(list_INDICES_splitBYName)<-list_of_names




list_indices<-unlist(list_INDICES_splitBYName, recursive = FALSE)
list_mean_sd_INDICES<-list()

#calculating mean per df (one index, one genotype df all dates)
for(i in list_indices){
  list_mean_sd_INDICES[[length(list_mean_sd_INDICES)+1]]<-i%>%
    group_by(`Measuring Date`,Condition)%>%
    summarise(Mean=mean(value, na.rm=T),SD=sd(value, na.rm=T))%>%mutate(Name=i$Genotype[1])%>%
    mutate(Index=i$variable[1])}

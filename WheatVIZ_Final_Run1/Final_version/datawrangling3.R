
#Lists I need:



#measn uns sd über die replicates dieser unlisted List:
#list_sd_andMEAN_allFiles<-lapply(list_meltTables_all,join_and_calc, metatable)

#verbindet alle meltTables mit Genotyp und anderer metaInformation
list_meltTables_Named<-lapply(list_meltTables_all, merge_and_select, metatable)
#vormals list_meltAndNames_2



#names_measurements<-list(unique(names(list_meltTables_all)))

list_of_names<-unique(metatable$Genotype)
list_indices_Hc<-colnames(dplyr::select(list_Hc_Plant[[1]],  ends_with("avg")))
#list_other-measurement-types

list_indices_Fc<-colnames(list_Fc_Plant[[1]][,13:ncol(list_Fc_Plant[[1]])])

#die folgende Funktion looped über die Indices der Messarten, weist die erstellten dfs einer Liste zu, die einen dynaimisch erstellten Namen hat und zurück gegeben wird


list_indices_by_loop<-lapply(list_indices_Hc, indices.to.list, y="Hc_Plant")
names(list_indices_by_loop)<-list_indices_Hc


list_Fluorescence_parameters<-lapply(list_indices_Fc, indices.to.list, y="Fc_Plant")
names(list_Fluorescence_parameters)<-list_indices_Fc#für FC hab ich grade nur 1 Index

list_RGB_C_hexcodes<-lapply(list_cutRgb_C, triplet_to_colour)

########################################################################


###Listen erstellen der Indices per Names: aber loop wär hier noch gut

#list_NDVI_splitNames<-lapply(list_of_names, function(x){list_indices_by_loop[["NDVI-avg"]]%>%dplyr::slice(grep(x, Genotype))})

########################################################################################
####################continue here tomorrow 11.09.: 
#für alle Indices alle Genotypen splitten (LOOP???)

#list_NDVI_splitBYnames<-lapply(list_of_names, testing_fun, x=list_indices_by_loop[["NDVI-avg"]])



#naming the dfs in the list of HC indices
list_INDICES_splitBYName<-lapply(list_of_names, function(b){lapply(list_indices_Hc, function(q){
  list_index<-indices.to.list(q,"Hc_Plant")
  genotype_list<-testing_fun(list_index,b)
  return(genotype_list)}
)})
names(list_INDICES_splitBYName)<-list_of_names
#######data wrangling 1

#var1<-readline(prompt="Gib das Beginn-Datum ein:")
#var2<-readline(prompt="Gib das End-Datum ein:")

#begin<-as.Date(var1_date,  format="%Y-%m-%d")
#end<-as.Date(var2_date,  format="%Y-%m-%d")

##begin<-as.Date("2024-02-21",  format="%Y-%m-%d")#muss ich noch irgendwie mit user-input machen
#end<-as.Date("2024-03-01",  format="%Y-%m-%d")



#dieser Zielpfad ist variabel, kann in CL oder sonstige als Argument übergeben werden....kommt im  meta-coding, das die pipeline in Gang setzt (bash script?, ...)


#2{r,warning=FALSE, message=FALSE}

#auxch hier wäre eine Funbktion noch ganz gut, die mir die dfs in einem
#zusammensschneidet, vllt so in etwa wie die list_extemsion funktion..

list_cutFc<-lapply(list_Fc_Plant, function(x){x%>%dplyr::select('Measuring Date', 'Plant ID', Fm:Size)%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})


list_cutHc<-lapply(list_Hc_Plant, function(x){x%>%dplyr::select('Measuring Date', 'Plant ID', ends_with("avg"), ends_with("median"))%>%dplyr::select(- `NDVI2-avg`, -`NDVI2-median`)%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})


list_cutKin<-lapply(list_Kinetic, function(x){x%>%dplyr::select('Measuring Date', 'Plant ID', '50000':ncol(x) )%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})

list_cutRgb_C<-lapply(list_Rgb_color, function(x){x%>%dplyr::select('Measuring Date','Plant ID', 13:ncol(x))%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})

#list_Rgb_Morpho<-list_Rgb_Morpho[-1]

list_cutRgb_M<-lapply(list_Rgb_Morpho, function(x){x%>%dplyr::select('Measuring Date','Plant ID', AREA_MM:SOL)%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})


list_cutRGB_C_hexcodes<-lapply(list_cutRgb_C, triplet_to_colour)

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

#hier noch einen if-case: falls ein TRUE in 1.anyNA dann lapply für alle cutlists
anyNA(list_meltTables_all, recursive = T)

###################################################
#datawrangling3
##################################################


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


#Fluorescence list separated by Genotype for plotting

list_Fluo_splitBYName<-lapply(list_of_names, function(b){lapply(list_Fluorescence_parameters, function(q){
  genotype_list<-testing_fun(q,b)
  return(genotype_list)}
)})
names(list_Fluo_splitBYName)<-list_of_names

list_fluo<-unlist(list_Fluo_splitBYName, recursive = FALSE)


list_INDICES_splitBYName<-lapply(list_of_names, function(b){lapply(list_indices_Hc, function(q){
  list_index<-indices.to.list(q,"Hc_Plant")
  genotype_list<-testing_fun(list_index,b)
  return(genotype_list)}
)})
names(list_INDICES_splitBYName)<-list_of_names


list_indices<-unlist(list_INDICES_splitBYName, recursive = FALSE)
dates<-unique(list_indices[[1]]$`Measuring Date`)
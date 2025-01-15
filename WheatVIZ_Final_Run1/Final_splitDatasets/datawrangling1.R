#######data wrangling 1

#var1<-readline(prompt="Gib das Beginn-Datum ein:")
#var2<-readline(prompt="Gib das End-Datum ein:")

#begin<-as.Date(var1_date,  format="%Y-%m-%d")
#end<-as.Date(var2_date,  format="%Y-%m-%d")

##begin<-as.Date("2024-02-21",  format="%Y-%m-%d")#muss ich noch irgendwie mit user-input machen
#end<-as.Date("2024-03-01",  format="%Y-%m-%d")

list_dfs<-list()

list1<-list()



for(i in path_allfiles){path<-i
filenames<-list.files(path,pattern = '*.xlsx', recursive=TRUE, full.names = T)
for(i in filenames){
  #if file_path_extnsn==".xlsx: read_excel(i)
  #else:
  newfile<-read_excel(i, na="NaN")
  output<-gsub(path, 
               paste0(newfile$PID[1]),
               tools::file_path_sans_ext(i))
  assign(output, newfile)
  list1[[length(list1)+1]]=output
  list_dfs[[length(list_dfs)+1]]=newfile
  names(list_dfs)<-list1
  #rm(newfile)
  rm(output)
}}

#2{r,warning=FALSE, message=FALSE}

list_allLists<-list_extension(names(list_dfs)) 
#checkliste: falls fehlende Observations: echo(".....)
#if_else(is_identical(dim(i), dim(i+1),....))so in etwa

list_Fc_Plant<-list_allLists[["list_Fc_Plant"]]
list_Hc_Plant<-list_allLists[["list_Hc_Plant"]]
list_Kinetic<-list_allLists[["list_Kinetic"]]
list_avgSpectrum<-list_allLists[["list_Hc_avgSpectrum"]]
list_Rgb_color<-list_allLists[["list_Rgb_Color_Plant"]]
list_Rgb_Morpho<-list_allLists[["list_Rgb_Morpho_Plant"]]



#auxch hier wäre eine Funbktion noch ganz gut, die mir die dfs in einem
#zusammensschneidet, vllt so in etwa wie die list_extemsion funktion..

list_cutFc<-lapply(list_Fc_Plant, function(x){x%>%
    dplyr::select('Measuring Date', 'Plant ID', Fm:Size)%>%
    mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%
    as.data.frame(.)})


list_cutHc<-lapply(list_Hc_Plant, function(x){x%>%dplyr::select('Measuring Date', 'Plant ID', ends_with("avg"), ends_with("median"))%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})

###########################################################

list_cutKin<-lapply(list_Kinetic, function(x){x%>%dplyr::select('Measuring Date', 'Plant ID', '50000':ncol(x) )%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})

list_cutRgb_C<-lapply(list_Rgb_color, function(x){x%>%dplyr::select('Measuring Date','Plant ID', 13:ncol(x))%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})

#list_Rgb_Morpho<-list_Rgb_Morpho[-1]

list_cutRgb_M<-lapply(list_Rgb_Morpho, function(x){x%>%dplyr::select('Measuring Date','Plant ID', AREA_MM:SOL)%>%mutate('Measuring Date'=gsub("\\s\\d+.+", "",x$`Measuring Date`[1]))%>%as.data.frame(.)})


list_cutRGB_C_hexcodes<-lapply(list_cutRgb_C, triplet_to_colour)






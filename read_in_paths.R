
#Eingabe read in paths:

###if folder data is available in the same directory as the code for the pipeline, you may as well use
#these template paths:

water_readings2<-
 read_csv2("data_Round1/metafiles/water_readings_correctDate_1.CSV") #auch dieser Pfad wird in CL übergeben werden

path_allfiles<-"data_Round1/XLSX"

tempandothermeta<-read_excel("data_Round1/metafiles/Temperature_andOther_GC.xlsx")


Bonitur_1<-read_excel("data_Round1/metafiles/WheatVIZ_ClimateChamber_2024_Masterfile.xlsx")

weizenernte_full<-read_excel("data_Round1/metafiles/Weizenernte.xlsx")


#water_readings2<-
  #read_csv2()

#path_allfiles<-

#tempandothermeta<-read_excel()


#Bonitur_1<-read_excel()

#weizenernte_full<-read_excel()


#Eingabe Beginn und Ende Trockenstressphase

stress_begin<-"2024-02-12"
stress_end<-"2024-03-01"


#einlesen der files aus folder
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



list_allLists<-list_extension(names(list_dfs)) 
#checkliste: falls fehlende Observations: echo(".....)
#if_else(is_identical(dim(i), dim(i+1),....))so in etwa

list_Fc_Plant<-list_allLists[["list_Fc_Plant"]]
list_Hc_Plant<-list_allLists[["list_Hc_Plant"]]
list_Kinetic<-list_allLists[["list_Kinetic"]]
list_Rgb_color<-list_allLists[["list_Rgb_Color_Plant"]]
list_Rgb_Morpho<-list_allLists[["list_Rgb_Morpho_Plant"]]



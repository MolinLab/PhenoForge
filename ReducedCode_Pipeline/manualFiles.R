####data rwangling Bonitur

Bonitur_1<-read_excel("C:/Users/HabisohnC/Desktop/WhaetVIZ_metadata/WheatVIZ_ClimateChamber_2024_Masterfile.xlsx")

weizenernte_full<-read_excel("C:/Users/HabisohnC/Desktop/Folder_StandDerDinge/Round1/metafiles/Weizenernte.xlsx")

bonitur_run1<-Bonitur_1[121:nrow(Bonitur_1),]%>%filter(Location=="ClimateChamber")

vector_PlantID_allBonitur<-c()

Bonitur_mitPlantID<-for(i in seq(1:nrow(bonitur_run1))){
  bonitur_PlantID_allBonitur<-paste(bonitur_run1$Tray[i],bonitur_run1$GenotypeNr[i],
                                    sep="_")
  
  vector_PlantID_allBonitur<-append(vector_PlantID_allBonitur,bonitur_PlantID_allBonitur)
}



Bonitur_all_slim<-cbind.data.frame(bonitur_run1, vector_PlantID_allBonitur)

Bonitur_growthCHamber<-Bonitur_all_slim



Bonitur_growthCHamber<-Bonitur_all_slim%>%
  mutate(Date=replace(Date, Date == "20240201", "2024-02-09"))%>%
  mutate(Date=replace(Date, Date == "20240214", "2024-02-14"))%>%
  mutate(Date=replace(Date, Date == "20240223", "2024-02-23"))%>%
  mutate(Date=replace(Date, Date == "20240314", "2024-03-13"))


Bonitur_growthCHamber<-Bonitur_growthCHamber%>%
  mutate(Treatment= replace(Treatment,Treatment=="irrigation", "Control"))%>%
  mutate(Treatment= replace(Treatment,Treatment=="drought stress", "Drought stress"))

Bonitur_growthCHamber<-Bonitur_growthCHamber%>%mutate(Nr=NULL, Location=NULL, 
                                                      x_coordinate=NULL, y_coordinate=NULL,Replication=NULL,
                                                      BBCH=NULL,BBCHmax=NULL, BBCHmin=NULL, Comment=NULL)
Bonitur_growthCHamber<-Bonitur_growthCHamber%>%dplyr::rename('Measuring Date'=Date)%>%
  dplyr::rename('Plant ID'=vector_PlantID_allBonitur)%>%dplyr::rename(Condition=Treatment)%>%
  dplyr::rename(Genotype=GenotypeName)%>%dplyr::relocate('Plant ID', .after='Measuring Date')%>%
  mutate(TillerNumber=as.numeric(TillerNumber))

####data wrangling weizenernte
weizenernte<-weizenernte_full%>%dplyr::select(TG:Tray)%>%relocate(`Plant ID`, .before = TG)
orderedYield_descending<-weizenernte[order(weizenernte$TG, decreasing = TRUE),]
orderedYield_descending<-orderedYield_descending%>%dplyr::select(-Tray)%>%inner_join(metatable, by="Plant ID")
orderdYield_DS<-orderedYield_descending%>%dplyr::filter(Condition=="Drought stress")
###data wrangling Indices_dfs

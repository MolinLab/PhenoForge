####data rwangling Bonitur



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

Bonitur_growthCHamber<-Bonitur_growthCHamber%>%
  mutate(Nr=NULL, Location=NULL, 
  x_coordinate=NULL, y_coordinate=NULL,Replication=NULL,
  BBCHmax=NULL, BBCHmin=NULL, Comment=NULL, Tray=NULL, GeneralImpression=NULL, GenotypeNr=NULL)

Bonitur_growthCHamber<-Bonitur_growthCHamber%>%dplyr::rename('Measuring Date'=Date)%>%
  dplyr::rename('Plant ID'=vector_PlantID_allBonitur)%>%dplyr::rename(Condition=Treatment)%>%
  dplyr::rename(Genotype=GenotypeName)%>%dplyr::relocate('Plant ID', .after='Measuring Date')%>%
  mutate(TillerNumber=as.numeric(TillerNumber))%>%mutate(BBCH=as.numeric(BBCH))




####data wrangling weizenernte
weizenernte<-weizenernte_full%>%dplyr::select(TG:Tray)%>%relocate(`Plant ID`, .before = TG)%>%na.omit()
#orderedYield_descending<-weizenernte[order(weizenernte$TG, decreasing = TRUE),]
#orderedYield_descending<-orderedYield_descending%>%dplyr::select(-Tray)%>%inner_join(metatable, by="Plant ID")
#orderdYield_DS<-orderedYield_descending%>%dplyr::filter(Condition=="Drought stress")



##################################################
#water
#################################################


for(i in water_readings2$Date){i<-gsub("\\s\\d+.+", "", water_readings2$Date)
water_readings2$Date<-i}

water_readings_DSOnly<-water_readings2%>%dplyr::select(contains("DS"))%>%mutate(Date=water_readings2$Date)%>%relocate(Date)

water_readings_DSOnly[,2:ncol(water_readings_DSOnly)]<-sapply(water_readings_DSOnly[,2:ncol(water_readings_DSOnly)], as.numeric)

water_readings_DSOnly[is.na(water_readings_DSOnly)]<-0

water_readings_justValues<-water_readings_DSOnly[,-1]#brauch ich für meine Funktion"build_water_info()"

#water_readings_DSOnly_mean_sd<-water_readings_DSOnly%>%mutate(Mean=rowMeans(.[,2:ncol(.)]))%>%
# mutate(SD=rowSds(as.matrix(.[,2:(ncol(.)-1)])))


water_mean<-water_readings_DSOnly%>%mutate(Mean=rowMeans(water_readings_DSOnly[,-1]))

#################################################
#metadata
################################################



Date_format<-format(as.POSIXct(tempandothermeta$Time,format = '%m/%d/%Y %H:%M:%S'),format = '%Y-%m-%d')

tempandothermeta<-tempandothermeta%>%mutate(Date=ymd(Date_format))%>%relocate(Date)
tempandothermeta_2<-tempandothermeta%>%dplyr::select(-Time)
#long_tempandothermeta<-melt(tempandothermeta, measure.vars = 3:9)


#plot_tempandso<-ggplot(long_tempandothermeta, aes(x=Date, y=value, color=variable))+geom_point()
#passt nicht, war der mean aus param1+param2, nach min und max getrennt!
#meta_min<-tempandothermeta_2%>%group_by(Date)%>%summarise(across(c(Temp1,RH1,), sum))
#meta_minmax<-inner_join(meta_min,meta_max, by="Date")
#meta_mean<-tempandothermeta_2%>%group_by(Date)%>%summarise(across(c(TempAvg,RHAvg), mean))
#meta_meanmaxmin<-inner_join(meta_mean, meta_minmax, by="Date")

#ggplot(meta_meanmaxmin, aes(x=Date))+geom_line(aes(y=Temp1),colour="blue")+
# geom_line(aes(y=Temp2),colour="yellow")
meta_data_mean<-tempandothermeta_2%>%
  mutate(Tempsum=(Temp1+Temp2)/2,
         RHsum=(RH1+RH2)/2)

metad_maxT<-meta_data_mean%>%group_by(Date)%>%
  summarise(across(c(Tempsum,RHsum),max))%>%
  mutate(Tempmax=Tempsum, RHmax=RHsum, RHsum=NULL,Tempsum=NULL)
metad_minT<-meta_data_mean%>%group_by(Date)%>%
  summarise(across(c(Tempsum,RHsum), min))%>%
  mutate(Tempmin=Tempsum, RHmin=RHsum,RHsum=NULL,Tempsum=NULL)


meta_data<-tempandothermeta_2%>%group_by(Date)%>%
  summarise(across(c(RHAvg,TempAvg,CO2), mean))%>%
  inner_join(metad_maxT, by="Date")%>%inner_join(metad_minT, by="Date")


#Bonitur_script


#für zwei Boniturtage
for(i in seq(1:nrow(bonitur_relevant))){
  bonitur_PlantID<-paste(bonitur_relevant$Tray[i],bonitur_relevant$GenotypeNr[i],
                         sep="_")
  
  vector_PlantID<-append(vector_PlantID,bonitur_PlantID)
}

vector_PlantID

Bonitur_tiny<-cbind.data.frame(bonitur_relevant$Plant.ID, bonitur_relevant$LeafNumber,
                               bonitur_relevant$TillerNumber)


#für alle Boniturtage
vector_PlantID_allBonitur<-c()

Bonitur_mitPlantID<-for(i in seq(1:nrow(bonitur_run1))){
  bonitur_PlantID_allBonitur<-paste(bonitur_run1$Tray[i],bonitur_run1$GenotypeNr[i],
                                    sep="_")
  
  vector_PlantID_allBonitur<-append(vector_PlantID_allBonitur,bonitur_PlantID_allBonitur)
}



Bonitur_all_slim<-cbind.data.frame(bonitur_run1, vector_PlantID_allBonitur)




#Boniturplot

Bonitur_growthCHamber<-Bonitur_all_slim%>%filter(Location=="ClimateChamber")

Bonitur_growthCHamber<-Bonitur_growthCHamber%>%
  mutate(Date=replace(Date, Date == "20240122", "2024-01-22"))


Bonitur_growthCHamber<-Bonitur_growthCHamber%>%
  mutate(Date=replace(Date, Date == "20240201", "2024-02_01"))%>%
  mutate(Date=replace(Date, Date == "20240214", "2024-02_14"))%>%
  mutate(Date=replace(Date, Date == "20240223", "2024-02_23"))%>%
  mutate(Date=replace(Date, Date == "20240314", "2024-03-14"))


Bonitur_growthCHamber<-Bonitur_growthCHamber%>%
  mutate(Treatment= replace(Treatment, Treatment=="irrigated", "irrigation"))

Bonitur_growthCHamber_slim<-cbind.data.frame(Bonitur_growthCHamber$Date,
                                             Bonitur_growthCHamber$vector_PlantID_allBonitur,
                                             Bonitur_growthCHamber$Treatment, 
                                             Bonitur_growthCHamber$LeafNumber,
                                             Bonitur_growthCHamber$TillerNumber)
colnames(Bonitur_growthCHamber_slim)<-c("Date", "Plant.ID", "Treatment", "LeafNumber", "TillerNumber")
Bonitur_growthCHamber_slimMelt<-melt(Bonitur_growthCHamber_slim, measure.vars = 4:5)
plot_Bonitur<-Bonitur_growthCHamber_slimMelt%>%filter(variable=="LeafNumber")%>%ggplot(aes(x=Date, y=value, color=Treatment))+geom_point()
plot_Bonitur


#besserer plot
Bonitur_growthCHamber_slimMelt%>%
  filter(variable=="LeafNumber")%>%
  ggplot(aes(x=Date, y=as.numeric(as.character(value)), 
             color=Treatment))+geom_point(aes(size=value))+
  scale_shape_manual(values=c("23", "25"))+
  theme_bw()+ylab("Leaf Number")+labs(title = "Leaf Number over Time")
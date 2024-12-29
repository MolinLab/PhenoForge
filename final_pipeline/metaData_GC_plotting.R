

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

temp<-ggplot(meta_data, aes(x=Date))+geom_rect(aes(xmin=begin, xmax=end,ymin=-Inf,ymax=Inf),
                                               fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line(aes(y=Tempmin, colour="Min"))+ylab("T, [°C]")+geom_line(aes(y=Tempmax, colour="Max"))+
  
  scale_colour_manual(name="Parameter", values=c("Min"="darkblue", "Max"="orange"))

hum<-ggplot(meta_data, aes(x=Date))+geom_rect(aes(xmin=begin, xmax=end,ymin=-Inf,ymax=Inf),
                                              fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line(aes(y=RHmax, colour="Max"))+
  geom_line(aes(y=RHmin, colour="Min"))+ylab("Humidity, [%]")+
  scale_colour_manual(name="Parameter", values=c("Min"="darkblue", "Max"="orange"))

oneof3_1<-ggplot(meta_data, aes(x=Date, y=CO2))+scale_y_continuous(name="Mean CO2, [ppm]")+
  geom_rect(aes(xmin=begin, xmax=end,ymin=-Inf,ymax=Inf),
        fill="lightgrey",alpha=0.02,color="lightgrey")+geom_line()+xlab(NULL)+
  theme(axis.text.y=element_text(size=8))

oneof3_3<-ggplot(meta_data,aes(x=Date, y=RHAvg))+scale_y_continuous(name="Mean Humidity, [%]")+
  geom_rect(aes(xmin=begin, xmax=end,ymin=-Inf,ymax=Inf),
  fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line()+xlab(NULL)+theme(axis.text.y=element_text(size=8))

oneof3_2<-ggplot(meta_data,aes(x=Date, y=TempAvg))+scale_y_continuous(name="Mean T, [°C]")+
  geom_rect(aes(xmin=begin, xmax=end,ymin=-Inf,ymax=Inf),
             fill="lightgrey",alpha=0.02,color="lightgrey")+geom_line()+
  theme(axis.text.y=element_text(size=8))

water<-ggplot(water_mean, aes(x=as.Date(Date), y=Mean))+xlab("Date")+
  scale_y_continuous(name="Mean water potential [kPa]")+
  geom_rect(aes(xmin=begin, xmax=end,ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line(colour="blue")


all3<-ggarrange(oneof3_1,oneof3_3,oneof3_2, ncol=1, widths = c(0.5,1))
all5<-ggarrange(temp,hum, all3,water, ncol=1, common.legend = T,legend = "top", heights = c(1,1,2,1))





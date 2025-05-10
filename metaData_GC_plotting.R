
temp<-ggplot(meta_data, aes(x=Date))+geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),
                                               fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line(aes(y=Tempmin, colour="Min"))+ylab("T, [°C]")+geom_line(aes(y=Tempmax, colour="Max"))+
  
  scale_colour_manual(name="Parameter", values=c("Min"="darkblue", "Max"="orange"))+theme_bw()

hum<-ggplot(meta_data, aes(x=Date))+geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),
                                              fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line(aes(y=RHmax, colour="Max"))+
  geom_line(aes(y=RHmin, colour="Min"))+ylab("Humidity, [%]")+
  scale_colour_manual(name="Parameter", values=c("Min"="darkblue", "Max"="orange"))+theme_bw()

oneof3_1<-ggplot(meta_data, aes(x=Date, y=CO2))+scale_y_continuous(name="Mean CO2, [ppm]")+
  geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),
            fill="lightgrey",alpha=0.02,color="lightgrey")+geom_line()+xlab(NULL)+
  theme(axis.text.y=element_text(size=8))+theme_bw()

oneof3_3<-ggplot(meta_data,aes(x=Date, y=RHAvg))+scale_y_continuous(name="Mean Humidity, [%]")+
  geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),
            fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line()+xlab(NULL)+theme(axis.text.y=element_text(size=8))+theme_bw()

oneof3_2<-ggplot(meta_data,aes(x=Date, y=TempAvg))+scale_y_continuous(name="Mean T, [°C]")+
  geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),
            fill="lightgrey",alpha=0.02,color="lightgrey")+geom_line()+
  theme(axis.text.y=element_text(size=8))+theme_bw()

water<-ggplot(water_mean, aes(x=as.Date(Date), y=Mean))+xlab("Date")+
  scale_y_continuous(name="Mean water potential [kPa]")+
  geom_rect(aes(xmin=as.Date(stress_begin), xmax=as.Date(stress_end),ymin=-Inf,ymax=Inf),fill="lightgrey",alpha=0.02,color="lightgrey")+
  geom_line(colour="blue")+theme_bw()


all3<-ggarrange(oneof3_1,oneof3_3,oneof3_2, ncol=1, widths = c(0.5,1))
all5<-ggarrange(temp,hum, all3,water, ncol=1, common.legend = T,legend = "top", heights = c(1,1,2,1))


#ggplot(CamData_Bonitur_TG_cut[[7]], aes(x=Genotype, y=`NDVI-avg`, colour=Condition))+geom_boxplot()
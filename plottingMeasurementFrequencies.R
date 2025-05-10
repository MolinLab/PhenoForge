
#all measurement dtaes as datafarme
dates_RGB<-lapply(list_cutRgb_C, function(x){print(x$`Measuring Date`[1])})
tiptop<-as.data.frame(unlist(dates_RGB))
rownames(tiptop)<-NULL
colnames(tiptop)<-"Date"
tiptop$RGB<-rep("TRUE", length(tiptop))



dates_FC<-lapply(list_cutFc, function(x){print(x$`Measuring Date`[1])})%>%unlist()%>%as.data.frame()
rownames(dates_FC)<-NULL
colnames(dates_FC)<-"Date"
dates_FC$FC<-rep("TRUE", length(dates_FC))


dates_Hc<-lapply(list_cutHc, function(x){print(x$`Measuring Date`[1])})%>%unlist()%>%as.data.frame()
rownames(dates_Hc)<-NULL
colnames(dates_Hc)<-"Date"
dates_Hc$HC<-rep("TRUE", length(dates_Hc))


allframe<-full_join(dates_Hc, dates_FC, by="Date")

allframe<-full_join(allframe, tiptop, by="Date")

allframe_melt<-melt(allframe, measure.vars = 2:4)
allframe_mlettest<-na.omit(allframe_melt)



ggplot(allframe_mlettest, aes(x=Date, y=variable))+geom_point()

measFrequ<-ggplot(allframe_mlettest, aes(x=Date, y=variable, colour=variable))+geom_point()+
  scale_color_discrete(labels=c("Hyperspectral", "Fluorescence", "Red Green Blue"))+
  theme(axis.text.x = element_text(angle=75, hjust=1),plot.title = element_text(size=16), 
        legend.text=element_text(size=12),legend.title = element_text(size=13), 
        axis.title.x = element_text(hjust=1),
        axis.text=element_text(size=9),axis.title=element_text(size=14))+
  labs(title= "Measurement schedule", colour="Type")+ylab("Measurement type")



allframe<-merge(data.frame(unlist(dates_FC), row.names=NULL), data.frame(unlist(dates_Hc), row.names=NULL), 
      by = 0, all = TRUE)

dates_FC<-lapply(list_cutFc, function(x){print(x$`Measuring Date`[1])})%>%unlist()%>%as.data.frame()
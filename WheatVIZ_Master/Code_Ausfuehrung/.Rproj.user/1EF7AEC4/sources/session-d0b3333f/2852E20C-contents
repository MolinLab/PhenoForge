


#plot_all_datapoints_over_experiment




select_columns<-function(x,y){
  result<-lapply(x, function(z){
    z%>%select(all_of(y))})
  return(result)
}

vector_Messtager<-c()

testr<-select_columns(names_andMCARI1avg, "value")%>%bind_cols()
for(i in names_andMCARI1avg){vector_Messtager[length(vector_Messtager)+1]<-
  i$Measuring.Date[1]}

vector_Messtager<-append(vector_Messtager, "Condition")
vector_new<-as.character(vector_Messtager)

#colnames(testr_new)<-vector_new
#ich brauche noch die Info Condition dazu

vector_Condition<-names_andMCARI1avg[[1]]$Condition
dataframe1<-data.frame(Condition=vector_Condition)

identical(names_andMCARI1avg[[1]]$Condition, names_andMCARI1avg[[8]]$Condition)
#[1] TRUE

testr_new<-select_columns(names_andMCARI1avg, "value")%>%
  cbind.data.frame(dataframe1)

colnames(testr_new)<-vector_new



melt_MCARI1<-melt(testr_new, measure.vars = 1:14)

ggplot(melt_MCARI1, aes(x=variable, y=value, color=Condition))+
  ylab("MCARI1")+xlab("Date")+labs(title="MCARI1 all Genotypes")+
  geom_point()+theme_bw(base_size = 10)+
  scale_color_manual(values = c("Control"="turquoise3", 
                                "Drought stress"="coral"))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
  geom_smooth(method="lm", aes(color=Condition, group=Condition))+geom_point()

ggsave(filename = "MCARI_allPlot.pdf")


###############pipeline from names_andINDEX .... to plot +geom_smooth() :))

#NDVI

NDVI_allGenotypes<-select_columns(names_andNDVIavg, "value")%>%
  cbind.data.frame(dataframe1)

colnames(NDVI_allGenotypes)<-vector_new

melt_NDVI<-melt(NDVI_allGenotypes
                , measure.vars = 1:14)


ggplot(melt_NDVI, aes(x=variable, y=value, color=Condition))+
  ylab("NDVI")+xlab("Date")+labs(title="NDVI all Genotypes")+
  geom_point()+theme_bw(base_size = 10)+
  scale_color_manual(values = c("Control"="turquoise3", 
                                "Drought stress"="coral"))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
  geom_smooth(method="lm", aes(color=Condition, group=Condition))+geom_point()

ggsave(filename = "NDVI_allPlot.pdf")

#QYMax

QYMax_allGenotypes<-select_columns(names_andQY_max, "value")%>%
  cbind.data.frame(dataframe1)

colnames(QYMax_allGenotypes)<-vector_new

melt_QYMax<-melt(QYMax_allGenotypes
                 , measure.vars = 1:14)


ggplot(melt_QYMax, aes(x=variable, y=value, color=Condition))+
  ylab("QY max")+xlab("Date")+labs(title="QY max all Genotypes")+
  geom_point()+theme_bw(base_size = 10)+
  scale_color_manual(values = c("Control"="turquoise3", 
                                "Drought stress"="coral"))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=9))+
  geom_smooth(method="lm",aes(color=Condition, group=Condition))+geom_point()

ggsave(filename = "QYMax_allPlot.pdf")
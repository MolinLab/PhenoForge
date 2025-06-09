
#Anovas rechnen: NDVI
names(CamData_Bonitur_TG)<-dates
names(CamData_Bonitur_TG_cut)<-dates

dir.create("Anovas/")

#testing for outliers and normaility for the relevant measuring dates before performing Anovas:

###normality for 26th feb

#dir.create("Anovas/Assumptions/")
#assumptions1<-qqPlot(CamData_Bonitur_TG_cut[[6]]$`NDVI-avg`, ylab = "Sample Quartiles", main = "Data NDVI February 26th")

#out1<-rosnerTest(CamData_Bonitur_TG_cut[[6]]$`NDVI-avg`, k=3)

#28th feb
#assumptions2<-qqPlot(CamData_Bonitur_TG_cut[[7]]$`NDVI-avg`, ylab = "Sample Quartiles", main = "Data NDVI February 28th")

#out2<-rosnerTest(CamData_Bonitur_TG_cut[[7]]$`NDVI-avg`, k=1)


#1st March
#assumptions3<-qqPlot(CamData_Bonitur_TG_cut[[8]]$`NDVI-avg`, ylab = "Sample Quartiles", main = "Data NDVI March 1st")

#out3<-rosnerTest(CamData_Bonitur_TG_cut[[8]]$`NDVI-avg`, k=2)


#list_out<-list(out1,out2,out3)
#capture.output(out1, file = "Anovas/Assumptions/Ass_NDVI.txt")


####Anovas
list_anovas_NDVI_contrsum<-lapply(CamData_Bonitur_TG, function(an){
  model_anova<-aov(`NDVI-avg`~ Condition + Genotype +Genotype*Condition,
                   data=an,
                   contrasts = list(Condition="contr.sum", Genotype="contr.sum") )
  return(model_anova)
})

list_AnovasNDVI_tables_contrsum<-lapply(list_anovas_NDVI_contrsum, function(table){
  anovas_table<-Anova(table, type="III")
})



capture.output(list_AnovasNDVI_tables_contrsum, file = "Anovas/NDVI.txt")

##posthoc

#NDVI

list_anovas_NDVI_allPosthocs<-lapply(names(list_anovas_NDVI_contrsum), function(name){
  xyz<-list_anovas_NDVI_contrsum[[name]]
  emmobject<-emmeans(xyz, ~Condition*Genotype)
  
  cld_emmtable<-cld(emmobject, alpha=0.05, Letters = letters)
  date<-sub("^([0-9]+-[0-9]+-[0-9]+)\\..*$", "\\1", name)
  
  cld_emmtable$Date<-date
  
  return(cld_emmtable)
})
  
capture.output(list_anovas_NDVI_allPosthocs, append = TRUE,file = "Anovas/NDVI_posthoc.txt")

library(readxl)
library(tidyverse)
library(reshape2)
library(reshape)
library(stringr)
library(ggpubr)
library(car)
library(sjmisc)
library(emmeans)
library(multcomp)
library(GGally)
library(rstatix)
library(stats)
library(ggbiplot)
library(cowplot)
library(factoextra)
library(ggfortify)
library(effectsize)
library(EnvStats)

source("functions_updated.R", local = knitr::knit_global() )
source("read_in_paths.R", local = knitr::knit_global() )
source("metatable.R", local = knitr::knit_global() )
source("manualFiles.R",local = knitr::knit_global())
source("datawrangling1.R", local = knitr::knit_global() )
source("addScoreInfo.R",local = knitr::knit_global())
source("bisect_data.R", local = knitr::knit_global() )
####################
#Anovas rechnen: NDVI
names(CamData_Bonitur_TG)<-dates
names(CamData_Bonitur_TG_cut)<-dates

dir.create("Anovas/")

#testing for outliers and normaility for the relevant measuring dates before performing Anovas:

###normality for 26th feb

dir.create("Anovas/Assumptions/")
assumptions1<-qqPlot(CamData_Bonitur_TG_cut[[6]]$`NDVI-avg`, ylab = "Sample Quartiles", main = "Data NDVI February 26th")

out1<-rosnerTest(CamData_Bonitur_TG_cut[[6]]$`NDVI-avg`, k=3)

#28th feb
#assumptions2<-qqPlot(CamData_Bonitur_TG_cut[[7]]$`NDVI-avg`, ylab = "Sample Quartiles", main = "Data NDVI February 28th")

#out2<-rosnerTest(CamData_Bonitur_TG_cut[[7]]$`NDVI-avg`, k=1)


#1st March
#assumptions3<-qqPlot(CamData_Bonitur_TG_cut[[8]]$`NDVI-avg`, ylab = "Sample Quartiles", main = "Data NDVI March 1st")

#out3<-rosnerTest(CamData_Bonitur_TG_cut[[8]]$`NDVI-avg`, k=2)


#list_out<-list(out1,out2,out3)
capture.output(out1, file = "Anovas/Assumptions/Ass_NDVI.txt")


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

list_AnovasNDVI_tables_contrsum[[6]]

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
  

####checking assumptions

assumptions_anova<-car::qqPlot(list_anovas_NDVI_contrsum[[6]]$residuals, 
            ylab = "Sample Quantiles",main= "Residuals ANOVA February 26th", id=F)


Tu<-as.data.frame(list_anovas_NDVI_contrsum[[6]]$residuals)


normquant_check<-ggplot(Tu, aes(x=list_anovas_NDVI_contrsum[[6]]$residuals))+geom_histogram(bins = 10)+xlab("Residuals")

#checking homoskedasticity being met:
levene<-leveneTest(`NDVI-avg`~Condition*Genotype, CamData_Bonitur_TG_cut[[6]])

capture.output(levene, file = "Anovas/Assumptions/Ass_NDVI.txt", append = T)



#checking ouliers for 26th:
outlier_check_C<-CamData_Bonitur_TG_cut[[6]]%>%dplyr::filter(Condition=="Control")
outanova<-rosnerTest(outlier_check_C$`NDVI-avg`, k=1)
capture.output(outanova, file = "Anovas/Assumptions/Ass_NDVI.txt", append = T)

outlier_check_DS<-CamData_Bonitur_TG_cut[[6]]%>%dplyr::filter(Condition=="Drought stress")
outAnova2<-rosnerTest(outlier_check_DS$`NDVI-avg`, k=2)
capture.output(outAnova2, file = "Anovas/Assumptions/Ass_NDVI.txt", append = T)
#checking outliers residuals 26th:
outlier_check_residuals<-rosnerTest(list_anovas_NDVI_contrsum[[6]]$residuals, k=2)
capture.output(outlier_check_residuals, file = "Anovas/Assumptions/Ass_NDVI.txt", append = T)
#effectsize
omega_26th<-effectsize::omega_squared(list_AnovasNDVI_tables_contrsum[[6]], partial = TRUE)
capture.output(omega_26th, file = "Anovas/Assumptions/Ass_NDVI.txt", append = T)



#NDVI posthoc plot
dir.create("plots/stats_posthoc")
posthoc_path<-"plots/stats_posthoc/"


z<-list_anovas_NDVI_allPosthocs[[6]]

df_filtered <- z %>%dplyr::filter(Condition=="Drought stress")%>%
  group_by(.group) %>%
  filter(n() > 1) %>%
  ungroup()




stat_test<-df_filtered%>%group_by(".group")%>%t_test(emmean~.group, p.adjust.method = "bonferroni", var.equal = FALSE)%>%mutate(y.position = max(z$emmean))

one<-ggplot(z, aes(x=.group, y=emmean, color=Condition))+geom_point()+labs(title= z$Date[1],x="group")+
  
  scale_colour_manual(values = c("Control"="turquoise3", 
                                 "Drought stress"="coral"))+theme_bw()


one_stats<-one+stat_pvalue_manual(stat_test, label = "p.adj", y.position = "y.position", step.increase = .1)

ggsave(one_stats, filename=paste0(posthoc_path, paste0(z$Date[1]), "posthocNDVI.png"))

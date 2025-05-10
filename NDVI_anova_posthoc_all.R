#Anovas rechnen: NDVI
names(CamData_Bonitur_TG)<-dates
names(CamData_Bonitur_TG_cut)<-dates

dir.create("Anovas/")
###normality for 26th (e.g)
qqPlot(CamData_Bonitur_TG_cut[[6]]$`NDVI-avg`, ylab = "Sample Quartiles")

rosnerTest(CamData_Bonitur_TG_cut[[6]]$`NDVI-avg`, k=3)



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

car::qqPlot(list_anovas_NDVI_contrsum[[6]]$residuals, 
            ylab = "Sample Quantiles", id=T)



Tu<-as.data.frame(list_anovas_NDVI_contrsum[[6]]$residuals)


normquant_check<-ggplot(Tu, aes(x=list_anovas_NDVI_contrsum[[6]]$residuals))+geom_histogram(bins = 10)+xlab("Residuals")

#checking homoskedasticity being met:
leveneTest(`NDVI-avg`~Condition*Genotype, CamData_Bonitur_TG_cut[[6]])
#car::qqPlot(list_anovas_NDVI[[6]]$residuals, id=F)


#effectsize
effectsize::eta_squared(list_AnovasNDVI_tables_contrsum[[6]], partial = TRUE)



#checking ouliers for 26th:
outlier_check<-CamData_Bonitur_TG_cut[[6]]%>%dplyr::filter(Condition=="Control")
rosnerTest(outlier_check$`NDVI-avg`, k=1)

outlier_check_DS<-CamData_Bonitur_TG_cut[[6]]%>%dplyr::filter(Condition=="Drought stress")
rosnerTest(outlier_check_DS$`NDVI-avg`, k=2)


dir.create("plots/emmeans")
path_em<-"plots/emmeans/"

#NDVI

anovasNDVIposthocs_rbound<-do.call("rbind", list_anovas_NDVI_allPosthocs)
Mean_emmean_Groups<-anovasNDVIposthocs_rbound%>%group_by(Date, Condition)%>%
  summarise(Mean=mean(emmean), SD=sd(emmean))

plot_emmeans_NDVI<-ggplot(Mean_emmean_Groups, aes(x=Date, y=Mean, colour=Condition, group=Condition))+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2)+
  ylab("Mean estimated marginal mean (EMM)")+labs(title="NDVI Anova (emmeans)")+theme_bw()+
  theme(axis.text.x = element_text(angle=75, hjust=1.1), 
        axis.title.x = element_text(hjust=1.05))+
  scale_colour_manual(values = c("Control"="turquoise3", 
                                 "Drought stress"="coral"))
ggsave(plot_emmeans_NDVI, filename=paste0(path_em, "NDVI.png"))


#plotting posthoc estimated amrginal means

#code posthoc NDVI





dir.create("plots/stats_posthoc")
posthoc_path<-"plots/stats_posthoc/"


z<-list_anovas_NDVI_allPosthocs[[6]]

df_filtered <- z %>%dplyr::filter(Condition=="Drought stress")%>%
  group_by(.group) %>%
  filter(n() > 1) %>%
  ungroup()




stat_test<-df_filtered%>%group_by(".group")%>%t_test(emmean~.group, p.adjust.method = "bonferroni", var.equal = FALSE)%>%mutate(y.position = max(z$emmean))

one<-ggplot(z, aes(x=.group, y=emmean, color=Condition))+geom_boxplot()+labs(title= z$Date[1],x="group")+
  
  scale_colour_manual(values = c("Control"="turquoise3", 
                                 "Drought stress"="coral"))+theme_bw()


one_stats<-one+stat_pvalue_manual(stat_test, label = "p.adj", y.position = "y.position", step.increase = .1)

ggsave(one_stats, filename=paste0(posthoc_path, paste0(z$Date[1]), "posthocNDVI.png"))

ds_26<-CamData_Bonitur_TG_cut[[6]]%>%dplyr::filter(Condition=="Drought stress")
skewness(ds_26$`NDVI-avg`)
qqPlot(ds_26$`NDVI-avg`)


list_anovas_NDVI_contrsum<-lapply(CamData_Bonitur_TG, function(an){
  model_anova<-aov(`NDVI-avg`~ Condition + Genotype +Genotype*Condition,
                   data=an,
                   contrasts = list(Condition="contr.sum", Genotype="contr.sum") )
  return(model_anova)
})

list_AnovasNDVI_tables_contr_sum<-lapply(list_anovas_NDVI_contrsum, function(table){
       anovas_table<-Anova(table, type="III")
   })

plot(list_AnovasNDVI_tables_contr_sum[[6]])
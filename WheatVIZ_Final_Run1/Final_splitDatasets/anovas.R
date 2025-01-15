
###################
###
#die pfade müssen zum nahtlosen Laufen noch angepasst werden mit create.dir()
#siehe link:https://stackoverflow.com/questions/4216753/folder-management-with-r-check-existence-of-directory-and-create-it-if-it-does



dir.create("Anovas/")

list_anovas_NDVI<-lapply(CamData_Bonitur_TG, function(an){
  
  model_anova<-aov(`NDVI-avg`~ Condition + Genotype +Genotype*Condition, data=an)
  
  return(model_anova)
})

list_AnovasNDVI_tables<-lapply(list_anovas_NDVI, function(table){
  anovas_table<-Anova(table, type="III")
})


#for(i in 1:length(list_anovas_NDVI)){
  
 # list_items<-list(names(list_anovas_NDVI[i]), list_anovas_NDVI[[i]], list_AnovasNDVI_tables[[i]])
  
 # capture.output(list_items, file = "AnovaslistOutput/hegt/NDVI.txt", append = TRUE)
  
#}

capture.output(list_AnovasNDVI_tables, file = "Anovas/NDVI.txt")




list_anovas_Size<-lapply(CamData_Bonitur_TG, function(an){
  
  model_anova<-aov(`Size-avg`~ Condition + Genotype +Genotype*Condition, data=an)
  
  
  
  return(model_anova)
})

list_AnovasSize_tables<-lapply(list_anovas_Size, function(table){
  anovas_table<-Anova(table, type="III")
  
})

capture.output(list_AnovasSize_tables, file = "Anovas/Size.txt")




list_anovas_MCARI<-lapply(CamData_Bonitur_TG, function(an){
  
  if(str_contains(colnames(an), "MCARI1-avg")==TRUE){
  
  model_anova<-aov(`MCARI1-avg`~ Condition + Genotype +Genotype*Condition, data=an)
  
  return(model_anova)
  }
})

list_AnovasMCARI_tables<-lapply(list_anovas_MCARI, function(table){
  
  if(!is.null(table)){
  anovas_table<-Anova(table, type="III")
  }
  
})

capture.output(list_AnovasMCARI_tables, file = "Anovas/MCARI1.txt")








list_anovas_PRI<-lapply(CamData_Bonitur_TG, function(an){
  
  model_anova<-aov(`PRI-avg`~ Condition + Genotype +Genotype*Condition, data=an)
  
  
  
  return(model_anova)
})

list_AnovasPRI_tables<-lapply(list_anovas_PRI, function(table){
  anovas_table<-Anova(table, type="III")
  
})

capture.output(list_AnovasPRI_tables, file = "Anovas/PRI.txt")


#posthoc_2302<-emmeans(, ~ Condition*Genotype)
#for(i in length(list_AnovasNDVI_tables)){print(paste(CamData_Bonitur_TG[i]$`Measuring Date`, list_AnovasNDVI_tables[i], sep = "\n"))}



########### Anova TG

anova_TG_inter<-aov(TG~Genotype+Condition+Condition*Genotype, data=CamData_Bonitur_TG[[1]])
model_aov_TG_inter<-Anova(anova_TG_inter, type="III")


anova_TG<-aov(TG~Genotype+Condition, data=CamData_Bonitur_TG[[1]])
model_aov_TG<-Anova(anova_TG, type="III")

emmeans_TG<-emmeans(anova_TG, ~ Genotype)
cld_TG<-cld(emmeans_TG, alpha=0.05, Letters = letters, adjust="tukey")
#################################################
##### Anova BBCH


list_anovas_BBCH<-lapply(CamData_Bonitur_TG, function(an){
  
  colnames_an<-colnames(an)
  
  if(str_contains(colnames_an, "BBCH")==TRUE){
    
    model_anova<-aov(BBCH~ Condition + Genotype +Genotype*Condition, data=an)
    
    
    
    return(model_anova)}
})


###Liste nur mit vollen matrizen:
list_Anovas_filt<-Filter(Negate(is.null), list_anovas_BBCH)


list_AnovasBBCH_tables<-lapply(list_Anovas_filt, function(table){
  
  if (length(table) == 0){
    
    print("no Anova possible due to NULL matrix")
  }
  else
    anovas_table<-Anova(table, type="III")
  
})

#####################################################
#POSTHOCS
#####################################################
#für die Posthoc tests über listen-loop möchte ich die Datums-Information noch dazu geben; 
#diese Info liegt im Namen der Objekte, das heisst ich loope über die Namen, spreche die Objekte in der Schleife mittels Indices an und gebe eine eigene Spalte mit "Date" zum return object hinzu;
##### posthoc BBCH

list_posthocs_BBCH<-lapply(names(list_Anovas_filt), function(name){
  xyz<-list_Anovas_filt[[name]]
  emmobject<-emmeans(xyz, ~Condition*Genotype)
  
  cld_emmtable<-cld(emmobject, alpha=0.05, Letters = letters, adjust="tukey")
  date<-sub("^([0-9]+-[0-9]+-[0-9]+)\\..*$", "\\1", name)
  
  cld_emmtable$Date<-date
  
  return(cld_emmtable)
})




##gibts als warning beim obigen cld-command:
#Note: adjust = "tukey" was changed to "sidak"
#because "tukey" is only appropriate for one set of pairwise comparisons
#deshalb, ist TUKEY hier angebracht??????
###Tukey posthoc

#tukey_BBCH<-TukeyHSD(list_anovas_BBCH[[5]], conf.level = .95)
#tukey_BBCH

##########################################################################

posthoc_BBCH_2302<-emmeans(list_anovas_BBCH$`2024-02-23`, ~ Condition*Genotype)

posthoc_BBCH2302_cld<-cld(posthoc_BBCH_2302, alpha=0.05, Letters = letters, adjust="tukey")

#3 relevante, signifikante INteraktionen:

###################### NDVI

posthoc_NDVI_2602<-emmeans(list_anovas_NDVI$`2024-02-26`, ~ Condition*Genotype)

posthoc_NDVI2602_cld<-cld(posthoc_NDVI_2602, alpha=0.05, Letters = letters, adjust="tukey")


###################### Size

posthoc_Size_0103<-emmeans(list_anovas_Size$`2024-03-01`, ~Genotype*Condition)

posthoc_Size_0103_cld<-cld(posthoc_Size_0103, alpha=0.05, Letters = letters, adjust="tukey")





#################### MCARI


posthoc_MCARI_0603<-emmeans(list_anovas_MCARI$`2024-03-06`, ~ Condition*Genotype)

posthoc_MCARI0603_cld<-cld(posthoc_MCARI_0603, alpha=0.05, Letters = letters, adjust="tukey")




posthoc_MCARI_1103<-emmeans(list_anovas_MCARI$`2024-03-11` , ~ Condition*Genotype)

posthoc_MCARI1103_cld<-cld(posthoc_MCARI_1103, alpha=0.05, Letters = letters, adjust="tukey")

######################################################################
#################################
#anova_plots

#für einen Zeitreihenplot muss ich erstmal alle posthoc tests machen,
#für den NDVI, um emmeans plotten zu können;
#dann suche und klebe ich die Genotype-Informationn aus den jeweiligen 
#tables aneineander um sie zu plotten

#NDVI

list_anovas_NDVI_allPosthocs<-lapply(names(list_anovas_NDVI), function(name){
  xyz<-list_anovas_NDVI[[name]]
  emmobject<-emmeans(xyz, ~Condition*Genotype)
  
  cld_emmtable<-cld(emmobject, alpha=0.05, Letters = letters, adjust="tukey")
  date<-sub("^([0-9]+-[0-9]+-[0-9]+)\\..*$", "\\1", name)
  
  cld_emmtable$Date<-date
  
  return(cld_emmtable)
  
})


#Size

list_anovas_Size_allPosthocs<-lapply(names(list_anovas_Size), function(name){
  xyz<-list_anovas_Size[[name]]
  emmobject<-emmeans(xyz, ~Condition*Genotype)
  
  cld_emmtable<-cld(emmobject, alpha=0.05, Letters = letters, adjust="tukey")
  date<-sub("^([0-9]+-[0-9]+-[0-9]+)\\..*$", "\\1", name)
  
  cld_emmtable$Date<-date
  
  return(cld_emmtable)
  
})

#MCARI1


list_anovas_MCARI1_allPosthocs<-lapply(names(list_anovas_MCARI), function(name){
  
  if(!is.null(list_anovas_MCARI[[name]])){
  xyz<-list_anovas_MCARI[[name]]
  emmobject<-emmeans(xyz, ~Condition*Genotype)
  
  cld_emmtable<-cld(emmobject, alpha=0.05, Letters = letters, adjust="tukey")
  date<-sub("^([0-9]+-[0-9]+-[0-9]+)\\..*$", "\\1", name)
  
  cld_emmtable$Date<-date
  
  return(cld_emmtable)
  } else{
    
  }
  
})
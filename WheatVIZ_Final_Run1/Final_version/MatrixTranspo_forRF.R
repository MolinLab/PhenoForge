#den table mit allen Messatgen und Indices erstellen

list_Hc_slim<-lapply(list_indices_by_loop, function(x){
  tick<-as.character(x$variable[1])
  p<-x%>%dplyr::select(`Plant ID`,`Measuring Date`, value)%>%
    dplyr::rename(!!tick:=value)
  return(p)
})


#alle Indices über alle Messtage in wide format
allHC_Indexcolumns_w<- reduce(list_Hc_slim,
                              inner_join,
                              by = c("Plant ID", "Measuring Date"))

Genotype_allHC_Indexcolumns<-allHC_Indexcolumns_w%>%
  inner_join(metatable_slim,
             by="Plant ID")%>%
  inner_join(dplyr::select(weizenernte, `Plant ID`, TG),
             by="Plant ID")%>%
  relocate(Genotype, Condition, .after = `Plant ID`)

#alle Messtage über alle Indices in wide format
list_Hc_interm<-allHC_Indexcolumns_w%>%
  pivot_longer(cols = -c(`Measuring Date`, `Plant ID`),
               names_to = "Index", values_to = "Value")
Genotype_allHc_Datescolumns<- list_Hc_interm %>%
  pivot_wider(names_from = `Measuring Date`, 
              values_from = Value)%>%
  inner_join(metatable_slim,
             by="Plant ID")%>%
  relocate(Genotype, Condition,.after = `Plant ID`)%>%
  inner_join(dplyr::select(weizenernte, `Plant ID`, TG),
             by="Plant ID")

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



water_readings_justValues<-water_readings_DSOnly[,-1]
list_water_trayNumbers<-c(1,4,5,8,9,12,13,16,17)
#list_NDVI_NamesTray<-make_trayInfo_tables(list_NDVIavg_andNames)


water_tables_all20Genotypes<-lapply(list_NDVI_NamesTray, build_water_info)

dates<-list_mean_sd_NDVI[[4]]$`Measuring Date`
dates<-unique(dates)

water_tables_all20Genotypes_filtered<-lapply(water_tables_all20Genotypes,
                                             function(x){x[is.element(x$Date,dates),]})

vector_names_plantIDs<-c()
for(i in list_NDVIavg_andNames){
  vector_names_plantIDs[length(vector_names_plantIDs)+1]<-i$`Plant Name`[1]}

names(water_tables_all20Genotypes_filtered)<-vector_names_plantIDs
names(water_tables_all20Genotypes)<-vector_names_plantIDs

water_readings_DSOnly_mean_sd<-water_readings_DSOnly%>%mutate(Mean=rowMeans(.[,2:ncol(.)]))%>%
  mutate(SD=rowSds(as.matrix(.[,2:(ncol(.)-1)])))



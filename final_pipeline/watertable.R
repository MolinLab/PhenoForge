
for(i in water_readings2$Date){i<-gsub("\\s\\d+.+", "", water_readings2$Date)
water_readings2$Date<-i}

water_readings_DSOnly<-water_readings2%>%dplyr::select(contains("DS"))%>%mutate(Date=water_readings2$Date)%>%relocate(Date)

water_readings_DSOnly[,2:ncol(water_readings_DSOnly)]<-sapply(water_readings_DSOnly[,2:ncol(water_readings_DSOnly)], as.numeric)

water_readings_DSOnly[is.na(water_readings_DSOnly)]<-0

water_readings_justValues<-water_readings_DSOnly[,-1]#brauch ich für meine Funktion"build_water_info()"

#water_readings_DSOnly_mean_sd<-water_readings_DSOnly%>%mutate(Mean=rowMeans(.[,2:ncol(.)]))%>%
 # mutate(SD=rowSds(as.matrix(.[,2:(ncol(.)-1)])))


water_mean<-water_readings_DSOnly%>%mutate(Mean=rowMeans(water_readings_DSOnly[,-1]))


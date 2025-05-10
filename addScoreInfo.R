

weizenernte_meanTG<-weizenernte%>%inner_join(metatable_slim, by="Plant ID")%>%
  dplyr::filter(Condition=="Drought stress")%>%group_by(Genotype)%>%summarise(Mean_TG=mean(TG))

weizenernteMean_sorted<-weizenernte_meanTG[order(weizenernte_meanTG$Mean_TG),]

splitsize_mean<-nrow(weizenernte_meanTG)%/% 3

weizenernteMean_sorted$TG_score<-
  c(rep("low", splitsize_mean), rep("medium", splitsize_mean), rep("high", splitsize_mean+2))


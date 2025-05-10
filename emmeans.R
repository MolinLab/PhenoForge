#code estimated marginal means
dir.create("plots/emmeans")
path_em<-"plots/emmeans/"

#NDVI

anovasNDVIposthocs_rbound<-do.call("rbind", list_anovas_NDVI_allPosthocs)
Mean_emmean_Groups<-anovasNDVIposthocs_rbound%>%group_by(Date, Condition)%<%
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

#Size


anovasSizeposthocs_rbound<-do.call("rbind", list_anovas_Size_allPosthocs)
Mean_emmean_Groups_Size<-anovasSizeposthocs_rbound%>%group_by(Date, Condition)%>%
  summarise(Mean=mean(emmean), SD=sd(emmean))

plot_emmeans_Size<-ggplot(Mean_emmean_Groups_Size, aes(x=Date, y=Mean, colour=Condition, group=Condition))+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2)+
  ylab("Mean estimated marginal mean (EMM)")+labs(title="Size Anova (emmeans)")+theme_bw()+
  theme(axis.text.x = element_text(angle=75, hjust=1.1), 
        axis.title.x = element_text(hjust=1.05))+
  scale_colour_manual(values = c("Control"="turquoise3", 
                                 "Drought stress"="coral"))

ggsave(plot_emmeans_Size, filename=paste0(path_em, "Size.png"))
#MCARI1


anovasMCARI1eposthocs_rbound<-do.call("rbind", list_anovas_MCARI1_allPosthocs)
Mean_emmean_Groups_MCARI1<-anovasMCARI1eposthocs_rbound%>%group_by(Date, Condition)%>%
  summarise(Mean=mean(emmean), SD=sd(emmean))

plot_emmeans_MCARI1<-ggplot(Mean_emmean_Groups_MCARI1, aes(x=Date, y=Mean, colour=Condition, group=Condition))+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2)+
  ylab("Mean estimated marginal mean (EMM)")+labs(title="MCARI1 Anova (emmeans)")+theme_bw()+
  theme(axis.text.x = element_text(angle=75, hjust=1.1), 
        axis.title.x = element_text(hjust=1.05))+
  scale_colour_manual(values = c("Control"="turquoise3", 
                                 "Drought stress"="coral"))


ggsave(plot_emmeans_MCARI1, filename=paste0(path_em, "MCARI1.png"))

#BBCH emmeans plots


anovasBBCHposthocs_rbound<-do.call("rbind", list_posthocs_BBCH)
Mean_emmean_Groups_BBCH<-anovasBBCHposthocs_rbound%>%group_by(Date, Condition)%>%
  summarise(Mean=mean(emmean), SD=sd(emmean))


plot_emmeans_BBCH<-ggplot(Mean_emmean_Groups_BBCH, aes(x=Date, y=Mean, colour=Condition, group=Condition))+
  geom_point()+geom_line()+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2)+
  ylab("Mean estimated marginal mean (EMM)")+labs(title="BBCH Anova (emmeans)")+theme_bw()+
  theme(axis.text.x = element_text(angle=75, hjust=1.1), 
        axis.title.x = element_text(hjust=1.05))+
  scale_colour_manual(values = c("Control"="turquoise3", 
                                 "Drought stress"="coral"))



ggsave(plot_emmeans_BBCH, filename=paste0(path_em, "BBCH.png"))
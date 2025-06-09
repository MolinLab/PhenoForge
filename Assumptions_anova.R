

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



library(tidyverse)
library(ggplot2)
library(shiny)
library(lme4)
library(multcomp)
library(emmeans)
library(ggpubr)
library(car)
library(rstatix)
library(readxl)

source("functions_updated.R", local = knitr::knit_global() )
source("read_in_paths.R", local = knitr::knit_global() )
source("metatable.R", local = knitr::knit_global() )
source("manualFiles.R",local = knitr::knit_global())
source("datawrangling1.R", local = knitr::knit_global() )
source("addScoreInfo.R",local = knitr::knit_global())
source("bisect_data.R", local = knitr::knit_global() )
source("scores_boxplots.R", local=knitr::knit_global())


# UI
ui <- fluidPage(
  titlePanel("Anova and Posthoc Testing"),
  sidebarLayout(
    sidebarPanel(
      selectInput("date", "Select Date", choices = names(CamData_Bonitur_TG)),
      selectInput("variable", "Select a variable", choices=colnames(CamData_Bonitur_TG[[1]][,5:12]))
    ),
    mainPanel(
     # tableOutput("dataTable"),
      plotOutput("boxPlot"),
      verbatimTextOutput("anovaTable"),
      plotOutput("posthocPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  selectedData <- reactive({
    req(input$date)  # Ensure input$date is available
    data <- CamData_Bonitur_TG[[input$date]]
    data$Condition <- as.factor(data$Condition)#Ensure Condition and Genotype are factors: This is important for the ANOVA analysis.
    data$Genotype <- as.factor(data$Genotype)
    data
    #print(head(data))  # Debugging: Print the first few rows of the selected dataframe
    #data
  })

    
  
  output$anovaTable <- renderPrint({
    data<-selectedData()
    formula<-as.formula(paste(data[input$variable], "~ Condition + Genotype + Condition*Genotype"))
    aov.model<-aov(formula, data=data)
    anova<-Anova(aov.model,type="III")
    anova_part<-anova[,1:4]
   print(anova_part)
  })
  
  
  output$posthocPlot<- renderPlot({
    
   
   # data<-selectedData()
    data<-selectedData()
    formula<-as.formula(paste(data[input$variable], "~ Condition + Genotype + Condition*Genotype"))
    aov.model<-aov(formula, data=data)
    #anova<-Anova(aov.model,type="III")
    emmobject<-emmeans(aov.model, ~Condition*Genotype)
    cld_emmtabel<-cld(emmobject, alpha=.05, Letters=letters)
    
    cld_emmt_filt<-cld_emmtabel %>%dplyr::filter(Condition=="Drought stress")%>%
      group_by(.group) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if(length(unique(cld_emmt_filt$.group))>1){
    
    dat_filt<-cld_emmtabel%>%dplyr::filter(Condition=="Drought stress")
    stat_test<-dat_filt%>%group_by(".group")%>%t_test(emmean~.group)%>%mutate(y.position = max(cld_emmtabel$emmean))
    plot_cld<-ggplot(cld_emmtabel, aes(x=.group, y=emmean, color=Condition))+geom_boxplot()+
      stat_pvalue_manual(stat_test, label = "p", y.position = "y.position", step.increase = .1)+
      scale_colour_manual(values = c("Control"="turquoise3", 
                                     "Drought stress"="coral"))+theme_bw()
  
    plot_cld
    
    } else {
    print ("not enough groups for posthoc test!")
  }
})
  
      
  output$boxPlot <- renderPlot({
    data <- selectedData()
    #if (!is.null(data) && nrow(data) > 0) {  # Check if the dataframe is not empty
      ggplot(data, aes(x=Condition, y=.data[[input$variable]], color=Condition)) +
        geom_boxplot()+
        scale_colour_manual(values = c("Control"="turquoise3", 
                                       "Drought stress"="coral"))+theme_bw()
  
        
   # } else {
    #  print("No data available for the selected date.")  # Debugging: Print a message if the dataframe is empty
    #}
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)


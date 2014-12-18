#Problem 1
#ui
#ben arancibia

require(shiny)

setwd("/users/bcarancibia/CUNY_IS_608/lecture3/Problem_1") #you will need to change this
data <- read.csv("cleaned-cdc-mortality-1999-2010.csv") #load data

#used this as guide: http://rstudio.github.io/shiny/tutorial/#ui-and-server
# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Crude Mortality Rate Across States by Cause of Death"),
  
  sidebarPanel(
    selectInput("cause", "Mortality Cause:",
                choices = names(table(data$ICD.Chapter))),
    submitButton("GO")
    ),
  
  mainPanel(plotOutput("barPlot", height="600px"))
))
  



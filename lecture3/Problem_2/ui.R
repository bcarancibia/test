#Problem 2
#ui
#ben arancibia

require(shiny)

setwd("/users/bcarancibia/CUNY_IS_608/lecture3/Problem_2") #you will need to change this
data <- read.csv("cleaned-cdc-mortality-1999-2010.csv") #load data

#used this as guide: http://rstudio.github.io/shiny/tutorial/#ui-and-server
#used pretty much exact same setup as problem 1
# Define UI 
shinyUI(pageWithSidebar(
  headerPanel("Crude Mortality Rate by State compared to National Average"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput("cause", "Mortality Cause:",
                choices = names(table(data$ICD.Chapter))),
    selectInput("state", "State:",
                choices = names(table(data$State))),
    submitButton("GO")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("barPlot", height="600px")
  )
))
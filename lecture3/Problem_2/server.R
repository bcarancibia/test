#Problem 2
#server
#ben arancibia

library(shiny)
require(dplyr) # like dplyr more, easier to manipulate
library(ggplot2)

setwd("/users/bcarancibia/CUNY_IS_608/lecture3/Problem_2") #You will need to change this
data <- read.csv("cleaned-cdc-mortality-1999-2010.csv") #data

#http://rstudio.github.io/shiny/tutorial/#inputs-and-outputs for the server side of stuff
#reused a lot of the same components from problem 1
#i prefer dplyr over googlevis right now, don't feel comfortable with googlevis

shinyServer(function(input, output) {
  datasetChoice <- reactive({
    filter(data, ICD.Chapter == input$cause, State==input$state)
  })
  national <- reactive({ #national average
    national_choice <- filter(data, ICD.Chapter == input$cause)
    aggregate(national_choice[c("Population", "Deaths")], by=national_choice["Year"], FUN=sum)
  })
  
#summary
output$barPlot <- renderPlot({
  data_filtered <- datasetChoice()
  if (nrow(data_filtered) == 12){ #remove all cases where there is not complete dataset for years
    national_average <- national()
    data_filtered["compare_national"] <- data_filtered$Crude.Rate - ((national_average$Deaths/national_average$Population)) #compare seems wrong do i need to multiply by a factor
      
#viz use pretty much same as problem 1
ggplot(data=data_filtered, aes(x=factor(Year), y=compare_national, fill=Crude.Rate)) + #crude fill, looks better then having so many different things
  #could not figure out how to change the fill color from blue to another color like in problem 1 
  geom_bar(stat="identity", position = position_dodge(width=10)) + 
  ylab("Crude Mortality Rate") + 
  xlab("Year") +
  ggtitle(paste(input$cause, "compared to the national average")) + #need this so people know what they are looking at
  theme_bw()
    }
  })
})
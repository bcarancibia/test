#Problem 1
#server
#ben arancibia

require(shiny)
require(dplyr) # like dplyr more, easier to manipulate
require(ggplot2)

setwd("/users/bcarancibia/CUNY_IS_608/lecture3/Problem_1") #you will need to change this
data <- read.csv("cleaned-cdc-mortality-1999-2010.csv") #data

data <- filter(data, Year==2010) #picked 2010 based on instructions in hw

#http://rstudio.github.io/shiny/tutorial/#inputs-and-outputs for the server side of stuff
#i prefer dplyr over googlevis right now, don't feel comfortable with googlevis

shinyServer(function(input, output) {
  datasetInput <- reactive({
    filter(data, ICD.Chapter == input$cause)
  })
  output$barPlot <- renderPlot({
    sort_this <- datasetInput()
    sorted <- sort_this[order(sort_this$Crude.Rate),]
    sorted$State <- factor(sorted$State, levels=unique(as.character(sorted$State)))
    
    if (length(sorted$State) > 0){
      ggplot(data=sorted, aes(x=State, y=Crude.Rate, fill=Population)) + #i like the fill to see population, lets you see that areas of high population with rates
        #could not figure out how to change the fill color from blue to another color. 
        geom_bar(stat="identity", position = position_dodge(width=10)) + 
        coord_flip() + #need coord flip for state names
        ylab("Crude Mortality Rate Across States by Cause of Death") + 
        ggtitle(input$cause) +
        theme_bw()
    }
  })
})
 

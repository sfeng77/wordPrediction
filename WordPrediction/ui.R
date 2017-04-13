#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict next word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("This shiny application use NLP to predict user input based on input history."),
      textOutput("serverst")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
        textInput(inputId = 'intext', label = "Input text"),
        actionButton("goButton", "Predict"),
        h3(textOutput("predText"))
    )
  )
))

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shiny)



ui <- fluidPage(
  titlePanel("Website Clustering"),
  sidebarLayout(
    textAreaInput("query", "Serach Query", "", width = "500px"),
      actionButton("btn", "search")
    ),
    
    mainPanel(
      
      
      
      verbatimTextOutput("console_text"),
      textOutput("selected_var1")
    )
  )

source("f.R",local=TRUE)

server <- function(input, output) {
  
  
  observeEvent(input$btn, {
    output$console_text <- renderPrint(onerun <- new.function())
    
  })
    
}

shinyApp(ui = ui, server = server)


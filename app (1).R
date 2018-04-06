library(tidyverse)
library(shiny)
library(datasets)
library(ggplot2)
dataset <- diamonds


ui <- fluidPage(
  

  titlePanel("Diamond Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xcol",
                  "X Variable",
                  names(dataset)),
      selectInput("ycol",
                  "Y Variable",
                  names(dataset), names(dataset)[[2]]),
      selectInput('color', 
                  'Color', 
                  c(names(dataset))),
      checkboxInput("type","Add Smoothing Line"),
      textInput("title",
                "Add a Title"),
      actionButton("info", "Click for Dataset Info"),
      uiOutput("HelpBox")
    ),
    
    mainPanel(
      plotOutput("plot1"),
      verbatimTextOutput("r2"))
  )
)

server <- function(input, output, session) {
  output$HelpBox <- renderUI({
    if (input$info %% 2){
      helpText("The dataset contains the prices and other attributes of almost 54,000 diamonds.")
    } else {
      return()
    }
  })
  
  output$plot1 <- renderPlot({
    p <- ggplot(data = dataset, aes_string(x = input$xcol,y = input$ycol,color=input$color)) + geom_point() + ggtitle(input$title)
   if (input$type == "TRUE")
   p <- p + geom_smooth()
    print(p)
  })
  
  output$r2 <- renderPrint({
    lm((diamonds[[input$ycol]])~(diamonds[[input$xcol]]))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Body-Mass Index calculator"),
  
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("course", "Course:", 
                  choices=unique(as.character(tenormore$Course))),
      hr()
    ),
    
    mainPanel(
      mainPanel(textOutput("text")),
      plotOutput("coursePlot")
    )
  )
))
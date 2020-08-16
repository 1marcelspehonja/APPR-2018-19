library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Slovenske občine"),
  
   tabsetPanel(
       tabPanel("Rodnost",
                DT::dataTableOutput("rodnost"))
  #     
  #     tabPanel("Število naselij",
  #              sidebarPanel(
  #                 uiOutput("pokrajine")
  #               ),
  #              mainPanel(plotOutput("naselja")))
     )
))

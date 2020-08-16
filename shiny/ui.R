library(shiny)

# shinyUI(fluidPage(
#   
#   titlePanel("Slovenske občine"),
#   
#    tabsetPanel(
#        tabPanel("Rodnost",
#                 DT::dataTableOutput("rodnost"))
  #     
  #     tabPanel("Število naselij",
  #              sidebarPanel(
  #                 uiOutput("pokrajine")
  #               ),
  #              mainPanel(plotOutput("naselja")))
#      )
# ))


fluidPage(
  tabPanel("Zemljevid",
           sidebarPanel(
             selectInput("letnica", label = "Leto", 
                         choices = unique(rodnost.vse$Leto)), width=2),
           mainPanel(plotOutput("zemljevid1"), width=10))
)
library(shiny)

# shinyUI(
#   fluidPage(
# 
#     titlePanel("Rodnost drzav sveta"),
# 
#     tabsetPanel(
#        tabPanel(DT::dataTableOutput("rodnost"))
#       )
#   )
# )
  
fluidPage(
  tabPanel("Zemljevid",
           sidebarPanel(
             selectInput("letnica", label = "Leto", 
                         choices = unique(rodnost.vse$Leto)), width=2),
           mainPanel(plotOutput("zemljevid1"), width=10))
)

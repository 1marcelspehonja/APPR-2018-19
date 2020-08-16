library(shiny)
library(dplyr)
library(ggplot2)

# shinyServer(function(input, output) {
#   output$rodnost <- DT::renderDataTable({
#     zdruzeni.podatki %>% spread(key="Leto", value="Rodnost") %>%
#       rename(`Država`=Drzava)
#   })
  
  # output$pokrajine <- renderUI(
  #   selectInput("pokrajina", label="Izberi pokrajino",
  #               choices=c("Vse", levels(obcine$pokrajina)))
  # )
  # output$naselja <- renderPlot({
  #   main <- "Pogostost števila naselij"
  #   if (!is.null(input$pokrajina) && input$pokrajina %in% levels(obcine$pokrajina)) {
  #     t <- obcine %>% filter(pokrajina == input$pokrajina)
  #     main <- paste(main, "v regiji", input$pokrajina)
  #   } else {
  #     t <- obcine
  #   }
  #   ggplot(t, aes(x=naselja)) + geom_histogram() +
  #     ggtitle(main) + xlab("Število naselij") + ylab("Število občin")
  # })
# })

function(input, output) 
{
  
  output$zemljevid1 <- renderPlot({
    ggplot() +
      geom_polygon(data=left_join(zemljevid, rodnost.vse %>% filter(Leto == input$letnica), by=c("NAME"="Drzava")),
                   aes(x=long, y=lat, group=group, fill=Rodnost), color = "black") + 
      coord_cartesian(xlim=c(-150, 170), ylim=c(-50, 80)) +
      scale_fill_gradient2(low ="yellow", mid = "red", high = "blue", 
                           na.value = "white", name="Rodnost",
                           guide = "legend",
                           midpoint=25) +
      xlab("") + ylab("") + ggtitle("Rodnost po državah sveta") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
}

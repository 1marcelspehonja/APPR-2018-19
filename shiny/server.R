library(shiny)
library(dplyr)
library(ggplot2)

# shinyServer(function(input, output) {
#   output$rodnost <- DT::renderDataTable({
#     rodnost.vse %>% mutate_at(3, funs(round(.,1))) %>% 
#       filter(Leto=="2000"|Leto=="2009"|Leto=="2018") %>% 
#       spread(Leto, Rodnost)
#   })
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

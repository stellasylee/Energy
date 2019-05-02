#We referred to the following webpages while working on this project:
#https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da

library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("U.S. Energy Production and Consumption"),
  
  sidebarLayout(
    sidebarPanel({
      selectInput(inputId = "prod_or_cons",
                  label = "Please choose an option:",
                  choices = c("Production", "Consumption"))
    })
  ),
  
  mainPanel({
    #Map would go here
    leafletOutput(outputId = "map")
  })
  
)

server <- function(input, output) {
  
  
}

shinyApp(ui = ui, server = server)
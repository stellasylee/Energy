#We referred to the following webpages while working on this project:
#https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
#https://rstudio.github.io/leaflet/popups.html
#https://appsilon.com/how-to-use-viridis-colors-with-plotly-and-leaflet/
#https://stackoverflow.com/questions/47824893/coloring-continuous-data-in-leaflet-r-does-not-work
#https://stackoverflow.com/questions/45776232/define-palette-breaks-for-leaflet
#http://www.r-tutor.com/elementary-statistics/numerical-measures/quartile
#http://www.r-tutor.com/elementary-statistics/numerical-measures/percentile
#http://rprogramming.net/recode-data-in-r/
#https://stackoverflow.com/questions/54283852/leaflet-colorquantile-breaks-are-not-unique
#https://stackoverflow.com/questions/54913505/invalid-type-list-of-argument-in-r
#https://stackoverflow.com/questions/34134310/sum-of-returned-list-error-invalid-type-list-of-argument
#Some of the map code is copied pretty closely from these sites.

library(shiny)
library(leaflet)
library(viridis)

ui <- fluidPage(
  titlePanel("U.S. Energy Production and Consumption"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "prod_or_cons",
                  label = "Please choose an option:",
                  choices = c("Production", "Consumption")),
      
      selectInput(inputId = "which_year",
                  label = "Please choose a year:",
                  choices = c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
                              "1990","1991","1992","1993","1994","1995","1996","1997","1998","1999",
                              "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
                              "2010","2011","2012","2013","2014","2015","2016"))
    ),
    mainPanel({
      #Map would go here
      leafletOutput(outputId = "map")
    })
  )
)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    if(input$prod_or_cons == "Production") {
      
      #opacity
      op <- ifelse(is.na(tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,]), .1, 1)
      
      production_label <- sprintf(
        "<strong>%s</strong><br/>Production: %g BTUs",
        (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$country, (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$prod
      ) %>% lapply(htmltools::HTML)
      
      leaflet(tidy_combined_production_table) %>% setView(lng = 0, lat = 0, zoom = 2) %>% addTiles() %>% addCircles(data = tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,], lat = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$Latitude, lng = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$Longitude, weight = 1, radius = 100000, color = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$circle_color, label = production_label, opacity = op) %>% addLegend(colors = circle_colors, labels = c("Category 1 (least production)", "2", "3", "4", "5", "6", "7", "8 (greatest production)"), values = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$circle_color, opacity = 0.7, title = "Production (in BTUs)", position = "bottomright")
  
    } else if(input$prod_or_cons == "Consumption") {
      
      #opacity
      op <- ifelse(is.na(combined_consumption_table[,input$which_year]), .1, 1)
      
      consumption_label <- sprintf(
        "<strong>%s</strong><br/>Consumption: %g BTUs",
        (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$country, (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$cons
      ) %>% lapply(htmltools::HTML)
      
      leaflet(tidy_combined_consumption_table) %>% setView(lng = 0, lat = 0, zoom = 2) %>% addTiles() %>% addCircles(data = tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,], lat = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$Latitude, lng = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$Longitude, weight = 1, radius = 100000, color = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$circle_color, label = consumption_label, opacity = op) %>% addLegend(colors = circle_colors, labels = c("Category 1 (least consumption)", "2", "3", "4", "5", "6", "7", "8 (greatest consumption)"), values = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$circle_color, opacity = 0.7, title = "Consumption (in BTUs)", position = "bottomright")
    }
  })
  
}

shinyApp(ui = ui, server = server)
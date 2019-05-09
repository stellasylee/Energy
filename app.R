#We referred to the following webpages while working on this project:
#https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
#https://rstudio.github.io/leaflet/popups.html
#https://appsilon.com/how-to-use-viridis-colors-with-plotly-and-leaflet/
#Some of the map code is copied pretty closely from these sites.

library(shiny)
library(leaflet)

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
  
  color_scale_prod <- colorNumeric(palette = c('purple', 'blue', 'green', 'yellow', 'orange', 'red'), domain = c(0, 1e+18))
  
  color_scale_cons <- colorNumeric(palette = c('purple', 'blue', 'green', 'yellow', 'orange', 'red'), domain = c(0, 1e+18))
  
  output$map <- renderLeaflet({
    
    if(input$prod_or_cons == "Production") {
      
      #selected_year_only <- 
      
      #selected_year_without_NA_values <- combined_production_table[,input$which_year]
      #selected_year_without_NA_values <- selected_year_without_NA_values[!is.na(selected_year_without_NA_values)]
      
      #opacity
      op <- ifelse(is.na(combined_production_table[,input$which_year]), .1, 1)
      
      #production_label <- paste(sep = "<br/>",
      #                          paste("Country:", combined_production_table$country),
      #                          paste("Production:", combined_production_table[,input$which_year]))
      
      production_label <- sprintf(
        "<strong>%s</strong><br/>Production: %g",
        combined_production_table$country, combined_production_table[,input$which_year]
      ) %>% lapply(htmltools::HTML)
      
      leaflet(combined_production_table) %>% setView(lng = 0, lat = 0, zoom = 2) %>% addTiles() %>% addCircles(data = combined_production_table, lat = combined_production_table$Latitude, lng = combined_production_table$Longitude, weight = 1, radius = 100000, color = color_scale_prod(combined_production_table[,input$which_year]), label = production_label, opacity = op) %>% addLegend(pal = color_scale_prod, values = combined_production_table[,input$which_year], opacity = 0.7, title = "Legend", position = "bottomright")  #, highlightOptions = highlightOptions(weight = 5,
                                                                                                                                                                                                                                                                                                                                                                                   #                                                   color = "#666",
                                                                                                                                                                                                                                                                                                                                                                                    #                                                  dashArray = "",
                                                                                                                                                                                                                                                                                                                                                                                     #                                                 fillOpacity = 0.7,
                                                                                                                                                                                                                                                                                                                                                                                      #                                                bringToFront = TRUE)) # %>% addCircles(combined_production_table$Longitude, combined_production_table$Latitude, production_label,      # %>% addLegend()
    } else if(input$prod_or_cons == "Consumption") {
      
      #opacity
      op <- ifelse(is.na(combined_consumption_table[,input$which_year]), .1, 1)
      
      #consumption_label <- paste(sep = "<br/>",
       #                          paste("Country:", combined_consumption_table$country),
        #                         paste("Consumption:", combined_consumption_table[,input$which_year]))
      
      consumption_label <- sprintf(
        "<strong>%s</strong><br/>Consumption: %g",
        combined_consumption_table$country, combined_consumption_table[,input$which_year]
      ) %>% lapply(htmltools::HTML)
      
      #leaflet(combined_consumption_table) %>% setView(lng = 0, lat = 0, zoom = 2) %>% addTiles() %>% addCircles(data = combined_consumption_table, lat = combined_consumption_table$Latitude, lng = combined_consumption_table$Longitude, weight = 1, radius = 100000, color = color_scale_cons(combined_consumption_table[,input$which_year]), popup = ~as.character(combined_consumption_table[,input$which_year]), label = paste("Country: ", combined_consumption_table$country, "\n\n", "Consumption: ", combined_consumption_table[,input$which_year]), opacity = op)
      
      leaflet(combined_consumption_table) %>% setView(lng = 0, lat = 0, zoom = 2) %>% addTiles() %>% addCircles(data = combined_consumption_table, lat = combined_consumption_table$Latitude, lng = combined_consumption_table$Longitude, weight = 1, radius = 100000, color = color_scale_cons(combined_consumption_table[,input$which_year]), label = consumption_label, opacity = op)
    }
  })
  
}

shinyApp(ui = ui, server = server)
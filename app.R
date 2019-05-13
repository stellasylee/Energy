library(shiny)
library(plotly)
library(dplyr)
library(readr)  
library(leaflet)
library(rvest)
library(stringr)
library(readr)
library(ggplot2)
library(timeSeries)
library(ggfortify)
library(reshape2)
library(forecast)
library(viridis)

# Source helper functions -----
source("source.R")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#
# Define UI----
ui <- navbarPage (inverse= FALSE, "International Primary Energy Consumption and Production",
                  # First Page - Intro
                  tabPanel("Intro",
                           fluidPage(h1("project title?"),
                                     br(),
                                     p("project description"),
                                     br(),
                                     div(p(strong("Built by"),  "Stella Lee, Patrick Min, and Nate Lu"), 
                                         p(strong("R Packages:"), "<need to be fixed>"),
                                         p(strong("Data Sources:"), a("U.S. Environmental Information Administration (eia)"), href = "https://www.eia.gov/beta/international/data/browser/#/?c=4100000002000060000000000000g000200000000000000001&vs=INTL.44-1-AFRC-QBTU.A&vo=0&v=H&end=2016"),
                                         p("See", a("Our GitHub Repository", href = "https://github.com/stellasylee/Energy"), "for more information")
                                     ))),
                  # Second Page - Map
                  tabPanel("Map",
                           fluidPage(
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
                             ))),
                  # Third Page - Line Chart
                  tabPanel("Line Plot",
                           fluidPage(titlePanel("Consumption and Production of Each Country"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "country", label = strong("Country:"),
                                                     choices = unique(cleanedPro$country),
                                                     selected = "Afghanistan"),
                                         checkboxInput(inputId = "ppp", # per capita scale
                                                       label = "Show PPP (Purchasing Power Parity)",
                                                       value = FALSE),
                                         checkboxInput(inputId = "perCapita", # per capita scale
                                                       label = "Show per capita scale",
                                                       value = FALSE),
                                         checkboxInput(inputId = "worldAvg", # world average line
                                                       label = "Show world production and consumption per capita ",
                                                       value = FALSE),
                                         checkboxInput(inputId = "log", 
                                                       label = "Show log scale ", # log scale
                                                       value = FALSE)
                                       ),
                                       mainPanel(
                                         plotlyOutput(outputId = "timeseries", height = "600px")
                                       )
                                     ))),
                  # Fourth Page - Timeseries Prediction
                  tabPanel("Timeseries Prediction",
                           fluidPage(titlePanel("Prediction of Each Country"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "predProCons",
                                                     label = "Please choose an option:",
                                                     choices = c("Production", "Consumption")),
                                         selectInput(inputId = "predCountry", label = strong("Country:"),
                                                     choices = unique(production$country),
                                                     selected = "Afghanistan")
                                       ),
                                       mainPanel(
                                         plotOutput(outputId = "prediction", height = "600px")
                                       )
                                     )))
)

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#
# Define server logic ----
server <- function(input, output) {
  # Map ----
  output$map <- renderLeaflet({
    if(input$prod_or_cons == "Production") {
      #opacity
      op <- ifelse(is.na(tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,]), .1, 1)
      production_label <- sprintf(
        "<strong>%s</strong><br/>Production: %g BTUs",
        (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$country, (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$prod
      ) %>% lapply(htmltools::HTML)
      leaflet(tidy_combined_production_table) %>% 
        setView(lng = 0, lat = 0, zoom = 2) %>% 
        addTiles() %>% 
        addCircles(data = tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,],
                   lat = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$Latitude, 
                   lng = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$Longitude, weight = 1, 
                   radius = 100000, 
                   color = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$circle_color, 
                   label = production_label, opacity = op) %>% 
        addLegend(colors = circle_colors, 
                  labels = c("Category 1 (least production)", "2", "3", "4", "5", "6", "7", "8 (greatest production)"), 
                  values = (tidy_combined_production_table[tidy_combined_production_table$year == input$which_year,])$circle_color, opacity = 0.7, 
                  title = "Production (in BTUs)", position = "bottomright")
    } else if(input$prod_or_cons == "Consumption") {
      #opacity
      op <- ifelse(is.na(combined_consumption_table[,input$which_year]), .1, 1)
      consumption_label <- sprintf(
        "<strong>%s</strong><br/>Consumption: %g BTUs",
        (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$country, (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$cons
      ) %>% lapply(htmltools::HTML)
      leaflet(tidy_combined_consumption_table) %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addTiles() %>%
        addCircles(data = tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,],
                   lat = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$Latitude,
                   lng = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$Longitude,
                   weight = 1, radius = 100000,
                   color = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$circle_color,
                   label = consumption_label, opacity = op) %>% 
        addLegend(colors = circle_colors, 
                  labels = c("Category 1 (least consumption)", "2", "3", "4", "5", "6", "7", "8 (greatest consumption)"), 
                  values = (tidy_combined_consumption_table[tidy_combined_consumption_table$year == input$which_year,])$circle_color, 
                  opacity = 0.7, title = "Consumption (in BTUs)", position = "bottomright")
    }
  })
  
  # Line Chart Consumption and Production of Each Country ----
  output$timeseries <- renderPlotly({
    # Special Case: Germany reunification at 1990
    if (paste(input$country) == "Germany"){
      temp <- cbind (as.data.frame(t(cleanedPop [which(cleanedCon$country == "Germany"),-1])),
                     as.data.frame(t(production [which(production$country == "Germany, East"), -1])),
                     as.data.frame(t(production [which(production$country == "Germany, West"), -1])),
                     as.data.frame(t(cleanedPro [which(cleanedPro$country == "Germany"),-1])), 
                     as.data.frame(t(consumption [which(consumption$country == "Germany, East"), -1])),
                     as.data.frame(t(consumption [which(consumption$country == "Germany, West"), -1])),
                     as.data.frame(t(cleanedCon [which(cleanedCon$country == "Germany"),-1])),
                     world)
      names(temp) <- c("population", "proEast", "proWest", "production",
                       "conEast", "conWest", "consumption",  "worldPro", "worldCon")
    } else {
      # Create data frame for plot
      temp <- cbind(as.data.frame(t(cleanedPop [which(cleanedPop$country == paste(input$country)),-1])),
                    as.data.frame(t(cleanedPro [which(cleanedPro$country == paste(input$country)),-1])),
                    as.data.frame(t(cleanedCon [which(cleanedCon$country == paste(input$country)),-1])),
                    world)
      names(temp) <- c("population", "production", "consumption", "worldPro", "worldCon")
    }
    
    # per capita data
    temp <- mutate(temp, proPerCapita = production / population) %>%
      mutate(., conPerCapita = consumption / population)
    
    if (input$log){
      temp <- log (temp[,-1])
    }
    
    p <- plot_ly(temp, x = ~colsn) %>%
      layout(title = paste(input$country),
             xaxis = list(title = "Year"),
             yaxis = list (title = "Primary Energy (Giga BTU)"))
    
    if (input$perCapita){
      p <- p %>%
        add_trace(y = ~proPerCapita, name = 'production', mode = 'lines+markers', type = 'scatter') %>%
        add_trace(y = ~conPerCapita, name = 'consumption', mode = 'lines+markers', type = 'scatter')
    } else{
      p <- p %>%
        add_trace(y = ~production, name = 'production', mode = 'lines+markers', type = 'scatter') %>%
        add_trace(y = ~consumption, name = 'consumption', mode = 'lines+markers', type = 'scatter')
    }
    
    if ((paste(input$country) == "Germany") && (!input$perCapita)){
      p <- p %>%
        add_trace(y = ~proEast, name = 'production (East)', mode = 'lines+markers', type = 'scatter') %>%
        add_trace(y = ~proWest, name = 'production (West)', mode = 'lines+markers', type = 'scatter') %>%
        add_trace(y = ~conEast, name = 'consumption (East)', mode = 'lines+markers', type = 'scatter') %>%
        add_trace(y = ~conWest, name = 'consumption (West)', mode = 'lines+markers', type = 'scatter')
    }
    
    if (input$worldAvg && input$perCapita) {
      p <- p %>%
        add_trace(y = ~worldPro, name = 'world production', mode = 'lines+markers', type = 'scatter') %>%
        add_trace(y = ~worldCon, name = 'world consumption', mode = 'lines+markers', type = 'scatter')
    } 
    p
  })
  
  # Page 4 ----
  output$prediction <- renderPlot({
    if (input$predProCons == "Production"){
      data <- prod1.5[prod1.5$country == paste(input$predCountry), ]$value
      M2 = auto.arima(ts(data, frequency = 10), D=1)
      M2F = forecast(M2)
      plot(M2F,main="ARIMA Forecast")
    }else {
      
    }
    
   
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

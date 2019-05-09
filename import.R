#We referred to the following webpages while working on this project:
#https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
#https://rstudio.github.io/leaflet/popups.html
#https://appsilon.com/how-to-use-viridis-colors-with-plotly-and-leaflet/
#Some of the map code is copied pretty closely from these sites.
#https://plot.ly/r/shiny-coupled-hover-events/

library(shiny)
library(plotly)
library(dplyr)
library(readr)  
library(leaflet)
library(rvest)
library(stringr)
library(readr)
library(ggplot2)

#-------------------------------------------------------------------------------------------------------------------#
#                                         DATA WRANGLING & CLEANING                                                 #
#-------------------------------------------------------------------------------------------------------------------#

cols <- c ("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
           "1990","1991","1992","1993","1994","1995","1996","1997","1998","1999",
           "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
           "2010","2011","2012","2013","2014","2015","2016")
colsn <- (1980:2016)

# This function is used for dropping unneccesary column and set the column names----
set <- function(data){
  data <- data [-c(1,3)]
  names(data) <- c("country",
                   "1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
                   "1990","1991","1992","1993","1994","1995","1996","1997","1998","1999",
                   "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
                   "2010","2011","2012","2013","2014","2015","2016")
  
  for (col in cols){
    data[,col] <- as.numeric(as.character(data[,col]))
    data[,col] <- (10^15 * data[,col])
  }
  data
}

# Data----
# Dataframe for production of primary energy by country from 1980 to 2016 (unit: quad btu)
production <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/International_data_energy.csv", 
                       skip = 8, nrows = 228, head = FALSE) %>%
  set(.)

# Dataframe for consumption of primary energy by country from 1980 to 2016 (unit: quad btu)
consumption <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/International_data_energy.csv",
                        skip = 238, head = FALSE) %>%
  set(.)

# Page 2----
country_coordinates <- read_html("https://developers.google.com/public-data/docs/canonical/countries_csv")
tr_nodes <- html_nodes(country_coordinates, "tr")
string_of_nodes <- html_text(tr_nodes)
string_of_nodes <- str_squish(string_of_nodes)
string_of_nodes <- string_of_nodes[-1] #Get rid of labels
drop_abbreviations <- str_sub(string_of_nodes, start = 4)
drop_abbreviations <- drop_abbreviations[-227] #U.S. Minor Outlying Islands - doesn't have coordinates
latitudes <- str_replace_all(drop_abbreviations, c(" .*$" = ""))
longitudes <- str_replace_all(drop_abbreviations, c("^[^ ]* " = ""))
country_names <- str_replace_all(longitudes, c("^[^ ]* " = ""))
longitudes <- str_replace_all(longitudes, c(" .*$" = ""))

coordinates <- data.frame(Country = country_names, Latitude = latitudes, Longitude = longitudes, stringsAsFactors = FALSE)
new_coordinate_table <- coordinates #Keep old coordinates table as backup
#Rename countries in coordinate table to match names in production/consumption tables
new_coordinate_table$Country[30] <- "Bahamas, The"
new_coordinate_table$Country[143] <- "Burma (Myanmar)"
new_coordinate_table$Country[38] <- "Congo (Kinshasa)"
new_coordinate_table$Country[40] <- "Congo (Brazzaville)"
new_coordinate_table$Country[42] <- "Cote dIvoire (IvoryCoast)"
new_coordinate_table$Country[69] <- "Falkland Islands (Islas Malvinas)"
new_coordinate_table$Country[82] <- "Gambia, The"
new_coordinate_table$Country[120] <- "Korea, South"
new_coordinate_table$Country[119] <- "Korea, North"
new_coordinate_table$Country[141] <- "Macedonia"
new_coordinate_table$Country[185] <- "Reunion"
new_coordinate_table$Country[231] <- "Saint Vincent/Grenadines"
new_coordinate_table$Country[205] <- "Sao Tome and Principe"
new_coordinate_table$Country[216] <- "Timor-Leste (East Timor)"
new_coordinate_table$Country[234] <- "Virgin Islands, U.S."
new_coordinate_table$Country[233] <- "Virgin Islands, British"

#Remove countries from the coordinate table which aren't in the production/consumption tables
new_coordinate_table <- new_coordinate_table[-c(182, 1, 5, 32, 104, 51, 37, 211, 92, 78, 94, 222, 230, 237, 102, 109, 127, 140, 241, 70, 136, 160, 178, 201, 87, 198, 215),]
new_production_table <- production #Keep old production table as backup
#Remove countries from the production table which aren't in the coordinate table
new_production_table <- new_production_table[-c(68, 69, 70, 71, 79, 80, 93, 189, 210, 211, 224),]
#Similarly, we want to do the same thing with the consumption table
new_consumption_table <- consumption
#Remove countries from the consumption table which aren't in the coordinate table
new_consumption_table <- new_consumption_table[-c(68, 69, 70, 71, 79, 80, 93, 189, 210, 211, 224),]
#Rename the "Country" column to "country" in the coordinate table to match the column name in the production/consumption tables before we do the join
colnames(new_coordinate_table) <- c("country", "Latitude", "Longitude")
#Join the coordinates into the production table
combined_production_table <- left_join(x = new_production_table, y = new_coordinate_table, by = "country")
#Make the latitude and longitude numerics
combined_production_table$Latitude <- as.numeric(combined_production_table$Latitude)
combined_production_table$Longitude <- as.numeric(combined_production_table$Longitude)
#Join the coordinates into the consumption table
combined_consumption_table <- left_join(x = new_consumption_table, y = new_coordinate_table, by = "country")
#Make the latitude and longitude numerics
combined_consumption_table$Latitude <- as.numeric(combined_consumption_table$Latitude)
combined_consumption_table$Longitude <- as.numeric(combined_consumption_table$Longitude)

# Page 3 ----
# Population data from 1980 to 2016
population <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/worldPopulation.csv")
population <- population [,-(1:2)] %>% .[,-2]
names (population) <- c("country", cols)
# Modify country names to match with production and consumption data
population$country <- as.character(population$country)
population$country[33] <- "Cape Verde"
population$country[136] <- "Burma (Myanmar)"
population$country[45] <- "Congo (Kinshasa)"
population$country[46] <- "Congo (Brazzaville)"
population$country[59] <- "Egypt"
population$country[87] <- "Hong Kong"
population$country[104] <- "Korea, North"
population$country[105] <- "Korea, South"
population$country[109] <- "Laos"
population$country[118] <- "Macau"
population$country[146] <- "Macedonia"
population$country[140] <- "Netherlands"
population$country[162] <- "Russia"
population$country[64] <- "Swaziland"
population$country[213] <- "Virgin Islands, U.S."
population$country[28] <- "Virgin Islands, British"
population$country[215] <- "Yemen"

# Join the data (intersection)
# Combine Germany East, West to Germany
for (i in (1980:1990)){
  production[78,paste(i)] <- production[79,paste(i)] + production[80,paste(i)]
  consumption[78,paste(i)] <- consumption[79,paste(i)] + consumption[80,paste(i)] 
}

cleanedPro <- filter(production, !(production$country %in% anti_join(x = production, y = population, by = "country")$country))
cleanedCon <- filter(consumption, !(consumption$country %in% anti_join(x = consumption, y = population, by = "country")$country))
cleanedPop <- filter(population, !(population$country %in% anti_join(x = population, y = production, by = "country")$country))
for (i in 2:38){
  cleanedPop[,i] <- parse_number(as.character(cleanedPop[,i]))
}

# World Average
world <- cbind(x = as.data.frame(colSums(cleanedPro[,-1], na.rm = TRUE)), # production
               y = as.data.frame(colSums(cleanedCon[,-1], na.rm = TRUE))) # consumption
worldPop <- as.data.frame(colSums(cleanedPop[,-1], na.rm = TRUE))
for (i in (1:37)){
  world[i,1] <- as.numeric(world[i,1]) / as.numeric(worldPop[i,1]) # world avg production
  world[i,2] <- as.numeric(world[i,2]) / as.numeric(worldPop[i,1]) # world avg consumption
}
names(world)<- c("world pro", "world con")

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
                                     )))
)

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#
# Define server logic ----
server <- function(input, output){
  # Page 2 ----
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
  
  # Page 3 ----
  # Line Chart Consumption and Production of Each Country
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
}

# Run the app ----
shinyApp(ui = ui, server = server)

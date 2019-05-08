#We referred to the following webpages while working on this project:
#https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
#https://plot.ly/r/shiny-coupled-hover-events/
  
library(shiny)
library(plotly)
library(dplyr)
library(readr)  

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

# Add Germany East, West to Germany
for (i in (1980:1990)){
  production[78,paste(i)] <- production[79,paste(i)] + production[80,paste(i)]
  consumption[78,paste(i)] <- consumption[79,paste(i)] + consumption[80,paste(i)] 
}

# Join the data (intersection)
cleanedPro <- filter(production, !(production$country %in% anti_join(x = production, y = population, by = "country")$country))
cleanedCon <- filter(consumption, !(consumption$country %in% anti_join(x = consumption, y = population, by = "country")$country))
cleanedPop <- filter(population, !(population$country %in% anti_join(x = population, y = production, by = "country")$country))
for (i in 2:38){
  cleanedPop[,i] <- parse_number(as.character(cleanedPop[,i])) # Factor to character, then number
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
                           fluidPage(titlePanel("put world map"),
                                     sidebarLayout(
                                       sidebarPanel(
                                       ),
                                       mainPanel(
                                         #plotlyOutput(outputId = "timeseries", height = "600px")
                                       )
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
    
    # Log scale option
    if (input$log){
      temp <- log (temp[,-1])
    }
    
    # Create plot
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

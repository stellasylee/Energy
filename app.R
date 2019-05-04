#We referred to the following webpages while working on this project:
#https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da

#https://plot.ly/r/shiny-coupled-hover-events/
  
library(shiny)
library(plotly)
library(dplyr)

#-------------------------------------------------------------------------------------------------------------------#
#                                         DATA WRANGLING & CLEANING                                                 #
#-------------------------------------------------------------------------------------------------------------------#
# This function is used for dropping unneccesary column and set the column names

cols <- c ("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
           "1990","1991","1992","1993","1994","1995","1996","1997","1998","1999",
           "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
           "2010","2011","2012","2013","2014","2015","2016")

colsn <- c (1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989,
            1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
            2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
            2010, 2011, 2012, 2013, 2014, 2015, 2016)

# This function is used for dropping unneccesary column and set the column names
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

# Dataframe for production of primary energy by country from 1980 to 2016 (unit: quad btu -> giga btu)
production <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/International_data_energy.csv", 
                       skip = 8, nrows = 228, head = FALSE) %>%
  set(.)

# Dataframe for consumption of primary energy by country from 1980 to 2016 (unit: quad btu -> giga btu)
consumption <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/International_data_energy.csv",
                        skip = 238, head = FALSE) %>%
  set(.)

# World Mean Production and Consumption
world <- cbind(x = as.data.frame(colMeans(production[,-1], na.rm = TRUE)),
               y = as.data.frame(colMeans(consumption[,-1], na.rm = TRUE)))
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
                                        selectInput(inputId = "country", label = strong("Country"),
                                                    choices = unique(production$country),
                                                    selected = "Afghanistan"),
                                        checkboxInput(inputId = "worldAvg", 
                                                      label = "Show World Average ",
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
    pro <- production [which(production$country == paste(input$country)),-1]
    con <- consumption [which(consumption$country == paste(input$country)),-1]
    temp <- cbind(x = as.data.frame(t(pro)), y = as.data.frame(t(con)), z=world)
    names(temp) <- c("production", "consumption", "worldPro", "worldCon")
  
    p <- plot_ly(temp, x = ~colsn) %>%
      add_trace(y = ~production, name = 'production', mode = 'lines+markers', type = 'scatter') %>%
      add_trace(y = ~consumption, name = 'consumption', mode = 'lines+markers', type = 'scatter') %>%
      layout(title = paste(input$country),
             xaxis = list(title = "Year"),
             yaxis = list (title = "Primary Energy (Giga BTU)"))
   
    if (input$worldAvg) {
      p <- p %>%
        add_trace(y = ~worldPro, name = 'world production', mode = 'lines+markers', type = 'scatter') %>%
        add_trace(y = ~worldCon, name = 'world consumption', mode = 'lines+markers', type = 'scatter')
    } 
    p
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

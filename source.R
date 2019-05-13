library(dplyr)
library(rvest)
library(stringr)
library(readr)
library(ggplot2)
library(tidyr)
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

# Map data (page 2) ----
country_coordinates <- read_html("https://developers.google.com/public-data/docs/canonical/countries_csv")
tr_nodes <- html_nodes(country_coordinates, "tr")
string_of_nodes <- html_text(tr_nodes) %>% str_squish(.)
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
new_coordinate_table$Country[234] <- "Virgin Islands,  U.S."
new_coordinate_table$Country[233] <- "Virgin Islands, British"

#Remove countries from the coordinate table which aren't in the production/consumption tables
new_coordinate_table <- new_coordinate_table[-c(182, 1, 5, 32, 104, 51, 37, 211, 92, 78, 94, 222, 230, 237, 102, 109, 127, 140, 241, 70, 136, 160, 178, 201, 87, 198, 215),]
#Remove countries from the production table which aren't in the coordinate table #Keep old production table as backup
new_production_table <- production[-c(68, 69, 70, 71, 79, 80, 93, 189, 210, 211, 224),]
#Similarly, we want to do the same thing with the consumption table
#Remove countries from the consumption table which aren't in the coordinate table
new_consumption_table <- consumption[-c(68, 69, 70, 71, 79, 80, 93, 189, 210, 211, 224),]
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

#Check what the maximum value is for production so we can set the range for the color scale for circles on the map
mx = -1
#for(i in 2:ncol(production)) {
#  for (k in 1:228) {
#    if (as.numeric(production[k,i]) > mx) {
#      mx = as.numeric(production[k,i])
#    }
#  }
#}
mx <- max(production[,-1],na.rm=T) 
cat("The maximum is", mx, "\n")

#Check what the maximum value is for consumption so we can set the range for the color scale for circles on the map
#mx = -1
#for(i in 2:ncol(consumption)) {
#  for(k in 1:228) {
#    if(as.numeric(consumption[i,k]) > mx) {
#      mx = as.numeric(consumption[i,k])
#    }
#  }
#}

mx <- max(consumption[,-1],na.rm=T) 
cat("The maximum is", mx, "\n")

#Tidying production table so we can add column for color range
tidy_combined_production_table <- gather(data = combined_production_table, key = year, value = prod, 2:38)
#Creating recode column based on range of production
#Create the column
tidy_combined_production_table$prod_range <- NA
#Do the recode
for(i in 1:length(tidy_combined_production_table$prod_range)) {
  if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 23541600000) {
    tidy_combined_production_table$prod_range[i] <- 1
  } else if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 1.57e+12) {
    tidy_combined_production_table$prod_range[i] <- 2
  } else if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 1.14e+13) {
    tidy_combined_production_table$prod_range[i] <- 3
  } else if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 7.25e+13) {
    tidy_combined_production_table$prod_range[i] <- 4
  } else if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 2.696e+14) {
    tidy_combined_production_table$prod_range[i] <- 5
  } else if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 8.38e+14) {
    tidy_combined_production_table$prod_range[i] <- 6
  } else if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 3.32e+15) {
    tidy_combined_production_table$prod_range[i] <- 7
  } else if(!is.na(tidy_combined_production_table$prod[i]) && tidy_combined_production_table$prod[i] < 1e+18) {
    tidy_combined_production_table$prod_range[i] <- 8
  } else if(is.na(tidy_combined_production_table$prod[i])) {
    tidy_combined_production_table$prod_range[i] <- NA
  }
}

#List of colors (1 through 8, for each category)
circle_colors <- c("purple", "blue", "green", "pink", "yellow", "orange", "red", "black") #purple corresponds to 0, blue corresponds to 1, green corresponds to 2, and so on

#Add a column for the color to the table
tidy_combined_production_table$circle_color <- NA

for(i in 1:length(tidy_combined_production_table$circle_color)) {
  tidy_combined_production_table$circle_color[i] <- circle_colors[tidy_combined_production_table$prod_range[i]]
}

#Now do the same thing for consumption
#Create tidy consumption table
tidy_combined_consumption_table <- gather(data = combined_consumption_table, key = year, value = cons, 2:38)
#Add a column for the color to the table
tidy_combined_consumption_table$circle_color <- NA

for(i in 1:length(tidy_combined_consumption_table$circle_color)) {
  if(!is.na(tidy_combined_consumption_table$cons[i]) && tidy_combined_consumption_table$cons[i] < quantile(consumption$`2016`, na.rm = TRUE, .125)) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[1]
  } else if(!is.na(tidy_combined_consumption_table$cons[i]) && tidy_combined_consumption_table$cons[i] < quantile(consumption$`2016`, na.rm = TRUE, .25)) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[2]
  } else if(!is.na(tidy_combined_consumption_table$cons[i]) && tidy_combined_consumption_table$cons[i] < quantile(consumption$`2016`, na.rm = TRUE, .375)) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[3]
  } else if(!is.na(tidy_combined_consumption_table$cons[i]) && tidy_combined_consumption_table$cons[i] < quantile(consumption$`2016`, na.rm = TRUE, .5)) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[4]
  } else if(!is.na(tidy_combined_consumption_table$cons[i]) && tidy_combined_consumption_table$cons[i] < quantile(consumption$`2016`, na.rm = TRUE, .625)) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[5]
  } else if(!is.na(tidy_combined_consumption_table$cons[i]) && tidy_combined_consumption_table$cons[i] < quantile(consumption$`2016`, na.rm = TRUE, .75)) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[6]
  } else if(!is.na(tidy_combined_consumption_table$cons[i]) && tidy_combined_consumption_table$cons[i] < quantile(consumption$`2016`, na.rm = TRUE, .875)) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[7]
  } else if(!is.na(tidy_combined_consumption_table$cons[i])) {
    tidy_combined_consumption_table$circle_color[i] <- circle_colors[8]
  } else {
    tidy_combined_consumption_table$circle_color[i] <- NA
  }
}

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
production$country <- as.character(production$country)
consumption$country <- as.character(consumption$country)
cleanedPro <- dplyr::filter(production, !(production$country %in% anti_join(x = production, y = population, by = "country")$country))
cleanedCon <- dplyr::filter(consumption, !(consumption$country %in% anti_join(x = consumption, y = population, by = "country")$country))
cleanedPop <- dplyr::filter(population, !(population$country %in% anti_join(x = population, y = production, by = "country")$country))
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

# Page 4 ----
prod1.5 <- melt(production, id="country")
names(prod1.5)[names(prod1.5) == "variable"] <- "year"
#prod1.5 <- prod1.5[order(prod1.5$country),]

data <- prod1.5[prod1.5$country == "United States",]$value
M2 = auto.arima(data)
M2F = forecast(M2)
plot(M2F, main="ARIMA Forecast")

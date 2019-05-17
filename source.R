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

#Add coordinates for West Germany and East Germany
with_two_germanys <- c(drop_abbreviations, "50 10 Germany, West", "52 12 Germany, East")

latitudes <- str_replace_all(with_two_germanys, c(" .*$" = ""))
longitudes <- str_replace_all(with_two_germanys, c("^[^ ]* " = ""))
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
#cat("The maximum is", mx, "\n")

mx <- max(consumption[,-1],na.rm=T) 
#cat("The maximum is", mx, "\n")

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

##### Adding per-capita functionality to map

# Import population table
pop_table <- read.csv("worldPopulation.csv", stringsAsFactors = FALSE)

# Remove extra columns at the beginning
pop_table[,1] <- NULL
pop_table$Series.Code <- NULL
pop_table$Country.Code <- NULL

# Make column names consistent
colnames(pop_table)[1] <- "country"
colnames(pop_table)[2:38] <- c("1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")

# Create the production and consumption tables (which we will later join with population info)
capita_production_table <- combined_production_table
capita_consumption_table <- combined_consumption_table

#First, have to filter out countries/territories which aren't in the population/consumption tables
capita_production_table <- capita_production_table[-c(6, 46, 64, 69, 75, 76, 82, 120, 127, 134, 141, 147, 152, 162, 169, 166, 188, 192, 216),]
capita_consumption_table <- capita_consumption_table[-c(6, 46, 64, 69, 75, 76, 82, 120, 127, 134, 141, 147, 152, 162, 169, 166, 188, 192, 216),]

#Make country names in population table match country names in production/consumption tables
pop_table$country[28] <- "Virgin Islands, British"
pop_table$country[29] <- "Brunei"
pop_table$country[136] <- "Burma (Myanmar)"
pop_table$country[33] <- "Cape Verde"
pop_table$country[45] <- "Congo (Kinshasa)"
pop_table$country[46] <- "Congo (Brazzaville)"
pop_table$country[48] <- "Cote dIvoire (IvoryCoast)"
pop_table$country[87] <- "Hong Kong"
pop_table$country[118] <- "Macau"
pop_table$country[59] <- "Egypt"
pop_table$country[162] <- "Russia"
pop_table$country[92] <- "Iran"
pop_table$country[104] <- "Korea, North"
pop_table$country[105] <- "Korea, South"
pop_table$country[108] <- "Kyrgyzstan"
pop_table$country[109] <- "Laos"
pop_table$country[174] <- "Slovakia"
pop_table$country[190] <- "Syria" 
pop_table$country[211] <- "Venezuela" 
pop_table$country[194] <- "Timor-Leste (East Timor)"
pop_table$country[215] <- "Yemen"
pop_table$country[213] <- "Virgin Islands,  U.S."
pop_table$country[185] <- "Saint Vincent/Grenadines" 
pop_table$country[183] <- "Saint Lucia"
pop_table$country[182] <- "Saint Kitts and Nevis"

#We also have to remove countries/territories from the population table which aren't in the production/consumption tables, as well as the extra categories (218-269)
pop_table <- pop_table[-c(5, 40, 51, 64, 95, 115, 125, 129, 131, 146, 151, 165, 184, 173, 179, 202, 214, 218:269),]

#Tidy pop_table into long format
pop_table <- gather(data = pop_table, key = Year, value = Population, 2:38)

#Serbia is missing population data before 1990, but it's shown as ".." instead of "NA," so changing it to "NA" here
for(i in 1980:1989) {
  year_str <- toString(i)
  
  pop_table[((pop_table$Year == year_str) & (pop_table$country == "Serbia")),]$Population <- NA
}
#Same for Eritrea from 2012 through 2016
for(i in 2012:2016) {
  year_str <- toString(i)
  
  pop_table[((pop_table$Year == year_str) & (pop_table$country == "Eritrea")),]$Population <- NA
}
#Same for Kuwait from 1992 through 1994
for(i in 1992:1994) {
  year_str <- toString(i)
  
  pop_table[((pop_table$Year == year_str) & (pop_table$country == "Kuwait")),]$Population <- NA
}

#Tidy into long format and join to create per-capita production table
capita_production_table <- gather(data = capita_production_table, key = Year, value = Production, 2:38)
capita_production_table <- left_join(x = capita_production_table, y = pop_table, by = c("country", "Year"))

#Tidy into long format and join to create per-capita consumption table
capita_consumption_table <- gather(data = capita_consumption_table, key = Year, value = Consumption, 2:38)
capita_consumption_table <- left_join(x = capita_consumption_table, y = pop_table, by = c("country", "Year"))



#Create another column for production per capita and consumption per capita
#First, be sure production/consumption and population columns are numeric
capita_production_table$Production <- as.numeric(capita_production_table$Production)
capita_production_table$Population <- as.numeric(capita_production_table$Population)

capita_consumption_table$Consumption <- as.numeric(capita_consumption_table$Consumption)
capita_consumption_table$Population <- as.numeric(capita_consumption_table$Population)

#Use "mutate" to create the new column in both tables
capita_production_table <- mutate(capita_production_table, ppc = capita_production_table$Production / capita_production_table$Population)

capita_consumption_table <- mutate(capita_consumption_table, cpc = capita_consumption_table$Consumption / capita_consumption_table$Population)


#Create the circle colors for the per-capita production and consumption

prod_2016 <- dplyr::filter(capita_production_table, capita_production_table$Year == "2016")

cons_2016 <- dplyr::filter(capita_consumption_table, capita_consumption_table$Year == "2016")

#First, production

capita_production_table$circle_color <- NA

for(i in 1:length(capita_production_table$circle_color)) {
  if(!is.na(capita_production_table$ppc[i]) && capita_production_table$ppc[i] < quantile(prod_2016$ppc, na.rm = TRUE, .125)) {
    capita_production_table$circle_color[i] <- circle_colors[1]
    
  } else if(!is.na(capita_production_table$ppc[i]) && capita_production_table$ppc[i] < quantile(prod_2016$ppc, na.rm = TRUE, .25)) {
    capita_production_table$circle_color[i] <- circle_colors[2]
    
  } else if(!is.na(capita_production_table$ppc[i]) && capita_production_table$ppc[i] < quantile(prod_2016$ppc, na.rm = TRUE, .375)) {
    capita_production_table$circle_color[i] <- circle_colors[3]
    
  } else if(!is.na(capita_production_table$ppc[i]) && capita_production_table$ppc[i] < quantile(prod_2016$ppc, na.rm = TRUE, .5)) {
    capita_production_table$circle_color[i] <- circle_colors[4]
    
  } else if(!is.na(capita_production_table$ppc[i]) && capita_production_table$ppc[i] < quantile(prod_2016$ppc, na.rm = TRUE, .625)) {
    capita_production_table$circle_color[i] <- circle_colors[5]
    
  } else if(!is.na(capita_production_table$ppc[i]) && capita_production_table$ppc[i] < quantile(prod_2016$ppc, na.rm = TRUE, .75)) {
    capita_production_table$circle_color[i] <- circle_colors[6]
    
  } else if(!is.na(capita_production_table$ppc[i]) && capita_production_table$ppc[i] < quantile(prod_2016$ppc, na.rm = TRUE, .875)) {
    capita_production_table$circle_color[i] <- circle_colors[7]
    
  } else if(!is.na(capita_production_table$ppc[i])) {
    capita_production_table$circle_color[i] <- circle_colors[8]
    
  } else {
    capita_production_table$circle_color[i] <- NA
  }
}

  #Consumption
capita_consumption_table$circle_color <- NA

for(i in 1:length(capita_consumption_table$circle_color)) {
  if(!is.na(capita_consumption_table$cpc[i]) && capita_consumption_table$cpc[i] < quantile(cons_2016$cpc, na.rm = TRUE, .125)) {
    capita_consumption_table$circle_color[i] <- circle_colors[1]
    
  } else if(!is.na(capita_consumption_table$cpc[i]) && capita_consumption_table$cpc[i] < quantile(cons_2016$cpc, na.rm = TRUE, .25)) {
    capita_consumption_table$circle_color[i] <- circle_colors[2]
    
  } else if(!is.na(capita_consumption_table$cpc[i]) && capita_consumption_table$cpc[i] < quantile(cons_2016$cpc, na.rm = TRUE, .375)) {
    capita_consumption_table$circle_color[i] <- circle_colors[3]
    
  } else if(!is.na(capita_consumption_table$cpc[i]) && capita_consumption_table$cpc[i] < quantile(cons_2016$cpc, na.rm = TRUE, .5)) {
    capita_consumption_table$circle_color[i] <- circle_colors[4]
    
  } else if(!is.na(capita_consumption_table$cpc[i]) && capita_consumption_table$cpc[i] < quantile(cons_2016$cpc, na.rm = TRUE, .625)) {
    capita_consumption_table$circle_color[i] <- circle_colors[5]
    
  } else if(!is.na(capita_consumption_table$cpc[i]) && capita_consumption_table$cpc[i] < quantile(cons_2016$cpc, na.rm = TRUE, .75)) {
    capita_consumption_table$circle_color[i] <- circle_colors[6]
    
  } else if(!is.na(capita_consumption_table$cpc[i]) && capita_consumption_table$cpc[i] < quantile(cons_2016$cpc, na.rm = TRUE, .875)) {
    capita_consumption_table$circle_color[i] <- circle_colors[7]
    
  } else if(!is.na(capita_consumption_table$cpc[i])) {
    capita_consumption_table$circle_color[i] <- circle_colors[8]
    
  } else {
    capita_consumption_table$circle_color[i] <- NA
  }
}





# Page 3 ----
# Population data from 1980 to 2016
worldBank <- function (data){
  data <- data [,-(1:2)] %>% .[,-2]
  names (data) <- c("country", cols)
  # Modify country names to match with production and consumption data
  data$country <- as.character(data$country)
  data$country[33] <- "Cape Verde"
  data$country[136] <- "Burma (Myanmar)"
  data$country[45] <- "Congo (Kinshasa)"
  data$country[46] <- "Congo (Brazzaville)"
  data$country[59] <- "Egypt"
  data$country[87] <- "Hong Kong"
  data$country[104] <- "Korea, North"
  data$country[105] <- "Korea, South"
  data$country[109] <- "Laos"
  data$country[118] <- "Macau"
  data$country[146] <- "Macedonia"
  data$country[140] <- "Netherlands"
  data$country[162] <- "Russia"
  data$country[64] <- "Swaziland"
  data$country[213] <- "Virgin Islands, U.S."
  data$country[28] <- "Virgin Islands, British"
  data$country[215] <- "Yemen"
  
  data
}
population <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/worldPopulation.csv") %>%
  worldBank(.)

ppp <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/worldPPP.csv") %>%
  worldBank(.)
ppp[ppp == "" | ppp == ".."] <- NA

# Join the data (intersection)
# Combine Germany East, West to Germany
for (i in (1980:1990)){
  production[78,paste(i)] <- production[79,paste(i)] + production[80,paste(i)]
  consumption[78,paste(i)] <- consumption[79,paste(i)] + consumption[80,paste(i)] 
}
production$country <- as.character(production$country)
consumption$country <- as.character(consumption$country)

# This function is for sorting the countries by alphabetically and set the row name consistent for further cleaning process
sortByCountry <- function (data){
  data <- data[order(data$country),]
  row.names(data) <- c(1:191)
  data
} 

cleanedPro <- dplyr::filter(production, !(production$country %in% anti_join(x = production, y = population, by = "country")$country)) %>%
  sortByCountry (.)
cleanedCon <- dplyr::filter(consumption, !(consumption$country %in% anti_join(x = consumption, y = population, by = "country")$country))%>%
  sortByCountry (.)
cleanedPop <- dplyr::filter(population, !(population$country %in% anti_join(x = population, y = production, by = "country")$country))%>%
  sortByCountry (.)
cleanedPPP <- dplyr::filter(ppp, !(ppp$country %in% anti_join(x = ppp, y = production, by = "country")$country)) %>%
  sortByCountry (.)
for (i in 2:38){
  cleanedPop[,i] <- parse_number(as.character(cleanedPop[,i]))
  cleanedPPP[,i] <- parse_number(as.character(cleanedPPP[,i]))
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

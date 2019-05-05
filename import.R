library(dplyr)

# This function is used for dropping unneccesary column and set the column names
set <- function(data){
  data <- data [-c(1,3)]
  names(data) <- c("country",
                   "1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
                   "1990","1991","1992","1993","1994","1995","1996","1997","1998","1999",
                   "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
                   "2010","2011","2012","2013","2014","2015","2016")
  cols <- c ("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989",
             "1990","1991","1992","1993","1994","1995","1996","1997","1998","1999",
             "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009",
             "2010","2011","2012","2013","2014","2015","2016")
  
  for (col in cols){
    data[,col] <- as.numeric(as.character(data[,col]))
    data[,col] <- (10^15 * data[,col])
  }
  data
}

# Dataframe for production of primary energy by country from 1980 to 2016 (unit: btu)
production <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/International_data_energy.csv", 
                       skip = 8, nrows = 228, head = FALSE) %>%
  set(.)

# Dataframe for consumption of primary energy by country from 1980 to 2016 (unit: btu)
consumption <- read.csv("https://raw.githubusercontent.com/stellasylee/Energy/master/International_data_energy.csv",
                        skip = 238, head = FALSE) %>%
  set(.)



library(rvest)
library(stringr)
library(readr)
library(ggplot2)

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







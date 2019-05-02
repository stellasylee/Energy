library (dplyr)

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

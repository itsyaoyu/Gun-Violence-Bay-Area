# This script is a slightly modified version of the original here: 
# http://www.storybench.org/geocode-csv-addresses-r/

# Geocoding a csv column of "addresses" in R

#load ggmap
library(ggmap)
library(tidyverse)

# Read in the CSV data and store it in a variable 
origAddress <- read.csv("the_trace_data/Oakland_CA/OK_trace_cleaned.csv", stringsAsFactors = FALSE)

# Add in oakland, ca or san francisco, ca to end of street addresses
origAddress <- origAddress %>%
  mutate(Location = sapply(origAddress$Location..Address, function(x) paste(x, ', oakland, ca')))

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

#Google api key (hidden for obvious reasons)

register_google(key = "***************************************")

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$Location[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}
# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "clean-data/trace_OK.csv", row.names=FALSE)

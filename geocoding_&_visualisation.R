# Geocoding

# Install libraries
# _________________
# N/B: I used R 4.0.4 on linux Arch while writing this code.
install.packages(c("ggmap", "tmaptools","RCurl","jsonlite", "tidyverse","leaflet",
                   "ggplot2", "RgoogleMaps"))

# Import libraries:
library(plyr)
library(ggmap)
library(ggplot2)
library(leaflet)
library(reshape2)
library(tidyverse)
library(RgoogleMaps)

# Set working directory:
setwd("~/RProjects/GEOCODOING")

# Import Data:
data <- "Nairobi_Security_Incidents_raw.csv"
security_incidents <- read.csv(data, header = TRUE, na.strings = "")
attach(crime)

# Summary of data:
# __________________
# Set the Date variable (Date of incident) as a date
security_incidents$Date <- as.Date(security_incidents$Date)


# Data Structure:
str(security_incidents)

# Descriptives
# library(plyr)

# By Constituency:
constituency_count <- data.frame(
    plyr::count(security_incidents, 'Constituency')
  )
constituency_count

# By Offence type:
offence_count <- data.frame(
    plyr::count(security_incidents, 'Offence.Type')
  )
offence_count

# by Offence Category:
offence_category_count <- data.frame(
  plyr::count(security_incidents, 'Offence.Category')
)
offence_category_count


# By Constituency and Offence Category:
# library(reshape2)
constituency_category <- melt(
  table(
    security_incidents[,c("Constituency", "Offence.Type")]
  )
)
constituency_category

#  Geocoding
# __________
# library(ggmap)
# Integrate, calling the Google API key created and activated.
register_google(key = "XXXX")

# Loop through the locations to get the latitude and longitude of each address 
# and add it to the security_incidents data frame in new columns lat and long
for(i in 1:nrow(security_incidents)){
  # Print("Working...")
  loc_coord <- data.frame(tryCatch(geocode(
    data.frame(Add = paste(security_incidents$Constituency,security_incidents$Location,"Kenya",sep = ","))$Add[i],
    output = "latlona",
    source = "google"),
    warning = function(w) data.frame(lon = NA, lat = NA)))
  security_incidents$lat[i] <- as.numeric(loc_coord[2])
  security_incidents$long[i] <- as.numeric(loc_coord[1])
}

# Save the geocoded data
write.csv(security_incidents,"Nairobi_Security_Incidents_raw.csv")

# Mapping
# _______
# library(leaflet)
leaflet(security_incidents) %>%
  addTiles() %>%
  addCircleMarkers(security_incidents$long, security_incidents$lat, popup = security_incidents$Location )

# Plot the security operation offences in Nairobi
security_operation <- subset(security_incidents, Offence.Type == "SECURITY OPERATION")
qmplot(long, lat, data = security_operation, colour = I('red'), size = I(3), darken = .3)

# dusitD2 location and its environs.
attack_location <- 'dusitD2 nairobi'
qmap(attack_location, zoom = 17)

# Terrain representation
qmap(attack_location, zoom = 16, source = "stamen", maptype = "toner")

# Violent Crimes General
# find a reasonable spatial extent
qmap("nairobi", zoom = 12)

# order offense category
crime_data$Offence.Category <- 
  factor(crime_data$Offence.Category,
         levels = c("Arrest", "Assault", "Robbery", "Criminal killing", "Theft",
                    "Illicit trade"))

theme_set(theme_bw(16))
crimemap <- qmap("nairobi", zoom = 11, color = "bw") 

crimemap +
  geom_point(aes(x = long, y = lat, colour = Offence.Category, size = Offence.Category),
             data = security_incidents)
crimemap +
  stat_bin2d(
    aes(x = long, y = lat, colour = Offence.Category, fill = Offence.Category),
    size = .5, bins = 30, alpha = 1/2,
    data = security_incidents
  )

# Violent crime density
nairobi <- get_map("nairobi", zoom = 12)
nairobiMap <- ggmap(nairobi, extent = "device")
nairobiMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = security_incidents,
    geom = "polygon"
  ) +
  scale_fill_gradient(low = "blue", high = "red")

# Crime density over the week:
 security_incidents$Day <-  factor(security_incidents$Day, 
   levels=c('Monday','Tuesday','Wednesday','Thursday', 'Friday', 'Saturday', 'Sunday'))
 
nairobi <- get_map(location = "nairobi", zoom = 11, color = "bw",
                   source = "stamen")

nairobiMap <- ggmap(nairobi, base_layer = ggplot(aes(x = long, y = lat),
                                                 data = security_incidents))

nairobiMap +
  stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = security_incidents) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(.~ Day)

# Distance
from <- "Westgate Shopping Mall"
to <- "dusitD2"

mapdist(from, to)

# Route
# Directions API
legs_df <- route(
  "Doonholm, Nairobi",
  "Westgate Shopping Mall, 15 Mwanzi Rd Nairobi KE",
  alternatives = FALSE
)

qmap('nairobi', zoom = 13, maptype = 'hybrid',
     base_layer = ggplot(aes(x = start_lon, y = start_lat), data = legs_df)) +
  geom_leg(
    aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat,
        colour = route),
    alpha = 3/4, size = 2, data = legs_df
  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
  facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')

# Fun GIF!
# import libraries
library(purrr) 
library(magick)

Path = "/home/imusebe/RProjects/GEOCODOING/Mozambique"
MozamBeach_gif <- list.files(path = Path, pattern = "*.jpg", full.names = T) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_annotate("Mozambique, 1740 S Coast Hwy, Laguna Beach May 2019 - Aug 2019 ", location = "+10+10", size = 20, color = "white") %>%
  image_animate(fps=4) %>% 
  image_write("MozamBeach.gif") 

# End
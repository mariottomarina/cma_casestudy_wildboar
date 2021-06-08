# Libraries to use
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(lubridate)    # To handle dates and times
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData") # getting data from R Package
library(ComputationalMovementAnalysisData) #Wild boar data is on that package
library(sf) # encoding spatial vector data 
library(ggmap)
library(maps)
library(mapdata)
library(suncalc)
library(rgdal)

# Getting the data from the package
Wildschwein_BE <- wildschwein_BE # Why only data from BE? Should we only focus on the schrecks from there? 
Wildschwein_meta <- wildschwein_metadata
Wildschwein_overlap_temp <- wildschwein_overlap_temp
Schreckagenda <- schreck_agenda    # Was bedeutet Phase in der Schreckagenda?
Schrecklocation <- schreck_locations

# Joining Schreckagenda with Schreckagenda with a left join
Schrecklocation <- left_join(Schrecklocation, Schreckagenda, by = "id")
Schrecklocation$jagddruck <- factor(Schrecklocation$jagddruck, levels =c("gering", "mittel", "hoch"))
Schrecklocation <- Schrecklocation %>%
  filter(region == "fanel")

Schrecklocation_sf <- st_as_sf(Schrecklocation, 
                               coords = c("lon", "lat"),
                               crs = 4326)
# Filtering locations, so only Schrecklocations of Canton of Bern (fanel & hagneck) is in the data
Schrecklocation_sf <- Schrecklocation_sf %>%
  filter(region == "fanel")

Schrecklocation_sf <- st_transform(Schrecklocation_sf, 2056)
st_crs(Schrecklocation_sf)

coords_sep <- as.data.frame(st_coordinates(Schrecklocation_sf))

Schrecklocation$E <- coords_sep$X
Schrecklocation$N <- coords_sep$Y

# Getting map of bern
#get_map(location = c(-122.080954, 36.971709), maptype = "terrain", source = "osm", zoom = 14)

# Plot of all Wildschwein Schrecks and Wildschwein
ggplot() +
  geom_point(data = Wildschwein_BE, aes(x= E, y=N)) +
  geom_path(data = Wildschwein_BE, aes(x=E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N, shape = jagddruck), colour = "red")
  

# Setting column day/night to Wildbaor data

# Add a date column (with whatever timezone you want)

Wildschwein_BE$date <- as.Date(Wildschwein_BE$DatetimeUTC, tz = 'UTC')

#getting lat lon data for calculating sunrise and sunset
BE_sf <- st_as_sf(Wildschwein_BE, 
                  coords = c("E", "N"),
                  crs = 2056)
BE_sf <- st_transform(BE_sf, 4326)
coords_sep_BE <- as.data.frame(st_coordinates(BE_sf))
Wildschwein_BE$lon <- coords_sep_BE$X
Wildschwein_BE$lat <- coords_sep_BE$Y

# calculating sunset and sunrise

getSunlightTimes(date = Wildschwein_BE$date, 
                                    lat = Wildschwein_BE$lat, 
                                    lon = Wildschwein_BE$lon,
                                    keep = c("sunrise", "sunset"), 
                                    tz = "UTC")






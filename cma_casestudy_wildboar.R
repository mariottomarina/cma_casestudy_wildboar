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

# generating dataframes with single data point for each day of timespan for schrecks
dur_schreck <- function(datum_on, datum_off){ 
  difftime(datum_on, datum_off,  units = "days")
}

Schrecklocation <- Schrecklocation %>%
  group_by(id) %>%
  mutate(duration = dur_schreck(datum_off,datum_on) )
Schrecklocation$duration <- as.integer(Schrecklocation$duration)  # unit days

seq_dates <- function(data, datum_on, datum_off, i){
    dates <- seq(as.Date(datum_on [i]), as.Date(datum_off [i]), by="days")
    df1 <- data.frame(dates)
    df1$id <- data$id [i]
    df1$lat <- data$lat [i]
    df1$lon <- data$lon [i]
    print(df1)
    return(df1)
  }

Loc1 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=1)
Loc2 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=2)
Loc3 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=3)
Loc4 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=4)
Loc5 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=5)
Loc6 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=6)
Loc7 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=7)
Loc8 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=8)
Loc9 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=9)
Loc10 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=10)
Loc11 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=11)
Loc12 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=12)
Loc13 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=13)
Loc14 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=14)
Loc15 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=15)
Loc16 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=16)
Loc17 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=17)
Loc18 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=18)
Loc19 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=19)
Loc20 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=20)
Loc21 <- seq_dates(Schrecklocation, Schrecklocation$datum_on, Schrecklocation$datum_off, i=21)

Locations_adapted <- rbind(Loc1, Loc2, Loc3, Loc4, Loc5, Loc6, Loc7, Loc8, Loc9, Loc10, Loc11, Loc12, Loc13, Loc14, Loc15, Loc16, Loc17, Loc18, Loc19, Loc20, Loc21)

# calculating sunset and sunrise for Schrecklocations
sun <- function(data) {
for (i in 1:nrow(data)){
  suntimes <- getSunlightTimes(date = data$dates [i], 
                               lat = data$lat [i], 
                               lon = data$lon [i],
                               keep = c("sunrise", "sunset"),
                               tz = "UTC")
  data$sunrise [i] <- suntimes$sunrise
  data$sunset  [i] <- suntimes$sunset
  data$sunrise <- as.POSIXct(data$sunrise, origin="1970-01-01")
  data$sunset  <-  as.POSIXct(data$sunset, origin="1970-01-01")
}
  print(data)
}

Loc_adap_suntimes <- sun(Locations_adapted)


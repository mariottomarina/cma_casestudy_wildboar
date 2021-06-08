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

Schrecklocation_sf <- st_as_sf(Schrecklocation, 
                               coords = c("lon", "lat"),
                               crs = 4326)
st_crs(Schrecklocation_sf)
# Filtering locations, so only Schrecklocations of Canton of Bern (fanel & hagneck) is in the data
Schrecklocation_sf <- Schrecklocation_sf %>%
  filter(region == "fanel" | region == "hagneck")

Schrecklocation_sf <- st_transform(Schrecklocation_sf, 2056)

# Getting map of bern
#get_map(location = c(-122.080954, 36.971709), maptype = "terrain", source = "osm", zoom = 14)

# Plot of all Wildschwein Schrecks
ggplot(Schrecklocation_sf, aes(colour=region, shape = jagddruck)) +
  geom_sf(alpha = 0.4) 

# Separation of dataframe according to region
Schrecklocation_fanel <- Schrecklocation_sf %>%
  filter(region == "fanel")

Schrecklocation_hagneck <- Schrecklocation_sf %>%
  filter(region == "hagneck")

Schrecklocation_cheyres <- Schrecklocation_sf %>%
  filter(region == "cheyres")

# Plotting Schecks accoring to region

ggplot(Schrecklocation_fanel, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))

ggplot(Schrecklocation_hagneck, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))

ggplot(Schrecklocation_cheyres, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))


# Plotting for presentation
wildschwein_BE <- wildschwein_BE %>%
  mutate(timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC,units = "secs")))
group_by(wildschwein_BE, TierID) # Es sind 19 Wildschweine

ggplot(wildschwein_BE, aes(DatetimeUTC, TierID))+ 
  geom_point()

Wildschwein_BE_sf <- st_as_sf(Wildschwein_BE, 
                              coords = c("E", "N"),
                              crs = 2056)

Pres_schreckloc <- Schrecklocation_sf %>%
  filter(id == "WSS_2014_04" | id == "WSS_2014_05")

WB_Ueli <- Wildschwein_BE_sf %>%
  filter(TierName == "Ueli") %>%
  filter(DatetimeUTC > "2014-05-01 00:00:00" | DatetimeUTC < "2014-10-28 23:59:59")

ggplot() +
  geom_sf(data = WB_Ueli, alpha = 0.2) +
  geom_line(data = WB_Ueli, aes(geometry [1], geometry [2])) +
  geom_sf(data = Pres_schreckloc, alpha = 0.4, colour = "red", shape = 17, size = 3) +
  coord_sf(xlim = c(7.05, 7.07), ylim = c(46.99, 47.00), crs = 4326)

# Setting column day/night to Wildbaor data

# Add a date column (with whatever timezone you want)

change_crs <- data.frame(lat = Wildschwein_BE$E, lon = Wildschwein_BE$N)
Wildschwein_BE$date <- as.Date(Wildschwein_BE$DatetimeUTC, tz = 'UTC')


getSunlightTimes(date = "2014-05-28", 
                 lat = 2570390, 
                 lon = 1204820,
                 keep = c("sunrise", "sunset"), tz = "UTC")
class(Wildschwein_BE$date)

# Following generates the sunrise and sunset times for the two example dates
#sunRise <- c(as.POSIXct('2016-04-15 06:40:37'), as.POSIXct('2016-03-24 06:55:00'))
#sunSet <- c(as.POSIXct('2016-04-15 18:40:37'), as.POSIXct('2016-03-24 18:25:00'))
#sun <- data.frame(date = as.Date(sunRise, tz = 'EST'), sunRise = sunRise, sunSet = sunSet)

# Join the two tables and compute night/day
#df <- inner_join(df, sun)
#df$dayNight <- ifelse(df$DateTime > df$sunRise & df$DateTime < df$sunSet, 'day', 'night')





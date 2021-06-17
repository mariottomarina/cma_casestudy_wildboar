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
library(spatialrisk)
library(purrr)

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
                               keep = c("dusk", "dawn"),
                               tz = "UTC")
  data$dusk [i] <- suntimes$dusk
  data$dawn  [i] <- suntimes$dawn
  data$dusk <- as.POSIXct(data$dusk, origin="1970-01-01")
  data$dawn  <-  as.POSIXct(data$dawn, origin="1970-01-01")
}
  print(data)
}
Loc_adap_suntimes <- sun(Locations_adapted)

# drawing buffer around Wildschweinschrecks
joined_schreck <- left_join(Loc_adap_suntimes, Schrecklocation)

joined_schreck_sf <- st_as_sf(Schrecklocation,                    # Making SF Object
                              coords = c("lon", "lat"),
                              crs = 4326)
joined_schreck_sf <- st_transform(joined_schreck_sf, 2056)        # Transforming coordinate system. st_buffer doesn't like to work with lat lon. 

  # creating seperat column for the year, so we can filter that
Wildschwein_BE$year <- format(as.Date(Wildschwein_BE$DatetimeUTC, format = "%Y-%m-%d"),"%Y")
Schrecklocation$year <- format(as.Date(Schrecklocation$datum_on, format = "%Y-%m-%d"),"%Y")

# Filtering Wildschein Data depending on the year
Wildschwein_BE_14 <- Wildschwein_BE %>%
  filter(year == "2014")
Wildschwein_BE_15 <- Wildschwein_BE %>%
  filter(year == "2015")
Wildschwein_BE_16 <- Wildschwein_BE %>%
  filter(year == "2016")

# Filtering Schrecklocation depending on the year.
Schreckslocation_14 <- Schrecklocation %>%
  filter(year == "2014")

Schreckslocation_15 <- Schrecklocation %>%
  filter(year == "2015")
Schreckslocation_15_1 <- Schreckslocation_15 [1:2,]
Schreckslocation_15_3 <- Schreckslocation_15 [3,]
Schreckslocation_15_4 <- Schreckslocation_15 [4,]


Schreckslocation_16 <- Schrecklocation %>%
  filter(year == "2016")
Schreckslocation_16_1 <- Schreckslocation_16 [1,]
Schreckslocation_16_2 <- Schreckslocation_16 [2,]
Schreckslocation_16_3 <- Schreckslocation_16 [3,]
Schreckslocation_16_4 <- Schreckslocation_16 [4,]


# Function to calculate the euclidean distance between the Schreck and the boar
library(compiler)
enableJIT(3)

distance <- function(data1, data2) {
  dis <- data.frame(IDSchreck = character(),
                    TierName = character(),
                    DateTimeUTC = character(),
                    distance = numeric())
  z <- 1
  for (i in 1:nrow(data1)) {
    for (j in 1:nrow(data2)) {
      distance <- sqrt((data1$E [i] - data2$E [j])^2 + (data1$N [i] - data2$N [j])^2)
      new_vec <- c(data1$id [i], data2$TierName [j], data2$DatetimeUTC [j], distance)
      dis [z, ] <- new_vec
      z <- z+1
      print(j)
    }
    print(i)
    print("i")
  }
  distance_wildboar_schreck <<- dis
}

# execute the function
distance_14 <- distance(Schreckslocation_14, Wildschwein_BE_14)
distance_15_3_33 <- distance(Schreckslocation_15_3, Wildschwein_BE_15)
distance_15_4_44 <- distance(Schreckslocation_15_4, Wildschwein_BE_15)
distance_16_1 <- distance(Schreckslocation_16_1, Wildschwein_BE_16)
distance_16_2 <- distance(Schreckslocation_16_2, Wildschwein_BE_16)
distance_16_3 <- distance(Schreckslocation_16_3, Wildschwein_BE_16)
distance_16_4 <- distance(Schreckslocation_16_4, Wildschwein_BE_16)

# combining files
distance_15 <- rbind(distance_15_1, distance_15_3_33, distance_15_4_44, distance_15_3)
distance_16 <- rbind(distance_16_1, distance_16_2, distance_16_3, distance_16_4)

# saving distance files as csv
write.csv(distance_14, "P:/03_ZHAW_MSc/12_Patterns and Trends in Environmental Data/Semesterproject/cma_casestudy_wildboar/distance_14.csv", row.names = FALSE)
write.csv(distance_15, "P:/03_ZHAW_MSc/12_Patterns and Trends in Environmental Data/Semesterproject/cma_casestudy_wildboar/distance_15.csv", row.names = FALSE)
write.csv(distance_16, "P:/03_ZHAW_MSc/12_Patterns and Trends in Environmental Data/Semesterproject/cma_casestudy_wildboar/distance_16.csv", row.names = FALSE)

# Remove rows with distance > 1500 m



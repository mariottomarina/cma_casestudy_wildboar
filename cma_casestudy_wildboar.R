# Libraries to use
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(lubridate)    # To handle dates and times
# devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData") # getting data from R Package
library(ComputationalMovementAnalysisData) #Wild boar data is on that package
library(sf) # encoding spatial vector data 
library(ggmap)
library(maps)
library(mapdata)
library(suncalc)
library(rgdal)
library(spatialrisk)
library(purrr)
library(circle)
library(ggforce)
library(car)
library(ggsignif)
library(ggpubr)

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

######Plot machen für jedes Wildschwein
Wildschwein_BE$TierName <- as.factor(Wildschwein_BE$TierName)
levels(Wildschwein_BE$TierName)

Amos <- Wildschwein_BE %>%
  filter(TierName =="Amos")
Amos_plot <- ggplot() +
  geom_path(data = Amos, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Amos, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Amos_plot_alldata.png", Amos_plot)

Caroline <- Wildschwein_BE %>%
  filter(TierName =="Caroline")
Caroline_plot <- ggplot() +
  geom_path(data = Caroline, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Caroline, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Caroline_plot_alldata.png", Caroline_plot)

Claude <- Wildschwein_BE %>%
  filter(TierName =="Claude")
Claude_plot <- ggplot() +
  geom_path(data = Claude, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Claude, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Claude_plot_alldata.png", Claude_plot)

Donar <- Wildschwein_BE %>%
  filter(TierName =="Donar")
Donar_plot <- ggplot() +
  geom_path(data = Donar, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Donar, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Donar_plot_alldata.png", Donar_plot)

Evelin <- Wildschwein_BE %>%
  filter(TierName =="Evelin")
Evelin_plot <- ggplot() +
  geom_path(data = Evelin, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Evelin, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Evelin_plot_alldata.png", Evelin_plot)

Franz <- Wildschwein_BE %>%
  filter(TierName =="Franz")
Franz_plot <- ggplot() +
  geom_path(data = Franz, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Franz, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Franz_plot_alldata.png", Franz_plot)

Frida <- Wildschwein_BE %>%
  filter(TierName =="Frida")
Frida_plot <- ggplot() +
  geom_path(data = Frida, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Frida, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Frida_plot_alldata.png", Frida_plot)

Fritz <- Wildschwein_BE %>%
  filter(TierName =="Fritz")
Fritz_plot <- ggplot() +
  geom_path(data = Fritz, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Fritz, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Fritz_plot_alldata.png", Fritz_plot)

Gaby <- Wildschwein_BE %>%
  filter(TierName =="Gaby")
Gaby_plot <- ggplot() +
  geom_path(data = Gaby, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Gaby, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Gaby_plot_alldata.png", Gaby_plot)

Isabelle <- Wildschwein_BE %>%
  filter(TierName =="Isabelle")
Isabelle_plot <- ggplot() +
  geom_path(data = Isabelle, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Isabelle, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Isabelle_plot_alldata.png", Isabelle_plot)

Joanna <- Wildschwein_BE %>%
  filter(TierName =="Joanna")
Joanna_plot <- ggplot() +
  geom_path(data = Joanna, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Joanna, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Joanna_plot_alldata.png", Joanna_plot)

Miriam <- Wildschwein_BE %>%
  filter(TierName =="Miriam")
Miriam_plot <- ggplot() +
  geom_path(data = Miriam, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Miriam, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Joanna_plot_alldata.png", Miriam_plot)

Nicole <- Wildschwein_BE %>%
  filter(TierName =="Nicole")
Nicole_plot <- ggplot() +
  geom_path(data = Nicole, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Nicole, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Nicole_plot_alldata.png", Nicole_plot)

Olga <- Wildschwein_BE %>%
  filter(TierName =="Olga")
Olga_plot <- ggplot() +
  geom_path(data = Olga, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Olga, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Olga_plot_alldata.png", Olga_plot)

Rosa <- Wildschwein_BE %>%
  filter(TierName =="Rosa")
Rosa_plot <- ggplot() +
  geom_path(data = Rosa, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Rosa, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Rosa_plot_alldata.png", Rosa_plot)

Ruth <- Wildschwein_BE %>%
  filter(TierName =="Ruth")
Ruth_plot <- ggplot() +
  geom_path(data = Ruth, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Ruth, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Ruth_plot_alldata.png", Ruth_plot)

Sabine <- Wildschwein_BE %>%
  filter(TierName =="Sabine")
Sabine_plot <- ggplot() +
  geom_path(data = Sabine, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Sabine, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Sabine_plot_alldata.png", Sabine_plot)

Ueli <- Wildschwein_BE %>%
  filter(TierName =="Ueli")
Ueli_plot <- ggplot() +
  geom_path(data = Ueli, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Ueli, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Ueli_plot_alldata.png", Ueli_plot)

Venus <- Wildschwein_BE %>%
  filter(TierName =="Venus")
Venus_plot <- ggplot() +
  geom_path(data = Venus, aes(x=E, y=N), colour = "grey", lwd = 1) +
  geom_point(data = Venus, aes(x= E, y=N)) +
  geom_point(data = Schrecklocation, aes(x=E, y=N), colour = "blue", size = 2) +
  theme_bw()
ggsave("Venus_plot_alldata.png", Venus_plot)

# Plot Time Overlap boars and schrecks
Overlap_Schreck_boar <- ggplot() +
  geom_line(Wildschwein_BE, mapping = aes(x = date, y = TierName), colour = "darkgreen") +
  geom_line(Locations_adapted, mapping = aes(x = dates, y = id), colour = "blue") +
  theme_bw() +
  scale_x_date(date_breaks = "6 month") +
  labs(y = "Animal Names and SchreckID", x = "Time")
ggsave("Overlap_schreck_boar.png", Overlap_Schreck_boar)


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

# function for calculating dusk and dawn for Schrecklocations
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

# Load Distance files
distance_14 <- read_delim("distance_14.csv", ",")
distance_15 <- read_delim("distance_15.csv", ",")
distance_16 <- read_delim("distance_16.csv", ",")

distance_14$DateTimeUTC <- as.POSIXct(distance_14$DateTimeUTC, origin = "1970-01-01")
distance_15$DateTimeUTC <- as.POSIXct(distance_15$DateTimeUTC, origin = "1970-01-01")
distance_16$DateTimeUTC <- as.POSIXct(distance_16$DateTimeUTC, origin = "1970-01-01")

# rwmoving all data with distance higher than 1500
distance_14_fl <- distance_14 %>%
  filter(distance < 1500)
distance_15_fl <- distance_15 %>%
  filter(distance < 1500)
distance_16_fl <- distance_15 %>%
  filter(distance < 1500)

distance_14_fl$DateTimeUTC <- with_tz(distance_14_fl$DateTimeUTC, "UTC")
distance_15_fl$DateTimeUTC <- with_tz(distance_15_fl$DateTimeUTC, "UTC")
distance_16_fl$DateTimeUTC <- with_tz(distance_16_fl$DateTimeUTC, "UTC")


# year 2014
WSS_2014_04 <- distance_14_fl %>%
  filter(IDSchreck == "WSS_2014_04")%>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [1]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [1])
WSS_2014_05 <- distance_14_fl %>%
  filter(IDSchreck == "WSS_2014_05") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [2]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [2])
WSS_2014_06_s <- distance_14_fl %>%
  filter(IDSchreck == "WSS_2014_06")%>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [3]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [3])  # delete
WSS_2014_06_a <- distance_14_fl %>%
  filter(IDSchreck == "WSS_2014_06") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [4]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [4])  # delete

Overlap_boar <- rbind(WSS_2014_04, WSS_2014_05, WSS_2014_06_s, WSS_2014_06_a)
Overlap_boar$date <- as.Date(Overlap_boar$DateTimeUTC)
Overlap_Schreck_boar_after_processing <- ggplot() +
  geom_line(Overlap_boar, mapping = aes(x = date, y = TierName), colour = "darkgreen") +
  geom_line(Locations_adapted, mapping = aes(x = dates, y = id), colour = "blue") +
  theme_bw() +
  scale_x_date(date_breaks = "6 month") +
  labs(y = "Animal Names and SchreckID", x = "Time")
# year 2015
WSS_2015_01_s <- distance_15_fl %>%
  filter(IDSchreck == "WSS_2015_01") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [5]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [5])
WSS_2015_01_s <- distinct(WSS_2015_01_s)
WSS_2015_01_a <- distance_15_fl %>%
  filter(IDSchreck == "WSS_2015_01") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [6]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [6])
WSS_2015_01_a <- distinct(WSS_2015_01_a)
WSS_2015_03_s <- distance_15_fl %>%
  filter(IDSchreck == "WSS_2015_03") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [7]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [7])
WSS_2015_03_s <- distinct(WSS_2015_03_s)
WSS_2015_03_a <- distance_15_fl %>%
  filter(IDSchreck == "WSS_2015_03") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [8]) %>% 
  filter(DateTimeUTC <= Schrecklocation$datum_off [8])
WSS_2015_03_a <- distinct(WSS_2015_03_a)
WSS_2015_04 <- distance_15_fl %>%
  filter(IDSchreck == "WSS_2015_04") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [9]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [10])
WSS_2015_04 <- distinct(WSS_2015_04)

Overlap_boar <- rbind(WSS_2015_01_s, WSS_2015_01_a, WSS_2015_03_s, WSS_2015_03_a, WSS_2015_04)
Overlap_boar$date <- as.Date(Overlap_boar$DateTimeUTC)
Overlap_Schreck_boar_after_processing <- ggplot() +
  geom_line(Overlap_boar, mapping = aes(x = date, y = TierName), colour = "darkgreen") +
  geom_line(Locations_adapted, mapping = aes(x = dates, y = id), colour = "blue") +
  theme_bw() +
  scale_x_date(date_breaks = "6 month") +
  labs(y = "Animal Names and SchreckID", x = "Time")

# year 2016
WSS_2016_01 <- distance_16_fl %>%
  filter(IDSchreck == "WSS_2016_01") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [11]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [11])  # delete
WSS_2016_05 <- distance_16_fl %>%
  filter(IDSchreck == "WSS_2016_05") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [12]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [12])  # delete
WSS_2016_06 <- distance_16_fl %>%
  filter(IDSchreck == "WSS_2016_06") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [13]) %>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [13])  # delete
WSS_2016_13 <- distance_16_fl %>%
  filter(IDSchreck == "WSS_2016_13") %>%
  filter(DateTimeUTC >= Schrecklocation$datum_on [14])%>%
  filter(DateTimeUTC <= Schrecklocation$datum_off [14])  # delete

Overlap_boar <- rbind(WSS_2016_01, WSS_2016_05, WSS_2016_06, WSS_2016_13)
Overlap_boar$date <- as.Date(Overlap_boar$DateTimeUTC)
Overlap_Schreck_boar_after_processing <- ggplot() +
  geom_line(Overlap_boar, mapping = aes(x = date, y = TierName), colour = "darkgreen") +
  geom_line(Locations_adapted, mapping = aes(x = dates, y = id), colour = "blue") +
  theme_bw() +
  scale_x_date(date_breaks = "6 month") +
  labs(y = "Animal Names and SchreckID", x = "Time")

# keep working with
WSS_2014_04
WSS_2014_05
WSS_2015_01_s
WSS_2015_01_a
WSS_2015_03_s
WSS_2015_03_a
WSS_2015_04

# Toss the rest because the distance of the wildboar to the schrecks is out of reach of the schreck


# adding data from other dataframes to this file
# WSS 2014_04 ####
WSS_2014_04$Schreck.N <- Schrecklocation$N [1]
WSS_2014_04$Schreck.E <- Schrecklocation$E [1]
WSS_2014_04$Schreck.lat <- Schrecklocation$lat[1]
WSS_2014_04$Schreck.lon <- Schrecklocation$lon [1]
WSS_2014_04 <- left_join(WSS_2014_04, Wildschwein_BE_14, by = c("TierName" = "TierName", "DateTimeUTC" = "DatetimeUTC"))
WSS_2014_04 <- left_join(WSS_2014_04, Loc_adap_suntimes, by = c("date" = "dates", "IDSchreck" = "id", "Schreck.lat" = "lat", "Schreck.lon" = "lon"))
WSS_2014_04$dusk <- with_tz(WSS_2014_04$dusk, "UTC")
WSS_2014_04$dawn <- with_tz(WSS_2014_04$dawn, "UTC")
WSS_2014_04$daynight <- ifelse(WSS_2014_04$DateTimeUTC > WSS_2014_04$dawn & WSS_2014_04$DateTimeUTC < WSS_2014_04$dusk, "Day", "Night")

# Calculating Timelag
WSS_2014_04 <-WSS_2014_04 %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DateTimeUTC),DateTimeUTC, units = "secs")))

# Calculating steplength 
WSS_2014_04 <- WSS_2014_04 %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# Calculating speed
WSS_2014_04 <- WSS_2014_04 %>% 
  group_by(TierID) %>%
  mutate(speed = (steplength/timelag) * 3.6)
WSS_2014_04$TierName <- as.factor(WSS_2014_04$TierName)
levels(WSS_2014_04$TierName)

# Create factors for distance
WSS_2014_04$distance_kat [WSS_2014_04$distance <= 500] <- "near" 
WSS_2014_04$distance_kat [WSS_2014_04$distance <= 1000 & WSS_2014_04$distance > 500] <- "midway"
WSS_2014_04$distance_kat [WSS_2014_04$distance > 1000] <- "far"
WSS_2014_04$distance_kat <- as.factor(WSS_2014_04$distance_kat)

# Seperating Data Day and night
WSS_2014_04_day <- WSS_2014_04 %>%
  filter(daynight == "Day")
WSS_2014_04_night  <- WSS_2014_04 %>%
  filter(daynight == "Night")

# Normalverteilung - Normality
boxplot(WSS_2014_04$distance)
boxplot(WSS_2014_04$speed)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = WSS_2014_04$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = WSS_2014_04$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(WSS_2014_04$speed, WSS_2014_04$distance_kat) # Keine Varianzhomogenität 


# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(WSS_2014_04$speed, WSS_2014_04$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(WSS_2014_04$speed, WSS_2014_04$distance_kat, p.adjust.method = "holm")
#        fern    mittel 
# mittel < 2e-16 -      
# nah    < 2e-16 9.2e-05

# Filtering for individuals to plot the data
WSS_2014_04_Caroline <- WSS_2014_04_night %>%
  filter(TierName == "Caroline")
WSS_2014_04_Isabelle <- WSS_2014_04_night %>%
  filter(TierName == "Isabelle")
WSS_2014_04_Nicole <- WSS_2014_04_night %>%
  filter(TierName == "Nicole")
WSS_2014_04_Sabine <- WSS_2014_04_night %>%
  filter(TierName == "Sabine")
WSS_2014_04_Ueli <- WSS_2014_04_night %>%
  filter(TierName == "Ueli")

# Creating Plot of every individual
WSS_2014_04_Caro_pl <- ggplot() +
  geom_point(data = WSS_2014_04_Caroline, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_04_Caroline$Schreck.E, y0 = WSS_2014_04_Caroline$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_04_Caroline$Schreck.E, y0 = WSS_2014_04_Caroline$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_04_Caroline$Schreck.E, y0 = WSS_2014_04_Caroline$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_04_Caroline, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_04_Caroline, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_04_Caro.png", WSS_2014_04_Caro_pl)

WSS_2014_04_Isa_pl <- ggplot() +
  geom_point(data = WSS_2014_04_Isabelle, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_04_Isabelle$Schreck.E, y0 = WSS_2014_04_Isabelle$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_04_Isabelle$Schreck.E, y0 = WSS_2014_04_Isabelle$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_04_Isabelle$Schreck.E, y0 = WSS_2014_04_Isabelle$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_04_Isabelle, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_04_Isabelle, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_04_Isa.png", WSS_2014_04_Isa_pl)

WSS_2014_04_Nicole_pl <- ggplot() +
  geom_point(data = WSS_2014_04_Nicole, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_04_Nicole$Schreck.E, y0 = WSS_2014_04_Nicole$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_04_Nicole$Schreck.E, y0 = WSS_2014_04_Nicole$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_04_Nicole$Schreck.E, y0 = WSS_2014_04_Nicole$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_04_Nicole, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_04_Nicole, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_04_Nicole.png", WSS_2014_04_Nicole_pl)

WSS_2014_04_Sabine_pl <- ggplot() +
  geom_point(data = WSS_2014_04_Sabine, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_04_Sabine$Schreck.E, y0 = WSS_2014_04_Sabine$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_04_Sabine$Schreck.E, y0 = WSS_2014_04_Sabine$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_04_Sabine$Schreck.E, y0 = WSS_2014_04_Sabine$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_04_Sabine, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_04_Sabine, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_04_Sabine.png", WSS_2014_04_Sabine_pl)

WSS_2014_04_Ueli_pl <- ggplot() +
  geom_point(data = WSS_2014_04_Ueli, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_04_Ueli$Schreck.E, y0 = WSS_2014_04_Ueli$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_04_Ueli$Schreck.E, y0 = WSS_2014_04_Ueli$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_04_Ueli$Schreck.E, y0 = WSS_2014_04_Ueli$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_04_Ueli, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_04_Ueli, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_04_Ueli.png", WSS_2014_04_Ueli_pl)


# WSS 2014_05 ####
WSS_2014_05$Schreck.N <- Schrecklocation$N [2]
WSS_2014_05$Schreck.E <- Schrecklocation$E [2]
WSS_2014_05$Schreck.lat <- Schrecklocation$lat[2]
WSS_2014_05$Schreck.lon <- Schrecklocation$lon [2]
WSS_2014_05 <- left_join(WSS_2014_05, Wildschwein_BE_14, by = c("TierName" = "TierName", "DateTimeUTC" = "DatetimeUTC"))
WSS_2014_05 <- left_join(WSS_2014_05, Loc_adap_suntimes, by = c("date" = "dates", "IDSchreck" = "id", "Schreck.lat" = "lat", "Schreck.lon" = "lon"))
WSS_2014_05$dusk <- with_tz(WSS_2014_05$dusk, "UTC")
WSS_2014_05$dawn <- with_tz(WSS_2014_05$dawn, "UTC")
WSS_2014_05$daynight <- ifelse(WSS_2014_05$DateTimeUTC > WSS_2014_05$dawn & WSS_2014_05$DateTimeUTC < WSS_2014_05$dusk, "Day", "Night")

# Calculating Timelag
WSS_2014_05 <-WSS_2014_05 %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DateTimeUTC),DateTimeUTC, units = "secs")))

# Calculating steplength 
WSS_2014_05 <- WSS_2014_05 %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# Calculating speed
WSS_2014_05 <- WSS_2014_05 %>% 
  group_by(TierID) %>%
  mutate(speed = (steplength/timelag) * 3.6)
WSS_2014_05$TierName <- as.factor(WSS_2014_05$TierName)
levels(WSS_2014_05$TierName)

# Create factors for distance
WSS_2014_05$distance_kat [WSS_2014_05$distance <= 500] <- "near" 
WSS_2014_05$distance_kat [WSS_2014_05$distance <= 1000 & WSS_2014_05$distance > 500] <- "midway"
WSS_2014_05$distance_kat [WSS_2014_05$distance > 1000] <- "far"
WSS_2014_05$distance_kat <- as.factor(WSS_2014_05$distance_kat)

# Seperating Data Day and night
WSS_2014_05_day <- WSS_2014_05 %>%
  filter(daynight == "Day")
WSS_2014_05_night  <- WSS_2014_05 %>%
  filter(daynight == "Night")

# Normalverteilung - Normality
boxplot(WSS_2014_05$distance)
boxplot(WSS_2014_05$speed)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = WSS_2014_05$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = WSS_2014_05$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(WSS_2014_05$speed, WSS_2014_05$distance_kat) # Keine Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(WSS_2014_05$speed, WSS_2014_05$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(WSS_2014_05$speed, WSS_2014_05$distance_kat, p.adjust.method = "holm")

#         far    midway
# midway <2e-16 -     
# near   <2e-16 0.036 

# Filtering for individuals to plot the data
WSS_2014_05_Caroline <- WSS_2014_05_night %>%
  filter(TierName == "Caroline")
WSS_2014_05_Isabelle <- WSS_2014_05_night %>%
  filter(TierName == "Isabelle")
WSS_2014_05_Sabine <- WSS_2014_05_night %>%
  filter(TierName == "Sabine")
WSS_2014_05_Ueli <- WSS_2014_05_night %>%
  filter(TierName == "Ueli")

# Creating Plot of every individual
WSS_2014_05_Caro_pl <- ggplot() +
  geom_point(data = WSS_2014_05_Caroline, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_05_Caroline$Schreck.E, y0 = WSS_2014_05_Caroline$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_05_Caroline$Schreck.E, y0 = WSS_2014_05_Caroline$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_05_Caroline$Schreck.E, y0 = WSS_2014_05_Caroline$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_05_Caroline, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_05_Caroline, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_05_Caro.png", WSS_2014_05_Caro_pl)

WSS_2014_05_Isa_pl <- ggplot() +
  geom_point(data = WSS_2014_05_Isabelle, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_05_Isabelle$Schreck.E, y0 = WSS_2014_05_Isabelle$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_05_Isabelle$Schreck.E, y0 = WSS_2014_05_Isabelle$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_05_Isabelle$Schreck.E, y0 = WSS_2014_05_Isabelle$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_05_Isabelle, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_05_Isabelle, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_05_Isa.png", WSS_2014_05_Isa_pl)

WSS_2014_05_Sabine_pl <- ggplot() +
  geom_point(data = WSS_2014_05_Sabine, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_05_Sabine$Schreck.E, y0 = WSS_2014_05_Sabine$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_05_Sabine$Schreck.E, y0 = WSS_2014_05_Sabine$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_05_Sabine$Schreck.E, y0 = WSS_2014_05_Sabine$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_05_Sabine, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_05_Sabine, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_05_Sabine.png", WSS_2014_05_Sabine_pl)

WSS_2014_05_Ueli_pl <- ggplot() +
  geom_point(data = WSS_2014_05_Ueli, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2014_05_Ueli$Schreck.E, y0 = WSS_2014_05_Ueli$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2014_05_Ueli$Schreck.E, y0 = WSS_2014_05_Ueli$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2014_05_Ueli$Schreck.E, y0 = WSS_2014_05_Ueli$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2014_05_Ueli, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2014_05_Ueli, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2014_05_Ueli.png", WSS_2014_05_Ueli_pl)

# WSS 2015_01_s ####
WSS_2015_01_s$Schreck.N <- Schrecklocation$N [5]
WSS_2015_01_s$Schreck.E <- Schrecklocation$E [5]
WSS_2015_01_s$Schreck.lat <- Schrecklocation$lat[5]
WSS_2015_01_s$Schreck.lon <- Schrecklocation$lon [5]
WSS_2015_01_s <- left_join(WSS_2015_01_s, Wildschwein_BE_15, by = c("TierName" = "TierName", "DateTimeUTC" = "DatetimeUTC"))
WSS_2015_01_s <- left_join(WSS_2015_01_s, Loc_adap_suntimes, by = c("date" = "dates", "IDSchreck" = "id", "Schreck.lat" = "lat", "Schreck.lon" = "lon"))
WSS_2015_01_s$dusk <- with_tz(WSS_2015_01_s$dusk, "UTC")
WSS_2015_01_s$dawn <- with_tz(WSS_2015_01_s$dawn, "UTC")
WSS_2015_01_s$daynight <- ifelse(WSS_2015_01_s$DateTimeUTC > WSS_2015_01_s$dawn & WSS_2015_01_s$DateTimeUTC < WSS_2015_01_s$dusk, "Day", "Night")

# Calculating Timelag
WSS_2015_01_s <-WSS_2015_01_s %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DateTimeUTC),DateTimeUTC, units = "secs")))

# Calculating steplength 
WSS_2015_01_s <- WSS_2015_01_s %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# Calculating speed
WSS_2015_01_s <- WSS_2015_01_s %>% 
  group_by(TierID) %>%
  mutate(speed = (steplength/timelag) * 3.6)
WSS_2015_01_s$TierName <- as.factor(WSS_2015_01_s$TierName)
levels(WSS_2015_01_s$TierName)

# Create factors for distance
WSS_2015_01_s$distance_kat [WSS_2015_01_s$distance <= 500] <- "near" 
WSS_2015_01_s$distance_kat [WSS_2015_01_s$distance <= 1000 & WSS_2015_01_s$distance > 500] <- "midway"
WSS_2015_01_s$distance_kat [WSS_2015_01_s$distance > 1000] <- "far"
WSS_2015_01_s$distance_kat <- as.factor(WSS_2015_01_s$distance_kat)

# Seperating Data Day and night
WSS_2015_01_s_day <- WSS_2015_01_s %>%
  filter(daynight == "Day")
WSS_2015_01_s_night  <- WSS_2015_01_s %>%
  filter(daynight == "Night")

# Normalverteilung - Normality
boxplot(WSS_2015_01_s$distance)
boxplot(WSS_2015_01_s$speed)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = WSS_2015_01_s$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = WSS_2015_01_s$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(WSS_2015_01_s$speed, WSS_2015_01_s$distance_kat) # Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(WSS_2015_01_s$speed, WSS_2015_01_s$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(WSS_2015_01_s$speed, WSS_2015_01_s$distance_kat, p.adjust.method = "holm")

#        far     midway 
# midway 1.4e-06 -      
# near   0.00190 0.00013

# Filtering for individuals to plot the data
WSS_2015_01_s_Caroline <- WSS_2015_01_s_night %>%
  filter(TierName == "Caroline")
WSS_2015_01_s_Claude <- WSS_2015_01_s_night %>%
  filter(TierName == "Claude")
WSS_2015_01_s_Olga <- WSS_2015_01_s_night %>%
  filter(TierName == "Olga")
WSS_2015_01_s_Ruth <- WSS_2015_01_s_night %>%
  filter(TierName == "Ruth")
WSS_2015_01_s_Rosa <- WSS_2015_01_s_night %>%
  filter(TierName == "Rosa")
WSS_2015_01_s_Sabine <- WSS_2015_01_s_night %>%
  filter(TierName == "Sabine")
WSS_2015_01_s_Franz <- WSS_2015_01_s_night %>%
  filter(TierName =="Franz")

# Creating Plot of every individual
WSS_2015_01_s_Caroline_pl <- ggplot() +
  geom_point(data =WSS_2015_01_s_Caroline, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_s_Caroline$Schreck.E, y0 = WSS_2015_01_s_Caroline$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_s_Caroline$Schreck.E, y0 = WSS_2015_01_s_Caroline$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_s_Caroline$Schreck.E, y0 = WSS_2015_01_s_Caroline$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_s_Caroline, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_s_Caroline, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_s_Caroline.png", WSS_2015_01_s_Caroline_pl)

WSS_2015_01_s_Claude_pl <- ggplot() +
  geom_point(data = WSS_2015_01_s_Claude, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_s_Claude$Schreck.E, y0 = WSS_2015_01_s_Claude$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_s_Claude$Schreck.E, y0 = WSS_2015_01_s_Claude$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_s_Claude$Schreck.E, y0 = WSS_2015_01_s_Claude$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_s_Claude, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_s_Claude, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_s_Claude.png", WSS_2015_01_s_Claude_pl)

WSS_2015_01_s_Olga_pl <- ggplot() +
  geom_point(data = WSS_2015_01_s_Olga, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_s_Olga$Schreck.E, y0 = WSS_2015_01_s_Olga$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_s_Olga$Schreck.E, y0 = WSS_2015_01_s_Olga$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_s_Olga$Schreck.E, y0 = WSS_2015_01_s_Olga$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_s_Olga, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_s_Olga, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_s_Olga.png", WSS_2015_01_s_Olga_pl)

WSS_2015_01_s_Ruth_pl <- ggplot() +
  geom_point(data = WSS_2015_01_s_Ruth, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_s_Ruth$Schreck.E, y0 = WSS_2015_01_s_Ruth$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_s_Ruth$Schreck.E, y0 = WSS_2015_01_s_Ruth$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_s_Ruth$Schreck.E, y0 = WSS_2015_01_s_Ruth$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_s_Ruth, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_s_Ruth, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_s_Ruth.png", WSS_2015_01_s_Ruth_pl)

WSS_2015_01_s_Rosa_pl <- ggplot() +
  geom_point(data = WSS_2015_01_s_Rosa, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_s_Rosa$Schreck.E, y0 = WSS_2015_01_s_Rosa$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_s_Rosa$Schreck.E, y0 = WSS_2015_01_s_Rosa$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_s_Rosa$Schreck.E, y0 = WSS_2015_01_s_Rosa$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_s_Rosa, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_s_Rosa, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_s_Rosa.png", WSS_2015_01_s_Rosa_pl)

WSS_2015_01_s_Sabine_pl <- ggplot() +
  geom_point(data = WSS_2015_01_s_Sabine, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_s_Sabine$Schreck.E, y0 = WSS_2015_01_s_Sabine$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_s_Sabine$Schreck.E, y0 = WSS_2015_01_s_Sabine$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_s_Sabine$Schreck.E, y0 = WSS_2015_01_s_Sabine$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_s_Sabine, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_s_Sabine, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_s_Sabine.png", WSS_2015_01_s_Sabine_pl)

WSS_2015_01_s_Franz_pl <- ggplot() +
  geom_point(data = WSS_2015_01_s_Franz, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_s_Franz$Schreck.E, y0 = WSS_2015_01_s_Franz$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_s_Franz$Schreck.E, y0 = WSS_2015_01_s_Franz$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_s_Franz$Schreck.E, y0 = WSS_2015_01_s_Franz$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_s_Franz, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_s_Franz, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_s_Franz.png", WSS_2015_01_s_Franz_pl)

# WSS_2015_01_a ####
WSS_2015_01_a$Schreck.N <- Schrecklocation$N [6]
WSS_2015_01_a$Schreck.E <- Schrecklocation$E [6]
WSS_2015_01_a$Schreck.lat <- Schrecklocation$lat[6]
WSS_2015_01_a$Schreck.lon <- Schrecklocation$lon [6]
WSS_2015_01_a <- left_join(WSS_2015_01_a, Wildschwein_BE_15, by = c("TierName" = "TierName", "DateTimeUTC" = "DatetimeUTC"))
WSS_2015_01_a <- left_join(WSS_2015_01_a, Loc_adap_suntimes, by = c("date" = "dates", "IDSchreck" = "id", "Schreck.lat" = "lat", "Schreck.lon" = "lon"))
WSS_2015_01_a$dusk <- with_tz(WSS_2015_01_a$dusk, "UTC")
WSS_2015_01_a$dawn <- with_tz(WSS_2015_01_a$dawn, "UTC")
WSS_2015_01_a$daynight <- ifelse(WSS_2015_01_a$DateTimeUTC > WSS_2015_01_a$dawn & WSS_2015_01_a$DateTimeUTC < WSS_2015_01_a$dusk, "Day", "Night")

# Calculating Timelag
WSS_2015_01_a <-WSS_2015_01_a %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DateTimeUTC),DateTimeUTC, units = "secs")))
WSS_2015_01_a <- WSS_2015_01_a %>%
  filter(timelag > 0)
# Calculating steplength 
WSS_2015_01_a <- WSS_2015_01_a %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# Calculating speed
WSS_2015_01_a <- WSS_2015_01_a %>% 
  group_by(TierID) %>%
  mutate(speed = (steplength/timelag) * 3.6)
WSS_2015_01_a$TierName <- as.factor(WSS_2015_01_a$TierName)
levels(WSS_2015_01_a$TierName)

# Create factors for distance
WSS_2015_01_a$distance_kat [WSS_2015_01_a$distance <= 500] <- "near" 
WSS_2015_01_a$distance_kat [WSS_2015_01_a$distance <= 1000 & WSS_2015_01_a$distance > 500] <- "midway"
WSS_2015_01_a$distance_kat [WSS_2015_01_a$distance > 1000] <- "far"
WSS_2015_01_a$distance_kat <- as.factor(WSS_2015_01_a$distance_kat)

# Seperating Data Day and night
WSS_2015_01_a_day <- WSS_2015_01_a %>%
  filter(daynight == "Day")
WSS_2015_01_a_night  <- WSS_2015_01_a %>%
  filter(daynight == "Night")

# Normalverteilung - Normality
boxplot(WSS_2015_01_a$distance)
boxplot(WSS_2015_01_a$speed)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = WSS_2015_01_a$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = WSS_2015_01_a$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(WSS_2015_01_a$speed, WSS_2015_01_a$distance_kat) # keine Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(WSS_2015_01_a$speed, WSS_2015_01_a$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(WSS_2015_01_a$speed, WSS_2015_01_a$distance_kat, p.adjust.method = "holm")

#        far     midway 
#midway < 2e-16 -      
#near   < 2e-16 6.9e-08

# Filtering for individuals to plot the data
WSS_2015_01_a_Caroline <- WSS_2015_01_a_night %>%
  filter(TierName == "Caroline")
WSS_2015_01_a_Franz <- WSS_2015_01_a_night %>%
  filter(TierName == "Franz")
WSS_2015_01_a_Olga <- WSS_2015_01_a_night %>%
  filter(TierName == "Olga")
WSS_2015_01_a_Ruth <- WSS_2015_01_a_night %>%
  filter(TierName == "Ruth")
WSS_2015_01_a_Sabine <- WSS_2015_01_a_night %>%
  filter(TierName == "Sabine")

# Creating Plot of every individual
WSS_2015_01_a_Caroline_pl <- ggplot() +
  geom_point(data =WSS_2015_01_a_Caroline, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_a_Caroline$Schreck.E, y0 = WSS_2015_01_a_Caroline$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_a_Caroline$Schreck.E, y0 = WSS_2015_01_a_Caroline$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_a_Caroline$Schreck.E, y0 = WSS_2015_01_a_Caroline$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Caroline, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_a_Caroline, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_a_Caroline.png", WSS_2015_01_a_Caroline_pl)

WSS_2015_01_a_Franz_pl <- ggplot() +
  geom_point(data = WSS_2015_01_a_Franz, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_a_Franz$Schreck.E, y0 = WSS_2015_01_a_Franz$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_a_Franz$Schreck.E, y0 = WSS_2015_01_a_Franz$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_a_Franz$Schreck.E, y0 = WSS_2015_01_a_Franz$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Franz, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_a_Franz, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_a_Franz.png", WSS_2015_01_a_Franz_pl)

WSS_2015_01_a_Olga_pl <- ggplot() +
  geom_point(data = WSS_2015_01_a_Olga, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_a_Olga$Schreck.E, y0 = WSS_2015_01_a_Olga$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_a_Olga$Schreck.E, y0 = WSS_2015_01_a_Olga$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_a_Olga$Schreck.E, y0 = WSS_2015_01_a_Olga$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Olga, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_a_Olga, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_a_Olga.png", WSS_2015_01_a_Olga_pl)

WSS_2015_01_a_Ruth_pl <- ggplot() +
  geom_point(data = WSS_2015_01_s_Ruth, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_a_Ruth$Schreck.E, y0 = WSS_2015_01_a_Ruth$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_a_Ruth$Schreck.E, y0 = WSS_2015_01_a_Ruth$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_a_Ruth$Schreck.E, y0 = WSS_2015_01_a_Ruth$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Ruth, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_a_Ruth, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_a_Ruth.png", WSS_2015_01_a_Ruth_pl)

WSS_2015_01_a_Sabine_pl <- ggplot() +
  geom_point(data = WSS_2015_01_a_Sabine, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_01_a_Sabine$Schreck.E, y0 = WSS_2015_01_a_Sabine$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_01_a_Sabine$Schreck.E, y0 = WSS_2015_01_a_Sabine$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_01_a_Sabine$Schreck.E, y0 = WSS_2015_01_a_Sabine$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Sabine, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_01_a_Sabine, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_01_a_Sabine.png", WSS_2015_01_a_Sabine_pl)

# WSS_2015_03_s ####
WSS_2015_03_s$Schreck.N <- Schrecklocation$N [7]
WSS_2015_03_s$Schreck.E <- Schrecklocation$E [7]
WSS_2015_03_s$Schreck.lat <- Schrecklocation$lat[7]
WSS_2015_03_s$Schreck.lon <- Schrecklocation$lon [7]
WSS_2015_03_s <- left_join(WSS_2015_03_s, Wildschwein_BE_15, by = c("TierName" = "TierName", "DateTimeUTC" = "DatetimeUTC"))
WSS_2015_03_s <- left_join(WSS_2015_03_s, Loc_adap_suntimes, by = c("date" = "dates", "IDSchreck" = "id", "Schreck.lat" = "lat", "Schreck.lon" = "lon"))
WSS_2015_03_s$dusk <- with_tz(WSS_2015_03_s$dusk, "UTC")
WSS_2015_03_s$dawn <- with_tz(WSS_2015_03_s$dawn, "UTC")
WSS_2015_03_s$daynight <- ifelse(WSS_2015_03_s$DateTimeUTC > WSS_2015_03_s$dawn & WSS_2015_03_s$DateTimeUTC < WSS_2015_03_s$dusk, "Day", "Night")

# Calculating Timelag
WSS_2015_03_s <-WSS_2015_03_s %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DateTimeUTC),DateTimeUTC, units = "secs")))
WSS_2015_03_s <- WSS_2015_03_s %>%
  filter(timelag > 0 | timelag < 15000)
# Calculating steplength 
WSS_2015_03_s <- WSS_2015_03_s %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# Calculating speed
WSS_2015_03_s <- WSS_2015_03_s %>% 
  group_by(TierID) %>%
  mutate(speed = (steplength/timelag) * 3.6)
WSS_2015_03_s$TierName <- as.factor(WSS_2015_03_s$TierName)
levels(WSS_2015_03_s$TierName)

# Create factors for distance
WSS_2015_03_s$distance_kat [WSS_2015_03_s$distance <= 500] <- "near" 
WSS_2015_03_s$distance_kat [WSS_2015_03_s$distance <= 1000 & WSS_2015_03_s$distance > 500] <- "midway"
WSS_2015_03_s$distance_kat [WSS_2015_03_s$distance > 1000] <- "far"
WSS_2015_03_s$distance_kat <- as.factor(WSS_2015_03_s$distance_kat)

# Seperating Data Day and night
WSS_2015_03_s_day <- WSS_2015_03_s %>%
  filter(daynight == "Day")
WSS_2015_03_s_night  <- WSS_2015_03_s %>%
  filter(daynight == "Night")

# Normalverteilung - Normality
boxplot(WSS_2015_03_s$distance)
boxplot(WSS_2015_03_s$speed)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = WSS_2015_03_s$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = WSS_2015_03_s$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(WSS_2015_03_s$speed, WSS_2015_03_s$distance_kat) # keine Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(WSS_2015_03_s$speed, WSS_2015_03_s$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(WSS_2015_03_s$speed, WSS_2015_03_s$distance_kat, p.adjust.method = "holm")

#         far     midway 
# midway 1.3e-10 -      
# near   < 2e-16 6.7e-08

# Filtering for individuals to plot the data
WSS_2015_03_s_Ruth <- WSS_2015_03_s_night %>%
  filter(TierName == "Ruth")
WSS_2015_03_s_Sabine <- WSS_2015_03_s_night %>%
  filter(TierName == "Sabine")

# Creating Plot of every individual

WSS_2015_03_s_Ruth_pl <- ggplot() +
  geom_point(data = WSS_2015_03_s_Ruth, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_03_s_Ruth$Schreck.E, y0 = WSS_2015_03_s_Ruth$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_03_s_Ruth$Schreck.E, y0 = WSS_2015_03_s_Ruth$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_03_s_Ruth$Schreck.E, y0 = WSS_2015_03_s_Ruth$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Ruth, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_03_s_Ruth, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_03_s_Ruth.png", WSS_2015_03_s_Ruth_pl)

WSS_2015_03_s_Sabine_pl <- ggplot() +
  geom_point(data = WSS_2015_03_s_Sabine, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_03_s_Sabine$Schreck.E, y0 = WSS_2015_03_s_Sabine$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_03_s_Sabine$Schreck.E, y0 = WSS_2015_03_s_Sabine$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_03_s_Sabine$Schreck.E, y0 = WSS_2015_03_s_Sabine$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_03_s_Sabine, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_03_s_Sabine, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_03_s_Sabine.png", WSS_2015_03_s_Sabine_pl)

# WSS_2015_03_a ####
WSS_2015_03_a$Schreck.N <- Schrecklocation$N [8]
WSS_2015_03_a$Schreck.E <- Schrecklocation$E [8]
WSS_2015_03_a$Schreck.lat <- Schrecklocation$lat[8]
WSS_2015_03_a$Schreck.lon <- Schrecklocation$lon [8]
WSS_2015_03_a <- left_join(WSS_2015_03_a, Wildschwein_BE_15, by = c("TierName" = "TierName", "DateTimeUTC" = "DatetimeUTC"))
WSS_2015_03_a <- left_join(WSS_2015_03_a, Loc_adap_suntimes, by = c("date" = "dates", "IDSchreck" = "id", "Schreck.lat" = "lat", "Schreck.lon" = "lon"))
WSS_2015_03_a$dusk <- with_tz(WSS_2015_03_a$dusk, "UTC")
WSS_2015_03_a$dawn <- with_tz(WSS_2015_03_a$dawn, "UTC")
WSS_2015_03_a$daynight <- ifelse(WSS_2015_03_a$DateTimeUTC > WSS_2015_03_a$dawn & WSS_2015_03_a$DateTimeUTC < WSS_2015_03_a$dusk, "Day", "Night")
WSS_2015_03_a <- distinct(WSS_2015_03_a)

# Calculating Timelag
WSS_2015_03_a <-WSS_2015_03_a %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DateTimeUTC),DateTimeUTC, units = "secs")))
WSS_2015_03_a <- WSS_2015_03_a %>%
  filter(timelag > 0 | timelag < 15000)
# Calculating steplength 
WSS_2015_03_a <- WSS_2015_03_a %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# Calculating speed
WSS_2015_03_a <- WSS_2015_03_a %>% 
  group_by(TierID) %>%
  mutate(speed = (steplength/timelag) * 3.6)
WSS_2015_03_a$TierName <- as.factor(WSS_2015_03_a$TierName)
levels(WSS_2015_03_a$TierName)

# Create factors for distance
WSS_2015_03_a$distance_kat [WSS_2015_03_a$distance <= 500] <- "near" 
WSS_2015_03_a$distance_kat [WSS_2015_03_a$distance <= 1000 & WSS_2015_03_a$distance > 500] <- "midway"
WSS_2015_03_a$distance_kat [WSS_2015_03_a$distance > 1000] <- "far"
WSS_2015_03_a$distance_kat <- as.factor(WSS_2015_03_a$distance_kat)

# Seperating Data Day and night
WSS_2015_03_a_day <- WSS_2015_03_a %>%
  filter(daynight == "Day")
WSS_2015_03_a_night  <- WSS_2015_03_a %>%
  filter(daynight == "Night")

# Normalverteilung - Normality
boxplot(WSS_2015_03_a$distance)
boxplot(WSS_2015_03_a$speed)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = WSS_2015_03_a$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = WSS_2015_03_a$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(WSS_2015_03_a$speed, WSS_2015_03_a$distance_kat) # keine Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(WSS_2015_03_a$speed, WSS_2015_03_a$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(WSS_2015_03_a$speed, WSS_2015_03_a$distance_kat, p.adjust.method = "holm")

#        far     midway 
# midway 0.0028  -      
# near   < 2e-16 8.3e-12


# Filtering for individuals to plot the data
WSS_2015_03_a_Caroline <- WSS_2015_03_a_night %>%
  filter(TierName == "Caroline")
WSS_2015_03_a_Olga <- WSS_2015_03_a_night %>%
  filter(TierName == "Olga")
WSS_2015_03_a_Ruth <- WSS_2015_03_a_night %>%
  filter(TierName == "Ruth")
WSS_2015_03_a_Sabine <- WSS_2015_03_a_night %>%
  filter(TierName == "Sabine")

# Creating Plot of every individual

WSS_2015_03_a_Ruth_pl <- ggplot() +
  geom_point(data = WSS_2015_03_a_Ruth, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_03_a_Ruth$Schreck.E, y0 = WSS_2015_03_a_Ruth$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_03_a_Ruth$Schreck.E, y0 = WSS_2015_03_a_Ruth$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_03_a_Ruth$Schreck.E, y0 = WSS_2015_03_a_Ruth$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Ruth, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_03_a_Ruth, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_03_a_Ruth.png", WSS_2015_03_a_Ruth_pl)

WSS_2015_03_a_Sabine_pl <- ggplot() +
  geom_point(data = WSS_2015_03_a_Sabine, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_03_a_Sabine$Schreck.E, y0 = WSS_2015_03_a_Sabine$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_03_a_Sabine$Schreck.E, y0 = WSS_2015_03_a_Sabine$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_03_a_Sabine$Schreck.E, y0 = WSS_2015_03_a_Sabine$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_03_a_Sabine, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_03_a_Sabine, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_03_a_Sabine.png", WSS_2015_03_a_Sabine_pl)

WSS_2015_03_a_Caroline_pl <- ggplot() +
  geom_point(data = WSS_2015_03_a_Caroline, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_03_a_Caroline$Schreck.E, y0 = WSS_2015_03_a_Caroline$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_03_a_Caroline$Schreck.E, y0 = WSS_2015_03_a_Caroline$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_03_a_Caroline$Schreck.E, y0 = WSS_2015_03_a_Caroline$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_01_a_Caroline, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_03_a_Caroline, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_03_a_Caroline.png", WSS_2015_03_a_Caroline_pl)

WSS_2015_03_a_Olga_pl <- ggplot() +
  geom_point(data = WSS_2015_03_a_Olga, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_03_a_Olga$Schreck.E, y0 = WSS_2015_03_a_Olga$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_03_a_Olga$Schreck.E, y0 = WSS_2015_03_a_Olga$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_03_a_Olga$Schreck.E, y0 = WSS_2015_03_a_Olga$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_03_a_Olga, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_03_a_Olga, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_03_a_Olga.png", WSS_2015_03_a_Olga_pl)

# WSS_2015_04 ####
WSS_2015_04$Schreck.N <- Schrecklocation$N [9]
WSS_2015_04$Schreck.E <- Schrecklocation$E [9]
WSS_2015_04$Schreck.lat <- Schrecklocation$lat[9]
WSS_2015_04$Schreck.lon <- Schrecklocation$lon [9]
WSS_2015_04 <- left_join(WSS_2015_04, Wildschwein_BE_15, by = c("TierName" = "TierName", "DateTimeUTC" = "DatetimeUTC"))
WSS_2015_04 <- left_join(WSS_2015_04, Loc_adap_suntimes, by = c("date" = "dates", "IDSchreck" = "id", "Schreck.lat" = "lat", "Schreck.lon" = "lon"))
WSS_2015_04$dusk <- with_tz(WSS_2015_04$dusk, "UTC")
WSS_2015_04$dawn <- with_tz(WSS_2015_04$dawn, "UTC")
WSS_2015_04$daynight <- ifelse(WSS_2015_04$DateTimeUTC > WSS_2015_04$dawn & WSS_2015_04$DateTimeUTC < WSS_2015_04$dusk, "Day", "Night")
WSS_2015_04 <- distinct(WSS_2015_04)

# Calculating Timelag
WSS_2015_04 <-WSS_2015_04 %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DateTimeUTC),DateTimeUTC, units = "secs")))
WSS_2015_04 <- WSS_2015_04 %>%
  filter(timelag > 0 | timelag < 15000)
# Calculating steplength 
WSS_2015_04 <- WSS_2015_04 %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# Calculating speed
WSS_2015_04 <- WSS_2015_04 %>% 
  group_by(TierID) %>%
  mutate(speed = (steplength/timelag) * 3.6)
WSS_2015_04$TierName <- as.factor(WSS_2015_04$TierName)
levels(WSS_2015_04$TierName)

# Create factors for distance
WSS_2015_04$distance_kat [WSS_2015_04$distance <= 500] <- "near" 
WSS_2015_04$distance_kat [WSS_2015_04$distance <= 1000 & WSS_2015_04$distance > 500] <- "midway"
WSS_2015_04$distance_kat [WSS_2015_04$distance > 1000] <- "far"
WSS_2015_04$distance_kat <- as.factor(WSS_2015_04$distance_kat)

# Seperating Data Day and night
WSS_2015_04_day <- WSS_2015_04 %>%
  filter(daynight == "Day")
WSS_2015_04_night  <- WSS_2015_04 %>%
  filter(daynight == "Night")

# Normalverteilung - Normality
boxplot(WSS_2015_04$distance)
boxplot(WSS_2015_04$speed)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = WSS_2015_04$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = WSS_2015_04$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(WSS_2015_04$speed, WSS_2015_04$distance_kat) # keine Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(WSS_2015_04$speed, WSS_2015_04$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(WSS_2015_04$speed, WSS_2015_04$distance_kat, p.adjust.method = "holm")

#        far     midway 
# midway 1.9e-07 -      
# near   4.9e-11 0.00084

# Filtering for individuals to plot the data
WSS_2015_04_Caroline <- WSS_2015_04_night %>%
  filter(TierName == "Caroline")
WSS_2015_04_Olga <- WSS_2015_04_night %>%
  filter(TierName == "Olga")
WSS_2015_04_Ruth <- WSS_2015_04_night %>%
  filter(TierName == "Ruth")
WSS_2015_04_Sabine <- WSS_2015_04_night %>%
  filter(TierName == "Sabine")

# Creating Plot of every individual

WSS_2015_04_Ruth_pl <- ggplot() +
  geom_point(data = WSS_2015_04_Ruth, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_04_Ruth$Schreck.E, y0 = WSS_2015_04_Ruth$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_04_Ruth$Schreck.E, y0 = WSS_2015_04_Ruth$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_04_Ruth$Schreck.E, y0 = WSS_2015_04_Ruth$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_04_Ruth, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_04_Ruth, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_04_Ruth.png", WSS_2015_04_Ruth_pl)

WSS_2015_04_Sabine_pl <- ggplot() +
  geom_point(data = WSS_2015_04_Sabine, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_04_Sabine$Schreck.E, y0 = WSS_2015_04_Sabine$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_04_Sabine$Schreck.E, y0 = WSS_2015_04_Sabine$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_04_Sabine$Schreck.E, y0 = WSS_2015_04_Sabine$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_04_Sabine, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_04_Sabine, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_04_Sabine.png", WSS_2015_04_Sabine_pl)

WSS_2015_04_Caroline_pl <- ggplot() +
  geom_point(data = WSS_2015_04_Caroline, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_04_Caroline$Schreck.E, y0 = WSS_2015_04_Caroline$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_04_Caroline$Schreck.E, y0 = WSS_2015_04_Caroline$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_04_Caroline$Schreck.E, y0 = WSS_2015_04_Caroline$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_04_Caroline, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_04_Caroline, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_04_Caroline.png", WSS_2015_04_Caroline_pl)

WSS_2015_04_Olga_pl <- ggplot() +
  geom_point(data = WSS_2015_04_Olga, aes(x= E, y=N)) +
  geom_circle(aes(x0 = WSS_2015_04_Olga$Schreck.E, y0 = WSS_2015_04_Olga$Schreck.N, r = 1500), colour = "deepskyblue4") +
  geom_circle(aes(x0 = WSS_2015_04_Olga$Schreck.E, y0 = WSS_2015_04_Olga$Schreck.N, r = 1000), colour = "deepskyblue2") +
  geom_circle(aes(x0 = WSS_2015_04_Olga$Schreck.E, y0 = WSS_2015_04_Olga$Schreck.N, r = 500), colour = "deepskyblue")  +
  geom_path(data = WSS_2015_04_Olga, aes(x=E, y=N, colour = speed)) +
  scale_colour_gradientn(colours = terrain.colors(6), limits = c(0, 5), breaks = c(0,1,2,3,4,5)) +
  geom_point(data = WSS_2015_04_Olga, aes(x=Schreck.E, y=Schreck.N), colour = "blue") +
  theme_bw()
ggsave("WSS_2015_04_Olga.png", WSS_2015_04_Olga_pl)


# Overall ####
Overall <- rbind(WSS_2014_04, WSS_2014_05, WSS_2015_01_s, WSS_2015_01_a, WSS_2015_03_s, WSS_2015_03_a, WSS_2015_04)

# shapiro.test(WSS_2014_04$distance)  --> Shapiro doesn't work because sample size is too big
ks.test(x = Overall$distance, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung
ks.test(x = Overall$speed, y = "pnorm", alternative = "two.sided") # Keine Normalverteilung

# Varianzhomogenität testen
leveneTest(Overall$speed, Overall$distance_kat) # keine Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(Overall$speed, Overall$distance_kat) # Alpha < 0.001
pairwise.wilcox.test(Overall$speed, Overall$distance_kat, p.adjust.method = "holm")


Overall$distance_kat <- factor(Overall$distance_kat, levels = c("near", "midway", "far"))
boxplot(Overall$speed~Overall$distance_kat)

Overall_dis_cat <- ggplot(Overall, aes(x = distance_kat, y = speed)) +
  geom_boxplot() +
  theme_bw() +
  stat_compare_means(test="kruskal.test", label.x = 0.7, label.y = -0.5) +
  stat_compare_means(test="pairwise.wilcox.test", comparisons = list(c("near", "midway"), c("midway", "far"), c("far", "near")), p.adjust.methods = "holm", hide.ns = FALSE) +
  labs(y = "Speed", x = "Distance categories" )
ggsave("Overall_dist_cat.png", Overall_dis_cat)
# Unterschiede Tag Nacht
# Varianzhomogenität testen
leveneTest(Overall$speed, Overall$daynight) # keine Varianzhomogenität 

# As Normality and Homoscedasticity are not given we use a Kruskal Wallis test. 
kruskal.test(Overall$speed, Overall$daynight) # Alpha < 0.001
Overall_daynight <- ggplot(Overall, aes(x = daynight, y = speed))+
  geom_boxplot() +
  theme_bw() +
  stat_compare_means(test="kruskal.test", label.x = 0.7, label.y = -0.5) +
  labs(y = "Speed", x = "Distance categories" )
ggsave("Overall_daynight.png", Overall_daynight)

# Libraries to use
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(lubridate)    # To handle dates and times
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData") # getting data from R Package
library(ComputationalMovementAnalysisData) #Wild boar data is on that package
library(sf) # encoding spatial vector data 

# Getting the data from the package
Wildschwein_BE <- wildschwein_BE
Wildschwein_meta <- wildschwein_metadata
Wildschwein_overlap_temp <- wildschwein_overlap_temp
Schreckagenda <- schreck_agenda    # Was bedeutet Phase in der Schreckagenda?
Schrecklocation <- schreck_locations

# Joining Schreckagenda with Schreckagenda with a left join
Schrecklocation <- left_join(Schrecklocation, Schreckagenda, by = "id")

Schrecklocation_sf <- st_as_sf(Schrecklocation, 
                               coords = c("E", "N"),
                               crs = 4326)

Schrecklocation_sf$jagddruck <- factor(Schrecklocation$jagddruck, levels =c("gering", "mittel", "hoch"))

# Plot of all Wildschwein Schrecks
ggplot(Schrecklocation_sf, aes(colour=region, shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  geom_sf(data = world)

# Separation of dataframe according to region
Schrecklocation_elfingen <- Schrecklocation_sf %>%
  filter(region == "elfingen")

Schrecklocation_fanel <- Schrecklocation_sf %>%
  filter(region == "fanel")

Schrecklocation_zuzgen <- Schrecklocation_sf %>%
  filter(region == "zuzgen")

Schrecklocation_felsenau <- Schrecklocation_sf %>%
  filter(region == "felsenau")
  
Schrecklocation_buch <- Schrecklocation_sf %>%
  filter(region == "buch am irchel")

Schrecklocation_hagneck <- Schrecklocation_sf %>%
  filter(region == "hagneck")

# Plotting Schecks accoring to region
ggplot(Schrecklocation_elfingen, aes(shape = jagddruck, colour = )) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))

ggplot(Schrecklocation_fanel, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))

ggplot(Schrecklocation_zuzgen, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))

ggplot(Schrecklocation_felsenau, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))

ggplot(Schrecklocation_buch, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))

ggplot(Schrecklocation_hagneck, aes(shape = jagddruck)) +
  geom_sf(alpha = 0.4) +
  scale_shape_manual(values = c("gering" = 15, "mittel" = 16, "hoch" = 17))


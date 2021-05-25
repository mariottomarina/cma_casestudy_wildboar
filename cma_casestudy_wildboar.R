# Libraries to use
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(lubridate)    # To handle dates and times


## Import Data

schreckagenda <- read_delim("schreckagenda_2014-2017.csv", "," )
data_all <- read_delim("wildschwein_BE_all_raw.csv", ",")
wildboar_meta <- read_delim("Wildschweine_2018-04-17_METADATEN.csv", ";")
wildboar_meta <- wildboar_meta [-c(47:52), -c(33:59)]


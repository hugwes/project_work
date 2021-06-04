###################################################################
# LIBRARIES
library(tidyverse)    # diverse packages
library(readr)        # import tabular data (csv)
library(dplyr)        # manipulate data
library(ggplot2)      # visualize data
library(sf)           # spatial vector data (shape file)
library(terra)        # raster data
library(lubridate)    # dates and times
library(zoo)          # moving window functions
library(tmap)         # thematic map
library(recurse)      # revisitation metrics for trajectory data

###################################################################
# IMPORT DATA FROM GITHUB
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")
library(ComputationalMovementAnalysisData)
head(wildschwein_BE)
w <- wildschwein_BE

# WRITE CSV
write.csv(w, "Data/wildschwein.csv", row.names=FALSE)

# IMPORT DATA FROM MY DATA
wildschwein <- read_delim("Data/wildschwein.csv", ",")

###################################################################
# IMPORT MAPS
karte <- terra::rast("Data/pk100_BE_2056.tiff")
plotRGB(karte)

vegetationshoehe <- terra::rast("Data/vegetationshoehe_LFI.tif")
plot(vegetationshoehe)

felder <- read_sf("Data/Feldaufnahmen_Fanel.gpkg")
ggplot() + 
  geom_sf(data=felder, aes(fill = Frucht)) +
  theme(legend.position = "none") 

###################################################################
# PRE-PROCESSING
wildschwein <- wildschwein %>%
  rename(Name="TierName", ID="TierID") %>%
  mutate(Time = lubridate::round_date(DatetimeUTC, "15 minutes")) %>%
  dplyr::select(!DatetimeUTC) %>%
  dplyr::select(!CollarID) %>%
  drop_na()

###################################################################
# OVERVIEW  
w <- wildschwein %>%
  mutate(timelag = as.numeric(difftime(lead(Time),Time,units = "mins")))

ggplot(w, aes(Time, Name)) +
  geom_line()

ggplot(w, aes(E, N, colour=Name)) +
  geom_point(size=0.5) +
  theme(legend.position = "none")

###################################################################
# SPEED  
w <- w %>%
  group_by(Name) %>%
  mutate(steplength = sqrt((E-lead(E))^2+(N-lead(N))^2)) %>%
  mutate(speed = steplength/timelag)

###################################################################
# MOVING WINDOW FUNCTION


###################################################################
# KERNEL DENSITY ESTIMATION

###################################################################
# STATISTICAL TEST 
# Are wildboars more often within the Fanel area?

###################################################################
# VISUALIZATIONS







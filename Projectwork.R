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
w <- wildschwein_BE

###################################################################
# IMPORT MAPS
karte <- terra::rast("Data/pk100_BE_2056.tiff")
plotRGB(karte)

felder <- read_sf("Data/Feldaufnahmen_Fanel.gpkg")
ggplot() + 
  geom_sf(data=felder, aes(fill = Frucht)) +
  theme(legend.position = "none") 

###################################################################
# PRE-PROCESSING
w <- w %>%
  rename(Name="TierName", ID="TierID") %>%
  mutate(Date = lubridate::round_date(DatetimeUTC, "15 minutes")) %>%
  mutate(timelag = as.numeric(difftime(lead(Date),Date,units = "mins"))) %>%
  dplyr::select(!DatetimeUTC) %>%
  dplyr::select(!CollarID) %>%
  drop_na()

wildschwein <- w %>%
  filter(timelag==15)

###################################################################
# OVERVIEW  
ggplot(w, aes(Date, Name)) +
  geom_line()

ggplot(w, aes(E, N, colour=Name)) +
  geom_point(size=0.1) +
  theme(legend.position = "none")

###################################################################
# Wildschweine - Bewegungsdaten visualisieren
ggplot() + 
  geom_sf(data=felder, aes(fill = Frucht)) +
  theme(legend.position = "none") +
  geom_point(data=w, aes(E, N, colour=Name), size=0.5) # overplotting!!!

###################################################################
# KERNEL DENSITY ESTIMATION

# Datentyp umwandeln
w <- st_as_sf(w, coords = c("E", "N"), crs = 4326)

# KDE-Funktion erstellen
kde <- function(points, cellsize, bandwith, extent = NULL){
  require(MASS)
  require(raster)
  require(sf)
  require(stars)
  if(is.null(extent)){
    extent_vec <- st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- st_bbox(extent)[c(1,3,2,4)]
  }
  
  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)
  
  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize
  
  coords <- st_coordinates(points)
  matrix <- kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  raster(matrix)
}

# Dichteverteilung berechnen (--> Rasterdatensatz)
w_kde <- kde(w, cellsize=100, bandwith=500, extent=felder)

# Visualisierung mit base R
plot(w_kde)

# Visualisierung mit ggplot
ggplot() + 
  geom_stars(data=st_as_stars(w_kde)) +
  geom_sf(data=felder, fill=NA) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "none")

# Nur die höchsten 2 bzw. 5% der Werte
q95 <- raster::quantile(w_kde, probs=0.95)

# Visualisierung mit ggplot (nur die höchsten 5% der Werte darstellen)
ggplot() + 
  geom_sf(data=felder, fill=NA, color="black") +
  geom_stars(data=st_as_stars(w_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine")

###################################################################

# Konvertierung 
w_dataframe <- raster::as.data.frame(w_kde,xy=TRUE)
w_kde_sf <- st_as_sf(w_dataframe, coords = c("x", "y"), crs = 4326)


w_kde





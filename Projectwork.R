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

###################################################################
# IMPORT MAPS
karte <- terra::rast("Data/pk100_BE_2056.tiff")
plotRGB(karte)

felder <- read_sf("Data/Feldaufnahmen_Fanel.gpkg")
ggplot() + 
  geom_sf(data=felder, aes(fill = Frucht))
  theme(legend.position = "none") 

###################################################################
# PRE-PROCESSING
w <- w %>%
  rename(Name="TierName", ID="TierID") %>%
  mutate(Date = lubridate::round_date(DatetimeUTC, "15 minutes")) %>%
  dplyr::select(!DatetimeUTC) %>%
  dplyr::select(!CollarID) %>%
  drop_na()

###################################################################
# OVERVIEW  
w <- wildschwein %>%
  mutate(timelag = as.numeric(difftime(lead(Date),Date,units = "mins")))

ggplot(w, aes(Date, Name)) +
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
# KERNEL DENSITY ESTIMATION (TEST SAMPLE)
# Trainingsample erstellen (Simon)
w_sample <- w %>% 
  filter(Name %in% c("Ueli", "Caroline"), 
        Date > ymd_hms("2016-04-01 00:00:00"), 
        Date < ymd_hms("2016-06-01 00:00:00"))

# Datentyp umwandeln
w_sample_sf <- st_as_sf(w_sample, coords = c("E", "N"), crs = 4326)

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
# cellsize = Zellgrösse des Outputs
# Bandwidth = Suchradius für die Dichteberechnung
# Extent = Perimeter, in dem die Dicteverteilung berechnet werden soll

# Dichteverteilung berechnen
w_sample_kde <- kde(w_sample_sf, cellsize=10, bandwith=50, extent=felder)
w_sample_kde # wir erhalten einen Rasterdatensatz

# Visualisirung mit Base-R
plot(w_sample_kde)

# Visualisierung mit ggplot
ggplot() + 
  geom_stars(data=st_as_stars(w_sample_kde)) +
  geom_sf(data=felder, fill=NA) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "none")

# Visualisierung mit ggplot (nur die höchsten 5% der Werte darstellen)
q95 <- raster::quantile(w_sample_kde, probs=0.95)

ggplot() + 
  geom_sf(data=felder, fill=NA, color="black") +
  geom_stars(data=st_as_stars(w_sample_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine")

###################################################################
# KERNEL DENSITY ESTIMATION (ALLE)

# Datentyp umwandeln
w_sf <- st_as_sf(w, coords = c("E", "N"), crs = 4326)

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

# Dichteverteilung berechnen
w_kde <- kde(w_sf, cellsize=100, bandwith=500, extent=felder)
w_kde # wir erhalten einen Rasterdatensatz

# Visualisirung mit Base-R
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
q98 <- raster::quantile(w_kde, probs=0.98)

# Visualisierung mit ggplot (nur die höchsten 2% der Werte darstellen)
ggplot() + 
  geom_sf(data=felder, fill=NA, color="black") +
  geom_stars(data=st_as_stars(w_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine")

###################################################################







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

felder <- read_sf("Data/Feldaufnahmen_Fanel.gpkg")
ggplot() + 
  geom_sf(data=felder, aes(fill = Frucht)) +
  theme(legend.position = "none") 

###################################################################
# PRE-PROCESSING
wildschwein <- wildschwein %>%
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
# MOVING WINDOW FUNCTION
# temporal window = 15min
# threshold to differentiate between resting and no resting

Ueli <- Ueli %>%
  mutate(static = steplength < mean(Ueli$steplength, na.rm = TRUE))

Ueli <- w %>%
  filter(Name == "Ueli") %>%
  filter(Time > "2014-05-28",
         Time < "2014-05-30")
Ueli %>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

###################################################################
# KERNEL DENSITY ESTIMATION
# Trainingsample erstellen (Simon)
wildschwein_sample <- wildschwein %>% 
  filter(Name %in% c("Ueli", "Caroline"), 
        Date > ymd_hms("2016-04-01 00:00:00"), 
        Date < ymd_hms("2016-06-01 00:00:00"))

# Datentyp umwandeln
wildschwein_sf <- st_as_sf(wildschwein_sample, 
                           coords = c("E", "N"), 
                           crs = 4326)

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
wildschwein_kde <- kde(wildschwein_sf, cellsize=100, bandwith=500, extent=felder)
wildschwein_kde # wir erhalten einen Rasterdatensatz

# Visualisirung mit Base-R
plot(wildschwein_kde)

# Visualisierung mit ggplot
ggplot() + 
  geom_stars(data=st_as_stars(wildschwein_kde)) +
  geom_sf(data=felder, fill=NA) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(legend.position = "none")

# Visualisierung mit ggplot (nur die höchsten 5% der Werte darstellen)
q95 <- raster::quantile(wildschwein_kde, probs=0.95)

ggplot() + 
  geom_sf(data=felder, fill=NA, color="black") +
  geom_stars(data=st_as_stars(wildschwein_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine")


###################################################################
# STATISTICAL TEST 
# Are wildboars more often within the Fanel area?

###################################################################
# VISUALIZATIONS






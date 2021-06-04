###################################################################
# LIBRARIES
library(tidyverse) 
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data (sf = shape file)
library(terra)        # to handle raster data
library(lubridate)    # to handle dates and times
library(zoo)          # moving window functions
library(tmap)         # thematic map

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
pk100_BE <- terra::rast("Data/pk100_BE_2056.tiff")
plotRGB(pk100_BE)

vegetationshoehe <- terra::rast("Data/vegetationshoehe_LFI.tif")
plot(vegetationshoehe)

feldaufnahmen <- read_sf("Data/Feldaufnahmen_Fanel.gpkg")
ggplot() + 
  geom_sf(data=feldaufnahmen, aes(fill = Frucht)) +
  theme(legend.position = "none") 

###################################################################
# OVERVIEW  
ggplot(wildschwein, aes(E, N, colour = TierName)) +
  geom_point(size=0.7) +
  theme(legend.position = "none")

###################################################################
# PRE-PROCESSING
wildschwein_neu <- wildschwein %>%
  rename(Name="TierName", ID="TierID") %>%
  mutate(Time = lubridate::round_date(DatetimeUTC,"15 minutes")) %>%
  dplyr::select(!DatetimeUTC) %>%$
  dplyr::select(!CollarID) %>%
  drop_na()


###################################################################
# STATISTIC TEST 
# Are wildboars more often within the Fanel area?

###################################################################
# 




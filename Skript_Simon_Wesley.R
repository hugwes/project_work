
###################################################################
# BIBLIOTHEKEN
library(tidyverse)    # diverse packages
library(readr)        # import tabular data (csv)
library(dplyr)        # manipulate data
library(ggplot2)      # visualize data
library(sf)           # spatial vector data (shape file)
library(terra)        # raster data
library(lubridate)    # dates and times
library(zoo)          # moving window functions
library(tmap)         # thematic map
library(rgeos)        # centroids
library(plyr)
library(viridis)
library(tidyr)
library(naniar)
library(stringr)
library(ComputationalMovementAnalysisData)

###################################################################
# DATEN IMPORTIEREN 
# Wildschweine
devtools::install_github("ComputationalMovementAnalysis/ComputationalMovementAnalysisData")
head(wildschwein_BE)
wildschwein_BE <- wildschwein_BE

# Feldaufnahmen
Feldaufnahmen <- read_sf("Data/Feldaufnahmen_Fanel.gpkg") %>%
  st_as_sf(coords=geom, crs=2056, remove=FALSE)

###################################################################
# ERTSTE VISUALISIERUNG
# Zeiträume
ggplot(wildschwein_BE, aes(DatetimeUTC, TierName)) +
  geom_line() +
  labs(x="Zeit", y="Tiername") 

# Wildschweine
ggplot() +
  geom_point(data=wildschwein_BE, aes(E, N, color=TierName), size=0.2)# Feldaufnahmen

# Feldaufnahmen
ggplot() +
  geom_sf(data=Feldaufnahmen, aes(fill=Frucht))

# Wildschweine & Feldaufnahmen
ggplot() +
  geom_sf(data=Feldaufnahmen, aes(fill=Frucht)) +
  geom_point(data=wildschwein_BE, aes(E, N, color=TierName), size=0.2)

###################################################################
# DATENVORBEARBEITUNG
# Filtern nach 15 min-Messungen
wildschwein_BE$DatetimeUTC <- as.POSIXct(as.character(wildschwein_BE$DatetimeUTC), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
wildschwein_BE$Minute <- minute(wildschwein_BE$DatetimeUTC)
wildschwein_BE <- wildschwein_BE %>% filter(Minute == c(00, 15, 30, 45))
wildschwein_BE <- wildschwein_BE[,-(9),drop=FALSE]

###################################################################
### 1. ANSATZ - VEKTORDATEN
###################################################################

###################################################################
# EUKLIDISCHE DISTANZ / MOVING WINDOW FUNKTION
# Stepmean (Euklidische Distanz)
trainingsample <- wildschwein_BE %>%
  mutate(
    stepMean = rowMeans(                       
      cbind(                                   
        sqrt((lag(E,6)-E)^2+(lag(E,6)-E)^2),         
        sqrt((lag(E,5)-E)^2+(lag(E,5)-E)^2),         
        sqrt((lag(E,4)-E)^2+(lag(E,4)-E)^2),         
        sqrt((lag(E,3)-E)^2+(lag(E,3)-E)^2),   
        sqrt((lag(E,2)-E)^2+(lag(E,2)-E)^2),   
        sqrt((lag(E,1)-E)^2+(lag(E,1)-E)^2),   
        sqrt((E-lead(E,1))^2+(E-lead(E,1))^2),  
        sqrt((E-lead(E,2))^2+(E-lead(E,2))^2),
        sqrt((E-lead(E,3))^2+(E-lead(E,3))^2),  
        sqrt((E-lead(E,4))^2+(E-lead(E,4))^2),  
        sqrt((E-lead(E,5))^2+(E-lead(E,5))^2),  
        sqrt((E-lead(E,6))^2+(E-lead(E,6))^2))))

# Static (Aufteilung in Stops & Moves)
trainingsample_stops <- trainingsample %>% 
  ungroup() %>%
  mutate(static = stepMean < 8.69)

# Segmentbasierte Analyse (Segment-Id's für Trajektories definieren)
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))}

trainingsample_stops <- trainingsample_stops %>%
  mutate(segment_id = rle_id(static))

# Länge der Trajectories messen
trainingsample_stops$Dauer <- 1
trainingsample_stops_dauer <- aggregate(trainingsample_stops[, c(12)], list(trainingsample_stops$segment_id), sum)
names(trainingsample_stops_dauer)[1] <- "segment_id"
trainingsample_stops <- left_join(trainingsample_stops, trainingsample_stops_dauer, by="segment_id")

# Move-Segmente entfernen
trainingsample_filter <- trainingsample_stops %>%
  filter(static)

# Plot (mögliche Ruheplätze nach Tiernamen)
trainingsample_filter %>%
  ggplot()  +
  geom_point(aes(E, N, color=TierName))+
  coord_fixed()

# Segmentzentren festlegen
trainingsample_center <- aggregate(trainingsample_filter[, c(5:6)], list(trainingsample_filter$segment_id), mean)

# Join (TierID/TierName)
names(trainingsample_center)[1] <- "segment_id"
trainingsample_center_join <- left_join(trainingsample_filter, trainingsample_center, by="segment_id")

# Plot (Feldaufnahmen & Mögliche Ruheplätze)
ggplot() +
  geom_sf(data = Feldaufnahmen, aes(fill = Frucht))+
  geom_point(data = trainingsample_center_join, aes(E.y, N.y, color=TierName, size=Dauer.y))

###################################################################
# Feldaufnahmen joinen and Dataframe säubern
wildschwein <- trainingsample_center_join
head(wildschwein)
wildschwein <- wildschwein[,-(3),drop=FALSE]
head(wildschwein)
wildschwein <- wildschwein[,-(7),drop=FALSE]
head(wildschwein)
wildschwein <- wildschwein[,-(7:8),drop=FALSE]
head(wildschwein)
wildschwein <- wildschwein[,-(8),drop=FALSE]
head(wildschwein)

###################################################################
# Feldaufnahmen kategorisieren, NA's entfernen
Feldaufnahmen_korr <- Feldaufnahmen %>%
  mutate(Frucht = str_replace(Frucht, "Acker|Ackerbohnen|Baumschule|Blumenbeet|Bohnen|Brokkoli|Erbsen|Fenchel|Flachs|Flugplatz|Gemuese|Gewaechshaus|Karotten|Kartoffeln|Kohl|Kohlrabi|Kuerbis|Lauch|Lupinen|Mangold|Obstplantage|Rhabarber|Rueben|Salat|Sellerie|Spargel|Spinat|Zucchetti|Zwiebeln", "Niedere Kulturen"))%>%
  mutate(Frucht = str_replace(Frucht, "NiedereKulturenbohnen|Niedere Kulturenrabi|Niedere Kulturenbohnen", "Niedere Kulturen"))%>%
  mutate(Frucht = str_replace(Frucht, "Gerste|Hafer|Roggen|Weizen", "Getreide"))%>%
  mutate(Frucht = str_replace(Frucht, "Wiese|Weide", "Wiese/Weide"))%>%
  mutate(Frucht = str_replace(Frucht, "Buntbrache|Brache", "zus.(Bunt)-Brache"))%>%
  mutate(Frucht = str_replace(Frucht, "Raps", "zus. Raps"))

wildschwein <- wildschwein %>% st_as_sf(coords = c("E.y", "N.y"), crs=2056, remove=FALSE)
wildschwein <- st_join(wildschwein,Feldaufnahmen_korr, suffix = c("E.y", "N.y"))
wildschwein <- wildschwein %>% drop_na(Frucht)

# Plot (Ruheplätze im Untersuchungsgebiet nach Vegetationstyp)
ggplot() +
  geom_sf(data=Feldaufnahmen, aes())+
  geom_point(data = wildschwein, aes(E.y, N.y, color = Frucht, size = Dauer.y))

###################################################################
# AUFTEILUNG NACH JAHRESZEIT 
wildschwein$DatetimeUTC <- as.POSIXct(as.character(wildschwein$DatetimeUTC), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
wildschwein$Monat <- month(wildschwein$DatetimeUTC)
wildschwein$Jahreszeit[wildschwein$Monat == "3"] <- "Fruehling"
wildschwein$Jahreszeit[wildschwein$Monat == "4"] <- "Fruehling"
wildschwein$Jahreszeit[wildschwein$Monat == "5"] <- "Fruehling"
wildschwein$Jahreszeit[wildschwein$Monat == "6"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "7"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "8"] <- "Sommer"
wildschwein$Jahreszeit[wildschwein$Monat == "9"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "10"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "11"] <- "Herbst"
wildschwein$Jahreszeit[wildschwein$Monat == "12"] <- "Winter"
wildschwein$Jahreszeit[wildschwein$Monat == "1"] <- "Winter"
wildschwein$Jahreszeit[wildschwein$Monat == "2"] <- "Winter"
head(wildschwein)

# Anteil an Flächen
wildschwein$Anteil <- 1
wildschwein_anteil<- aggregate(wildschwein[, c(16)], list(wildschwein$Frucht), sum)
wildschwein_anteil_jahreszeit<- aggregate(wildschwein[, c(16)], list(wildschwein$Frucht, wildschwein$Jahreszeit), sum)

wildschwein_anteil_fruehling<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Fruehling")
wildschwein_anteil_sommer<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Sommer")
wildschwein_anteil_herbst<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Herbst")
wildschwein_anteil_winter<-wildschwein_anteil_jahreszeit%>%filter(Group.2 == "Winter")

barplot(Anteil~Group.1, data = wildschwein_anteil)

###################################################################
# RESULTATE PLOTS
names(wildschwein)[8] <- "Anzahl_Messungen"
names(wildschwein)[13] <- "Habitattyp"

# Frühling, Sommer, Herbst und Winter in richtiger Reihenfolge
neworder <- c("Fruehling","Sommer","Herbst", "Winter")
wildschwein <- arrange(transform(wildschwein,
                                 Jahreszeit=factor(Jahreszeit,levels=neworder)),Jahreszeit)

# Plot: Ruheplaetze im Untersuchungsgebiet
ggplot() +
  geom_bar(data=wildschwein, aes(sum(Anteil),fill = Habitattyp), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung der Ruheplaetze in die verschiedenen Habitattypen", title = "Ruheplaetze im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Plot: Ruheplaetze im Untersuchungsgebiet nach Vegetationstyp
ggplot() +
  geom_sf(data=Feldaufnahmen, aes())+
  geom_point(data = wildschwein, aes(E.y, N.y, color = Habitattyp, size = Anzahl_Messungen))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "", title = "Ruheplaetze im Untersuchungsgebiet nach Vegetationstyp", subtitle = "")

###################################################################
# Kuchendiagramme und Prozentzahlen total und aufgeteilt nach Jahreszeit
pct <- round(wildschwein_anteil$Anteil/sum(wildschwein_anteil$Anteil)*100)
lbls <- paste(wildschwein_anteil$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp")

pct <- round(wildschwein_anteil_fruehling$Anteil/sum(wildschwein_anteil_fruehling$Anteil)*100)
lbls <- paste(wildschwein_anteil_fruehling$Group.1, pct)
lbls <- paste(lbls,"%",sep="")
pie(wildschwein_anteil_fruehling$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Fruehling")

pct <- round(wildschwein_anteil_sommer$Anteil/sum(wildschwein_anteil_sommer$Anteil)*100)
lbls <- paste(wildschwein_anteil_sommer$Group.1, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(wildschwein_anteil_sommer$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Sommer")

pct <- round(wildschwein_anteil_herbst$Anteil/sum(wildschwein_anteil_herbst$Anteil)*100)
lbls <- paste(wildschwein_anteil_herbst$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil_herbst$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Herbst")

pct <- round(wildschwein_anteil_winter$Anteil/sum(wildschwein_anteil_winter$Anteil)*100)
lbls <- paste(wildschwein_anteil_winter$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_anteil_winter$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Winter")

# Durchschnittliche Dauer am Ruheort
wildschwein_dauer <- aggregate(wildschwein[, c(8)], list(wildschwein$Habitattyp), mean)



###################################################################
# Vergleich mit dem von den Wildschweinen genutztem Habitat
# Feldaufnahmen kategorisieren, NA's entfernen
wildschwein_BE <- wildschwein_BE%>%
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

wildschwein_all <-st_join(wildschwein_BE,Feldaufnahmen_korr, suffix = c("E", "N"))
wildschwein_all <-wildschwein_all%>% drop_na(Frucht)

names(wildschwein_all)[11] <- "Habitattyp"

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein_all, aes(E, N))

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes())+
  geom_point(data = wildschwein_all, aes(E, N, color = Habitattyp))

# Anteil an Flächen

# Aufteilen nach Jahreszeit
wildschwein_all$DatetimeUTC<-as.POSIXct(as.character(wildschwein_all$DatetimeUTC), format = "%Y-%m-%d %H:%M:%OS",tz = "UTC")
wildschwein_all$Monat <- month(wildschwein_all$DatetimeUTC)
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "3"] <- "Fruehling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "4"] <- "Fruehling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "5"] <- "Fruehling"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "6"] <- "Sommer"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "7"] <- "Sommer"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "8"] <- "Sommer"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "9"] <- "Herbst"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "10"] <- "Herbst"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "11"] <- "Herbst"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "12"] <- "Winter"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "1"] <- "Winter"
wildschwein_all$Jahreszeit[wildschwein_all$Monat == "2"] <- "Winter"

wildschwein_all$Anteil <- 1
wildschwein_all_anteil<- aggregate(wildschwein_all[, c(14)], list(wildschwein_all$Habitattyp), sum)
wildschwein_all_anteil_jahreszeit<- aggregate(wildschwein_all[, c(14)], list(wildschwein_all$Habitattyp, wildschwein_all$Jahreszeit), sum)

wildschwein_all_anteil_fruehling<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Fruehling")
wildschwein_all_anteil_sommer<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Sommer")
wildschwein_all_anteil_herbst<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Herbst")
wildschwein_all_anteil_winter<-wildschwein_all_anteil_jahreszeit%>%filter(Group.2 == "Winter")

barplot(Anteil~Group.1, data = wildschwein_all_anteil)

# Frühling, Sommer, Herbst und Winter in richtiger Rheienfolge
neworder <- c("Fruehling","Sommer","Herbst", "Winter")
library(plyr)  ## or dplyr (transform -> mutate)
wildschwein_all <- arrange(transform(wildschwein_all,
                                     Jahreszeit=factor(Jahreszeit,levels=neworder)),Jahreszeit)

# Resultate Plots
ggplot() +
  geom_bar(data=wildschwein_all, aes(sum(Anteil),fill = Habitattyp), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung aller Lokationen in die verschiedenen Habitattypen", title = "Raumnutzung im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill = Frucht))+
  geom_point(data = wildschwein_all, aes(E, N))+
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank())+
  labs(x = "", y = "", title = "Raumnutzung Untersuchungsgebiet nach Habitattyp", subtitle = "")

# Kuchendiagramme und Prozentzahlen
pct <- round(wildschwein_all_anteil$Anteil/sum(wildschwein_all_anteil$Anteil)*100)
lbls <- paste(wildschwein_all_anteil$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_all_anteil$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der aller Lokationen nach Vegetationstyp")

pct <- round(wildschwein_all_anteil_fruehling$Anteil/sum(wildschwein_all_anteil_fruehling$Anteil)*100)
lbls <- paste(wildschwein_all_anteil_fruehling$Group.1, pct)
lbls <- paste(lbls,"%",sep="")
pie(wildschwein_all_anteil_fruehling$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Fruehling")

pct <- round(wildschwein_anteil_sommer$Anteil/sum(wildschwein_anteil_sommer$Anteil)*100)
lbls <- paste(wildschwein_anteil_sommer$Group.1, pct)
lbls <- paste(lbls,"%",sep="")
pie(wildschwein_anteil_sommer$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Sommer")

pct <- round(wildschwein_all_anteil_herbst$Anteil/sum(wildschwein_all_anteil_herbst$Anteil)*100)
lbls <- paste(wildschwein_all_anteil_herbst$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_all_anteil_herbst$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Herbst")

pct <- round(wildschwein_all_anteil_winter$Anteil/sum(wildschwein_all_anteil_winter$Anteil)*100)
lbls <- paste(wildschwein_all_anteil_winter$Group.1, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(wildschwein_all_anteil_winter$Anteil,labels = lbls, col=rainbow(length(lbls)),
    main="Aufteilung der Ruheplaetze nach Vegetationstyp - Winter")

# Vergleich Ruheplätze zu gesamter Raumnutzung
wildschwein_anteil$Data<-"Ruheplatz"
wildschwein_all_anteil$Data<-"Alle"
wildschwein_anteil_rbind<-rbind(wildschwein_anteil, wildschwein_all_anteil)

ggplot() +
  geom_bar(data=wildschwein_all, aes(sum(Anteil),fill = Habitattyp), position = "fill")+
  facet_grid(~Jahreszeit)+
  labs(x = "Jahreszeiten", y = "Aufteilung aller Lokationen in die verschiedenen Habitattypen", title = "Raumnutzung im Untersuchungsgebiet")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

###################################################################
# STATISTISCHE TESTS

# Präferenz von Schilf als Ruheplatz:
# Anteil Ruhezeit im Feuchtgebiet vs. Anteil Aufenthaltszeit total im Feuchtgebiet
Ruheplatz <- c(76, 39, 82, 91)
All <- c(48, 39, 53, 70)
Anteil_Schilf<-data.frame(Ruheplatz,All)

boxplot(Anteil_Schilf$Ruheplatz, Anteil_Schilf$All)
t.test(Anteil_Schilf$Ruheplatz, Anteil_Schilf$All) 
# --> nicht signifikant

Ruheplatz <- c(15,24,6,9)
All <- c(29,24,23,21)
Anteil_Wald<-data.frame(Ruheplatz,All)

boxplot(Anteil_Wald$Ruheplatz, Anteil_Schilf$All)
t.test(Anteil_Wald$Ruheplatz, Anteil_Schilf$All)
# --> signifikant

Ruheplatz <- c(9,38,14,0)
All <- c(23, 37,24,9)
Anteil_landw<-data.frame(Ruheplatz,All)

boxplot(Anteil_landw$Ruheplatz, Anteil_Schilf$All)
t.test(Anteil_landw$Ruheplatz, Anteil_Schilf$All)
# --> signifikant

# Ruhephase untersuchen zwischen den verschiedenen Vegetationstypen
Schilf<- wildschwein%>%filter(Habitattyp == "Feuchtgebiet")
Schilf<-Schilf[, 8:9]
Schilf<-Schilf[!duplicated(Schilf), ]
Schilf<-Schilf[, 1]
mean(Schilf)
Schilf<-Schilf[1:1208]

Chinaschilf<- wildschwein%>%filter(Habitattyp == "Chinaschilf")
Chinaschilf<-Chinaschilf[, 8:9]
Chinaschilf<-Chinaschilf[!duplicated(Chinaschilf), ]
Chinaschilf<-Chinaschilf[, 1]
mean(Chinaschilf)

Raps<- wildschwein%>%filter(Habitattyp == "zus. Raps")
Raps<-Raps[, 8:9]
Raps<-Raps[, 1]
mean(Raps)

Getreide<- wildschwein%>%filter(Habitattyp == "Getreide")
Getreide<-Getreide[, 8:9]
Getreide<-Getreide[, 1]
mean(Getreide)

Wald<- wildschwein%>%filter(Habitattyp == "Wald")
Wald<-Wald[, 8:9]
Wald<-Wald[!duplicated(Wald), ]
Wald<-Wald[, 1]
mean(Wald)

Mais<- wildschwein%>%filter(Habitattyp == "Mais")
Mais<-Mais[, 8:9]
Mais<-Mais[!duplicated(Mais), ]
Mais<-Mais[, 1]
mean(Mais)

Niedere_Kulturen<- wildschwein%>%filter(Habitattyp == "Niedere Kulturen")
Niedere_Kulturen<-Niedere_Kulturen[, 8:9]
Niedere_Kulturen<-Niedere_Kulturen[!duplicated(Niedere_Kulturen), ]
Niedere_Kulturen<-Niedere_Kulturen[, 1]
mean(Niedere_Kulturen)

Sonnenblumen<- wildschwein%>%filter(Habitattyp == "Sonnenblumen")
Sonnenblumen<-Sonnenblumen[, 8:9]
Sonnenblumen<-Sonnenblumen[!duplicated(Sonnenblumen), ]
Sonnenblumen<-Sonnenblumen[, 1]
mean(Sonnenblumen)

Wiese_Weide<- wildschwein%>%filter(Habitattyp == "Wiese/Weide")
Wiese_Weide<-Wiese_Weide[, 8:9]
Wiese_Weide<-Wiese_Weide[!duplicated(Wiese_Weide), ]
Wiese_Weide<-Wiese_Weide[, 1]
mean(Wiese_Weide)

# Vergleich Schilf, Wald
Dauer<-data.frame(Schilf, Wald)

boxplot(Dauer$Schilf, Dauer$Wald)
t.test(Dauer$Schilf, Dauer$Wald)
# --> signifikant

###################################################################
### 2. ANSATZ - RASTERDATEN ###
###################################################################

###################################################################
# DATEN FILTERN UM DIESE DANACH FÜR DIE KDE ZU VERWENDEN
# Stop-Segmente entfernen
trainingsample_moves <- trainingsample_stops %>%
  filter(!static)

# Join mit Feldaufnahmen
w <- st_as_sf(trainingsample_moves, coords = c("E", "N"), crs=2056, remove= FALSE)
w_join <-  st_join(w, Feldaufnahmen)

w_filter <- w_join %>%
  filter(!is.na(Frucht))

###################################################################
# KERNDICHTESCHÄTZUNG (KDE)
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

# KDE ausführen 
w_kde <- kde(w_filter, cellsize=20, bandwith=50, extent=Feldaufnahmen)

# Visualisierung mit ggplot (nur die höchsten 5% der Werte darstellen)
q95 <- raster::quantile(w_kde, probs=0.95)

# Plot (Dichteverteilung der Wildschweine)
ggplot() + 
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(w_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine")


###################################################################
# VERKNÜPFUNG MIT DEN VEGETATIONSTYPEN
# Zentroide der einzelnen Vegetationszonen
centroids <- sf::st_centroid(Feldaufnahmen)

# Plot: Parzellen mit Zentroiden
ggplot() + 
  geom_sf(data=Feldaufnahmen, aes(fill=Frucht)) + 
  geom_sf(data=centroids)

# Werte der KDE in den Zentroiden auf die Parzellen extrahieren
kde_value <- terra::extract(w_kde, st_coordinates(centroids))
W_kde <- cbind(Feldaufnahmen, kde_value)

# Plot: Zentroide & KDE
ggplot() + 
  geom_sf(data=centroids, size=0.8) +
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(w_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine")

# Plot: KDE-Werte der Parzellen
ggplot() + 
  geom_sf(data=Feldaufnahmen) + 
  geom_sf(data=W_kde, aes(fill=kde_value), color="black") +
  theme_void()

ggplot() + 
  geom_sf(data = Feldaufnahmen,  aes()) +
  geom_sf(data = Feldaufnahmen,  aes(fill = kde_value), lwd = 0) +
  scale_fill_viridis_c(trans="log10", limits=c(q95,NA), option= "viridis", na.value=NA) +
  theme_void()

###################################################################
# MITTELWERTE DER VEGETATIONSTYPEN
# Mittelwerte pro Vegetationstyp
w_mittelwerte <- aggregate(W_kde$kde_value ~ W_kde$Frucht, FUN=mean)

###################################################################
# KDE (AUFGETEILT NACH JAHRESZEITEN)
# Daten filtern
Winter <- w_filter %>%
  mutate(month = month(DatetimeUTC)) %>%
  filter(month == "1" | month == "2" | month == "12")

Frühjahr <- w_filter %>%
  mutate(month = month(DatetimeUTC)) %>%
  filter(month == "3" | month == "4" | month == "5")

Sommer <- w_filter %>%
  mutate(month = month(DatetimeUTC)) %>%
  filter(month == "6" | month == "7" | month == "8")

Herbst <- w_filter %>%
  mutate(month = month(DatetimeUTC)) %>%
  filter(month == "9" | month == "10" | month == "11")

# KDE für den Winter
winter_kde <- kde(Winter, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_winter <- raster::quantile(winter_kde, probs=0.95)
ggplot() + 
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(winter_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_winter,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Winter")

# KDE für den Frühling
frühjahr_kde <- kde(Frühjahr, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_frühjahr <- raster::quantile(frühjahr_kde, probs=0.95)
ggplot() + 
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(frühjahr_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_frühjahr,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Frühjahr")

# KDE für den Sommer
sommer_kde <- kde(Sommer, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_sommer <- raster::quantile(sommer_kde, probs=0.95)
ggplot() + 
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(sommer_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_sommer,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Sommer")

# KDE für den Herbst
herbst_kde <- kde(Herbst, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_herbst <- raster::quantile(herbst_kde, probs=0.95)
ggplot() + 
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(herbst_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_herbst,NA), na.value=NA) +
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Herbst")

###################################################################
# VERKNÜPFUNG MIT DEN VEGETATIONSTYPEN & MITTELWERTE

# Winter
kde_value_winter <- terra::extract(winter_kde, st_coordinates(centroids))
W_kde <- cbind(Feldaufnahmen, kde_value_winter)
winter_mittelwerte <- aggregate(W_kde$kde_value_winter ~ W_kde$Frucht, FUN=mean)

# Frühling
kde_value_frühjahr <- terra::extract(frühjahr_kde, st_coordinates(centroids))
W_kde <- cbind(Feldaufnahmen, kde_value_frühjahr)
frühjahr_mittelwerte <- aggregate(W_kde$kde_value_frühjahr ~ W_kde$Frucht, FUN=mean)

# Sommer
kde_value_sommer <- terra::extract(sommer_kde, st_coordinates(centroids))
W_kde <- cbind(Feldaufnahmen, kde_value_sommer)
sommer_mittelwerte <- aggregate(W_kde$kde_value_sommer ~ W_kde$Frucht, FUN=mean)

# Herbst
kde_value_herbst <- terra::extract(herbst_kde, st_coordinates(centroids))
W_kde <- cbind(Feldaufnahmen, kde_value_herbst)
herbst_mittelwerte <- aggregate(W_kde$kde_value_herbst ~ W_kde$Frucht, FUN=mean)


###################################################################
### PLOTS FÜR DISKUSSION ###
###################################################################

###################################################################
# RUHEPLÄTZE NACH JAHRESZEITEN FILTERN
wildschwein_winter <- wildschwein%>%filter(Jahreszeit == "Winter")
wildschwein_frueling <- wildschwein%>%filter(Jahreszeit == "Fruehling")
wildschwein_sommer <- wildschwein%>%filter(Jahreszeit == "Sommer")
wildschwein_herbst <- wildschwein%>%filter(Jahreszeit == "Herbst")

###################################################################
# RUHEPLÄTZE MIT KDE - VERGLEICH
# Winter
winter_kde <- kde(Winter, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_winter <- raster::quantile(winter_kde, probs=0.95)
ggplot() +
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(winter_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_winter,NA), na.value=NA) +
  geom_point(data = wildschwein_winter, aes(E.y, N.y, size = Anzahl_Messungen))+
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Winter")

# Frühling
frühjahr_kde <- kde(Frühjahr, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_frühjahr <- raster::quantile(frühjahr_kde, probs=0.95)
ggplot() +
  geom_sf(data=Feldaufnahmen_korr, aes(fill=NA, color=Feldaufnahmen_korr$Frucht)) +
  geom_stars(data=st_as_stars(frühjahr_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_frühjahr,NA), na.value=NA) +
  geom_point(data = wildschwein_frueling, aes(E.y, N.y, color = Habitattyp, size = Anzahl_Messungen))+
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Frühjahr")

# Sommer
sommer_kde <- kde(Sommer, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_sommer <- raster::quantile(sommer_kde, probs=0.95)
ggplot() +
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(sommer_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_sommer,NA), na.value=NA) +
  geom_point(data = wildschwein_sommer, aes(E.y, N.y, color = Habitattyp, size = Anzahl_Messungen))+
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Sommer")

# Herbst
herbst_kde <- kde(Herbst, cellsize=20, bandwith=50, extent=Feldaufnahmen)
q95_herbst <- raster::quantile(herbst_kde, probs=0.95)
ggplot() +
  geom_sf(data=Feldaufnahmen, fill=NA, color="black") +
  geom_stars(data=st_as_stars(herbst_kde), alpha=0.8) +
  scale_fill_viridis_c(trans="log10", limits=c(q95_herbst,NA), na.value=NA) +
  geom_point(data = wildschwein_herbst, aes(E.y, N.y, color = Habitattyp, size = Anzahl_Messungen))+
  theme_void() +
  labs(fill="KDE", title="Dichteverteilung der Wildschweine im Herbst")



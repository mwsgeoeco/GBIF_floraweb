### content: GBIF Übung - Flora Web Daten laden und Karten erstellen
###

# benötigte Pakete
library(rgbif)
library(data.table)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# wir laden FloraWeb-Daten aus GBIF.
# dafür müssen wir uns den datasetKey aus GBIF heraussuchen: e6fab7b3-c733-40b9-8df3-2a03e49532c1 oder wir nutzen den institutionCode="BfN"



# Immergrünes Felsenblümchen

speclist <- "Draba aizoides"


#  diese Funktion soll dabei helfen die richtige Art zu finden, eventuell muss man sie mehrmals aufrufen 
name_suggest(speclist)
name_suggest(speclist)$data$key[1] # wir wählen den ersten aus

# mit dieser Funktion kann man das später für eine ganze Liste von Namen machen. Eventuell muss man das mehrmals wiederholen, bevor es klappt. Manchmal ist die Serververbindung nicht gut. 
keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], numeric(1), USE.NAMES=FALSE)
keys 
# vergleich: https://www.gbif.org/species/3049799


# auslesen der Daten aus GBIF. Je höher das Limit ist, desto länger dauert es
gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 1000, datasetKey="e6fab7b3-c733-40b9-8df3-2a03e49532c1")

gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 1000, institutionCode="BfN") # das ist das Gleiche

# umwandeln in Data Frame
Draba_aizoides <- as.data.frame(gbif_species$data) 

dim(Draba_aizoides) # kommt in 318 Kartenblättern vor


#### plotten auf Karte
# Umwandeln in SimpleFeature Geodaten
Draba_aizoides_sf <- st_as_sf(Draba_aizoides, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))

# wir laden die Geometrie von Deutschland 
germany <- ne_countries(country="Germany")


# plotten
p <- ggplot()  
p <- p +geom_sf(data=germany)
p <- p +  geom_sf(data=Draba_aizoides_sf, aes()) 
p <- p +  ggtitle("Quadranten mit Draba aizoides")
p


library("maptiles")
library(tidyterra)

sat <- get_tiles(germany, provider = "Esri.WorldImagery", crop = TRUE)

p <- ggplot(germany)  
p <- p +  geom_spatraster_rgb(data  = sat)
p <- p +  geom_sf(data=Draba_aizoides_sf, aes()) 
p <- p + geom_sf(data=germany, fill = NA, color = "white")
p <- p +  ggtitle("Quadranten mit Draba aizoides")
p


## zweiter Versuch mit der Hirschzunge
speclist <- "Asplenium scolopendrium"
name_suggest(speclist)
name_suggest(speclist)$data$key[1] # wir wählen den ersten aus


#  diese Funktion soll dabei helfen die richtige Art zu finden
keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], numeric(1), USE.NAMES=FALSE)
keys 
# vergleich: https://www.gbif.org/species/2650669


# auslesen der Daten aus GBIF. Für Borstgrasrasen dauert das sehr lange. 
gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 10000, datasetKey="e6fab7b3-c733-40b9-8df3-2a03e49532c1")

Asplenium_scolopendrium <- as.data.frame(gbif_species$data)

dim(Asplenium_scolopendrium)

#### plotten auf Karte

# Umwandeln in SimpleFeature Geodaten

Asplenium_scolopendrium_sf <- st_as_sf(Asplenium_scolopendrium, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))

# plotten
p <- ggplot()  
p <- p +geom_sf(data=germany)
p <- p +  geom_sf(data=Asplenium_scolopendrium_sf, aes()) 
p <- p +  ggtitle("Quadranten mit Asplenium scolopendrium")
p


###################### Geht es auch für Pflanzengesellschaften


# Beispiel Xerobrometum (Trespen-Trockenrasen): Typische Arten
speclist <- c("Globularia bisnagarica", "Linum tenuifolium", "Fumana procumbens", "Trinia glauca","Teucrium montanum") 


#  diese Funktion soll dabei helfen die richtige Art zu finden
keys <- vapply(speclist, function(x) name_suggest(x)$data$key[1], numeric(1), USE.NAMES=FALSE)
keys 


gbif_species <- occ_data(taxonKey =keys, country="DE", hasCoordinate = TRUE, limit = 10000, institutionCode="BfN")



# Aus GBIF wird eine verschachtelte Liste exportiert, mit dieser Funktion macht man einen Data-Frame draus. 
gbif_species_df<- rbindlist(lapply(gbif_species, function(x) x$data), fill = TRUE, use.names = TRUE)

# auswählen von Koordinaten und Artname
gbif_species_df <- gbif_species_df[,c("species","decimalLatitude","decimalLongitude")]

# anzeigen, wie viele Vorkommen gefunden wurden
table(gbif_species_df$species)

# Doppelungen löschen
gbif_species_df <- gbif_species_df[!duplicated(gbif_species_df),]


#### Zählen der Kombination und plotten auf Karte #########

# Umwandeln in SimpleFeature Geodaten

gbif_species_df_geo <- st_as_sf(gbif_species_df, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(4326))


df <- st_coordinates(gbif_species_df_geo)
df <- as.data.frame(df)

# zählen die Anzahl von Punkten, die übereinander liegen (~Artenzahl)
df2 <-  setDT(df)[,list(value=.N),names(df)]
df2 <- as.data.frame(df2) # wieder in Data Frame umwandeln

table(df2$value)

# alle löschen, wo nur eine Art vorkommt
df2 <- df2[df2$value>1,]


df2 <- df2[order(df2$value),]

# Data Frame in räumliches Objekt umwandeln 
df_count <- st_as_sf(df2, coords = c("X", "Y"), crs = st_crs(4326))


# plotten
p <- ggplot()  
p <- p +geom_sf(data=germany)
p <- p +  geom_sf(data=df_count, aes(colour = value)) 
p <- p +  ggtitle("Anzahl Kennarten Trespen-Trockenrasen")
p <- p + scale_color_distiller(palette ="YlOrRd", na.value=NA,direction = 1) 
windows(width=5, height=7)
p


#### Aufgabe 1:
# Vorkommen von Botrychium lunaria





###############################
# Aufgabe 2 Steppenrasen Gesellschaft
##############################
#"Pulsatilla pratensis", "Adonis vernalis", "Astragalus exscapus", "Oxytropis pilosa", "Potentilla incana", "Seseli hippomarathrum", "Silene otites"





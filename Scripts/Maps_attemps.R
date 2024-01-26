


delphis_byc <- BYC_MANCHE[BYC_MANCHE$ESP_COD_FAO %in% c("DCO", "DLP"), ]

delphis_total <- as.data.frame.table(table(delphis_byc$ESP_COD_FAO))
delphis_total <- delphis_total[delphis_total$Freq != 0, ]
delphis_total <- sum(delphis_total$Freq, na.rm = TRUE)
delphis_total <- as.data.frame(delphis_byc)


D <- rbind(OBSMER_zone, DECL_d_zone[DECL_d_zone$Var1 == "Dauphins nca",])



####### Adding the dolphin to the sp_byc data


index_delphis <- which(sp_total$Var1 == 'Delphinus delphis')

# Ensure that delphis_total is a data frame or table
delphis_total <- as.data.frame.table(table(delphis_byc$ESP_COD_FAO))

# Update 'Freq' for 'Delphinus delphis' in sp_total
sp_total$Freq[index_delphis] <- sp_total$Freq[index_delphis] +
  sum(delphis_total$Freq[delphis_total$Var1 %in% c('DCO', 'DLP')])



delphis_zone$ESP_LIB_FAO[delphis_zone$ESP_LIB_FAO == "Dauphin commun"] <- "Delphinus delphis"

delphis_zone <- delphis_byc[,c("ESP_LIB_FAO","SECT_COD_SACROIS_NIV3","SECT_COD_SACROIS_NIV5","MAREE_DATE_RET", "MAREE_ID")]





years <- 2019:2023
BYC_MANCHE$Year <- NA

# Create loop through each year and update the 'Year' column of the sp_byc dataset
for (year in years) {
  BYC_MANCHE$Year[grepl(as.character(year), BYC_MANCHE$MAREE_DATE_RET, fixed = TRUE)] <- as.character(year)
}




############################################################
############################################################
############################################################
################            MAP             ################             

dauphin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Delphinus delphis"]
murre <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Uria aalge",]
marsouin <- OBSMER_BYC_2021[OBSMER_BYC_2021$ESPECE == "Phocoena phocoena",]

install.packages("maps")
install.packages("mapdata")

library(maps)   # package qui permet de créer des cartes
library(mapdata)

manche <- map("world", xlim=c(-8,4), ylim=c(47,51), col = "gray90", boundary = T, fill = T)

# On rajoute ensuite les axes et une échelle
map.axes(cex.axis=1)
map.scale(3, 41.5, ratio=FALSE, relwidth=0.10, cex=0.8)

#add data points 

dauphin_loc <- points(OBSMER_BYC$LONG_DEB_OP, OBSMER_BYC$LAT_DEB_OP, pch = 3, col = "black")
marsouin_loc <- points(marsouin$LONG_DEB_OP, marsouin$LAT_DEB_OP, pch = 21, col = "blue")
murre_loc <- points(murre$LONG_DEB_OP, murre$LAT_DEB_OP, pch = 20, col = "red")
anguille_loc <- points(anguille$LONG_DEB_OP, anguille$LAT_DEB_OP, pch = 23, col = "orange")

legend(x="bottomright", legend=c("Dauphins","Marsouins", "Murre", "Anguille"), col=c("black","blue", "red", "orange"), pch=c(3,21, 20, 23), cex = 0.8)







library(rnaturalearth)


localisation1 <- data.frame(lon = OBSMER_BYC$LONG_DEB_OP, lat = OBSMER_BYC$LAT_DEB_OP, var = OBSMER_BYC$ESPECE)

selected_species <- c("Delphinus delphis", "Phocoena phocoena", "Halichoerus grypus", "Phoca vitulina")

localisation <- localisation1[localisation1$var %in% selected_species, ]

basemap(limits = c(-7, 2, 47, 52), bathymetry = TRUE)+
     geom_point(data = transform_coord(localisation), aes(x = lon, y = lat))


basemap(data = transform_coord(localisation)) +
  ggspatial::geom_spatial_point(transform_coord(localisation), aes(x = lon, y = lat), color = "red")


basemap(data = localisation, bathymetry = TRUE) +
  geom_point(data = localisation, aes(x = lon, y = lat), color = "red")


########################



library(raster) #for processing some spatial data
library(rnaturalearth) #for downloading shapefiles
library(sf) #for processing shapefiles
library(elevatr) #for downloading elevation data
library(dplyr) #to keep things tidy
library(magrittr) #to keep things very tidy
library(ggspatial) #for scale bars and arrows
library(ggplot2) #for tidy plotting
library(ggpubr) #for easy selection of symbols
library(colourpicker) #for easy selection of colors


if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# Load the devtools package
library(devtools)
# Install the rnaturalearthhires package from GitHub
devtools::install_github("ropensci/rnaturalearthhires")


points <- st_as_sf(localisation, coords = c(1, 2), 
                   crs = '+proj=longlat +datum=WGS84 +ellps=WGS84')
               
map <- ne_countries(scale = 10, returnclass = "sf")
ocean <- ne_download(scale = 10, type = 'ocean', 
                     category = 'physical', returnclass = 'sf')

focalArea <- map %>% filter(admin == "France")


ggplot(map) +
  geom_sf() +
  ggtitle("Basic Map") +
  theme_minimal()


ggplot() +
  geom_sf(data = map) +
  geom_sf(data = ocean, color = "black", size = 0.05, fill = "white") +
  ggtitle("English Channel") +
  xlim(-7, 2) +
  ylim(48, 52)+
  geom_sf(data = points,aes(geometry = geometry,
                                  shape = var,
                                  color = var), size = 2)



###############################################################################


b_points <- data.frame(lon = OBSMER_BYC$LONG_DEB_OP, lat = OBSMER_BYC$LAT_DEB_OP, taxa = OBSMER_BYC$ESPECE)
selected_birds <- c("Alca torda", "Uria aalge", "Phalacrocorax aristotelis", "Phalacrocorax carbo",
                      "Morus bassanus", "Larus marinus", "Larus argentatus", "Gavia immer", "Gavia stellata")
b_points <- b_points[b_points$taxa %in% selected_birds, ]
b_points <- st_as_sf(b_points, coords = c(1, 2), 
                   crs = '+proj=longlat +datum=WGS84 +ellps=WGS84')




map <- ne_countries(scale = 10, returnclass = "sf")
ocean <- ne_download(scale = 10, type = 'ocean', 
                     category = 'physical', returnclass = 'sf')



ggplot(map) +
  geom_sf() +
  ggtitle("Basic Map") +
  theme_minimal()

shape_values <- c(16, 17, 18, 19, 20, 21, 22, 23, 24)  # Adjust the shape values

ggplot() +
  geom_sf(data = map) +
  geom_sf(data = ocean, color = "black", size = 0.05, fill = "white") +
  ggtitle("English Channel") +
  xlim(-7, 1) +
  ylim(48, 50.5)+
  geom_sf(data = b_points,aes(geometry = geometry,
                            shape = taxa,
                            color = taxa), size = 2)+
  scale_shape_manual(values = shape_values)






ggplot() +
  geom_sf(data = map) +
  geom_sf(data = ocean, color = "black", size = 0.05, fill = "white") +
  ggtitle("English Channel") +
  xlim(-5.5, -3) +
  ylim(47.5, 49)+
  geom_sf(data = b_points,aes(geometry = geometry,
                              shape = taxa,
                              color = taxa), size = 2)+
  scale_shape_manual(values = shape_values)

































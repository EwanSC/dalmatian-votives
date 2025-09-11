# votives from datasets
# started 10/09/2025
# last edit 11/09/2025
# last ran 11/09/2025
library(dplyr)
library(data.table)

# EDH
EDH_Dalmatia <- read.csv("output_data/dalmatia/EDH_Dalmatia.csv")
EDH_votives <- EDH_Dalmatia %>%
  filter(i_gattung_sort_de %in% c("22"))
write.csv(EDH_votives,
          file = "output_data/votives/EDH_votives.csv")

# EDCS
EDCS_Dalmatia <- read.csv("output_data/dalmatia/EDCS_Dalmatia.csv")
EDCS_votives <- EDCS_Dalmatia %>% dplyr::filter(status %like% "tituli sacri")
write.csv(EDCS_votives,
          file = "output_data/votives/EDCS_votives.csv")

# LIRE
LIRE_Dalmatia <- read.csv("output_data/dalmatia/LIRE_Dalmatia.csv")
LIRE_votives <- LIRE_Dalmatia %>%
  filter(type_of_inscription_auto %in% c("votive inscription"))
write.csv(LIRE_votives,
          file = "output_data/votives/LIRE_votives.csv")

# count
EDH_votives_count <- count(EDH_votives)
EDCS_votives_count <- count(EDCS_votives)
LIRE_votives_count <- count(LIRE_votives)

EDH_votives_count$database <- "EDH"
EDCS_votives_count$database <- "EDCS"
LIRE_votives_count$database <- "LIRE"

count_list <- list(LIRE_votives_count, EDCS_votives_count, EDH_votives_count)      

database_votives_count <- Reduce(function(x, y) merge(x, y, all=TRUE), count_list) 

write.csv(database_votives_count,
          file = "output_data/votives/database_votives_count.csv")

# plot
library(ggplot2)
library(sf)
library(ggrepel)
library(stringr)
library("gridExtra")
library("rnaturalearthdata")
library("rnaturalearth")
library("rnaturalearthhires")
library("ggspatial")

# download rnaturalearthhires for basemap
world <- ne_countries(scale = "large", returnclass = "sf")

# adding road and province data from DARMC https://hub.arcgis.com/datasets/55a54a1350e14ca0b355d95633da3851_0
roman_roads <- st_read(
  "shape_files/Roman_roads.shp")

roman_provincess <- st_read(
  "shape_files/roman_empire_ad_69_provinces.shp")

key_civ_sites <- data.frame(findspot_ancient_clean=c("Narona",
                                                     "Iader",
                                                     "Salona"),
                            Longitude=c(17.625,
                                        15.223778,
                                        16.483426),
                            Latitude=c(43.0801,
                                       44.115501,
                                       43.539561))

print(key_civ_sites)

(key_sites_civ_ll <- st_as_sf(key_civ_sites,
                              coords = c("Longitude",
                                         "Latitude"),
                              remove = FALSE,
                              crs = 4326, agr = "constant"))
#EDH
EDH_votives_place <- na.omit(EDH_votives %>%
                               select(fo_antik,
                                      koordinaten1) %>%
                               group_by(fo_antik) %>%
                               count(fo_antik,
                                     koordinaten1) %>%
                               arrange(desc(n)))
EDH_votives_place[c('latitude', 'longitude')] <- str_split_fixed(EDH_votives_place$koordinaten1, ',', 2)
(EDH_votives_ll <- st_as_sf(EDH_votives_place, coords = c("longitude", "latitude"),
                            remove = FALSE,
                            crs = 4326, agr = "constant"))

EDH_votives_n <- count(EDH_votives)

plot1 <- 
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = EDH_votives_ll, aes(size = n), alpha=0.8) +
  geom_sf(data = key_sites_civ_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_civ_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(  -0.75,  -0.75,  -0.75), 
                  nudge_y = c(-0.5,-0.5,-0.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       EDH_votives_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDH (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0)."),
       title = "Distribution of votive 'titsac' (tituli sacri)",
       subtitle = "EDH") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot1)

ggsave("output_images/EDH_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

#EDCS
EDCS_votives_place <- na.omit(EDCS_votives %>%
                                 select(place,
                                        longitude,
                                        latitude) %>%
                                 group_by(place) %>%
                                 count(place,
                                       longitude,
                                       latitude) %>%
                                 arrange(desc(n)))

(EDCS_votives_ll <- st_as_sf(EDCS_votives_place, coords = c("longitude", "latitude"),
                              remove = FALSE,
                              crs = 4326, agr = "constant"))

EDCS_votives_n <- count(EDCS_votives)

plot2 <-
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = EDCS_votives_ll, aes(size = n), alpha=0.8) +
  geom_sf(data = key_sites_civ_ll, colour = "#000000", size = 1) +
  geom_text_repel(data = key_sites_civ_ll,
                  aes(x = Longitude,
                      y = Latitude,
                      label = findspot_ancient_clean), 
                  nudge_x = c(  -0.75,  -0.75,  -0.75), 
                  nudge_y = c(-0.5,-0.5,-0.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       EDCS_votives_n$n,
                       sep = "",
                       ".\nEpigraphic data = EDCS.\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0)"),
       title = "Distribution of tituli sacri",
       subtitle = "EDCS") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot2)

ggsave("output_images/EDCS_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)

# LIRE
LIRE_votives_place <- na.omit(LIRE_votives %>%
                               select(findspot_ancient_clean,
                                      Longitude,
                                      Latitude) %>%
                               group_by(findspot_ancient_clean) %>%
                               count(findspot_ancient_clean,
                                     Longitude,
                                     Latitude) %>%
                               arrange(desc(n)))
(LIRE_votives_ll <- st_as_sf(LIRE_votives_place, coords = c("Longitude", "Latitude"),
                            remove = FALSE,
                            crs = 4326, agr = "constant"))

LIRE_votives_n <- count(LIRE_votives)

plot3 <- 
  ggplot() + 
  geom_sf(data = world, color = "#c9c9c9", fill = "#e4e4e4") + 
  geom_sf(data = roman_roads, colour = "#a1a1a1", size = 0.6) +
  geom_sf(data = LIRE_votives_ll, aes(size = n), alpha=0.8) +
  geom_sf(data = key_sites_civ_ll, colour = "#000000", size = 0.5) +
  geom_label_repel(data = key_sites_civ_ll,
                   fill = "white",
                   aes(x = Longitude,
                       y = Latitude,
                       label = findspot_ancient_clean), 
                   nudge_x = c(  -0.75,  -0.75,  -0.75), 
                   nudge_y = c(-0.5,-0.5,-0.5)) +
  labs(size = "Density",
       caption = paste("n = ",
                       LIRE_votives_n$n,
                       sep = "",
                       ".\nEpigraphic data = LIRE v.3.0 (CC BY 4.0).\n",
                       "Roads = DARMC (CC BY-NC-SA 4.0)."),
       title = "Distribution of votive inscriptions",
       subtitle = "LIRE") +
  coord_sf(default_crs = st_crs(4326), xlim = c(14, 21), ylim = c(41.5, 46)) +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +
  annotation_scale() +
  theme_void()

plot(plot3)

ggsave("output_images/LIRE_scatter.jpeg",
       width = 180, height = 140, unit = "mm", dpi = 600)
#### FEO indicators / Biodiversity.fi indicators / Rivers without barriers / Indicator processing #####################################################
# Author: Peter Kullberg, peter.kullberg@syke.fi                                                                                                      #
#                                                                                                                                                     #
# This code is intended to be run after src/data.R. It processes the input data and outputs two tables containing the indicator values and            #
# a map showing the river network and accessibility of each river                                                                                     #
# For full description see src/Rivers_without_barriers_indicator.R                                                                                    #
#######################################################################################################################################################

#libraries
library(sf)
library(dplyr)
library(lwgeom)
library(igraph)
library(units)

# load some functions
source("src/helpper_functions.R")


# Load data 
rivers <- st_read("data/VHS/VHSjoki2016.shp")
river_network <- st_read("data/Uoma10.shp") 
node_endpoints <- st_read("data/Uomasolmupisteet.shp")
river_mouths <- node_endpoints %>% filter(pisteLuokk == "outlet")
dams <- st_read(dams, "data/dams.gpkg")

# Snap the dams to nodes of the rivers
# This will take a very long time  (several hours with an ordinary laptop)
dams_snapped <- snap_points(dams, river_network, 500)
dam_points <- st_geometry(dams_snapped) %>% st_is("POINT")
dams_snapped <- dams_snapped[dam_points, ]
# st_write(dams_snapped, "data/dams_snapped_500.gpkg")

# select rivers that are longer than 30 km (This is the approach taken by Hyytinen in his thesis) and snap dams to it for cutting
rivers_30km <- rivers %>%  filter(Shape_len > 30000)
dams_snapped_r30 <- snap_points(dams, rivers_30km, 500)
# st_write(dams_snapped_r30, "data/dams_snapped_500_r30.gpkg")

# Identify parts of river network that are reachable from the sea trough different types of migration barriers
# free flowing rivers
river_network_free <- select_connected_to_sea(river_network, dams_snapped, river_mouths)

# rivers that can be reached trough fishways
fish_ways <- dams_snapped %>% filter(KalojenVaellusesteellisyys != "kalatie")
river_network_fish_ways <- select_connected_to_sea(river_network, fish_ways, river_mouths)

# rivers that can be reach trough fishways or partial barriers
fish_ways_or_partials <- dams_snapped %>% filter(KalojenVaellusesteellisyys != "kalatie", KalojenVaellusesteellisyys != "osittainen este")
river_network_fish_ways_partials <- select_connected_to_sea(river_network, fish_ways_or_partials, river_mouths)

# st_write(river_network_free, "data/river_network_free.gpkg")
# st_write(river_network_fish_ways, "data/river_network_fish_ways.gpkg")
# st_write(river_network_fish_ways_partials, "data/river_network_fish_ways_partials.gpkg")


# optionally load old data to speed up analysis 
# dams_snapped <- st_read("data/dams_snapped_500.gpkg")
# dams_snapped_r30 <- st_read("data/dams_snapped.gpkg")
# river_network_free <- 
# river_network_fish_ways <- 
# river_network_fish_ways_partials <- 


# Use only river stretches that are longer than 30 km (his is how the original indicator was done, personally I would use all rivers)
rivers_30km_split <- split_and_link(rivers_30km, dams_snapped_r30) %>% st_sf() %>% mutate("accessibility" = "blocked")

# Mark the river stretches by their accessibility (following  priority order defined by Hyytinen where fishways take priotity over partial migration barriers)
rivers_30km_split <- annotate_rivers(rivers_30km_split, river_network_fish_ways_partials, annot = "fisway_and_partial")
rivers_30km_split <- annotate_rivers(rivers_30km_split, river_network_fish_ways, annot = "through_fisway")
rivers_30km_split <- annotate_rivers(rivers_30km_split, river_network_free, annot = "free")

# Write the river file
st_write(rivers_30km_split, "outputs/rivers_30km_accessibility.gpkg", delete_dsn = T)

### compute basic output tables ############################################################################################################################

## calculate lengths and tabulate
rivers_30km_split$river_length <- st_length(rivers_30km_split)
result_table <- rivers_30km_split %>% st_drop_geometry() %>%
  group_by(accessibility) %>%
  summarise(river_length = sum(river_length)) %>%
  ungroup() %>%
  mutate(pr_tot =  river_length / sum(river_length),
         type = factor(accessibility, levels = c("free", "through_fisway", "fisway_and_partial", "blocked")))

# Write the table showing summed lengths of different rivertypes
write_csv(result_table, "rivers_without_barriers_pub/pub_outputs/river_lengths_b.csv")

# Table of migration barrier types
dam_types <- dams_snapped %>% st_drop_geometry() %>%
  group_by(KalojenVaellusesteellisyys) %>% summarise(n = n()) %>%
  ungroup() %>% mutate(type = factor(KalojenVaellusesteellisyys, levels = c("ei tietoa", "kalatie", "osittainen este", "totaalinen este")),
                       pr_tot =  n / sum(n))

write_csv(result_table, "rivers_without_barriers_pub/pub_outputs/barrier_types.csv")

############################################################################################################################################
### This is a proof-of-concept version of automated computation of the Rivers without barriers indicator 
### published in the biodiversity.fi website (https://www.biodiversity.fi/en/habitats/inland-waters/iw11-rivers-without-barriers).
### The aim is to automate the production of the indicator using only opensource software and publicly availAble data.
### The computation of the original indicator  was described by Reima Hyytiäinen in his masters thesis work
### "SUOMEN JOKIEN PADOT JA KALATIET 1900-2017 - AIKASARJA KALOILLE VAELLUSKELPOISTEN JOKIUOMIEN MUUTOKSISTA" (https://erepo.uef.fi/handle/123456789/21543, http://urn.fi/urn:nbn:fi:uef-20190863)
### The method used here is differtent from the original publication, but is designed to produce as similar results as possible (although not identical).
### The focus here is in the technical proof-of-concept and the resutls have not been double-checked for errors and thus should not be taken as true description of the situation. 
### This work was inspired by the development of the Finish Ecosystem Observatory (FEO, www.syke.fi/hankkeet/feo.fi)
### The code takes several hours (roughly 8 hours with my laptop i5-8265U CPU @ 1.60GHz (currentlyu single core only), 16 GB RAM) to run trough with full data. 
### Author: peter.kullberg@syke.fi 
############################################################################################################################################

#### Libraries #### 
library(sf)
library(dplyr)
library(geojsonsf)
library(mapview) # not really needed in production 
library(lwgeom)
library(igraph)
library(units)

# #### Remove # to run smaller test version #####
# test_crop <- function(x) {
#   x %>% st_crop(xmin = -535435, xmax = 564564,
#                 ymin = 7214895 , ymax = 7314895)
# }

## Basic functions
# Snaps points to lines and removes any point outside max_dist. 
# Takes in sf objects. x = points, y = line, max_dist = maximum distance for snapping (in meters).
snap_points <- function(x, y, max_dist = 500) {
  
  # only the node points of the lines are considered. This sacrefices some accuracy, but make data easier and faster to manage. 
  y <- st_cast(y, "POINT") %>% st_union()
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out <- do.call(rbind,
                lapply(seq(n), function(i) {
                  geomi <- st_geometry(x)[i]
                  bbi <- st_bbox(c(xmin = geomi[[1]][1] - max_dist,
                                   xmax = geomi[[1]][1] + max_dist,
                                   ymin = geomi[[1]][2] - max_dist,
                                   ymax = geomi[[1]][2] + max_dist))    
                  yc <- st_crop(y, bbi) 
                  if(length(yc) == 0) return(NULL)
                  nrst <- st_nearest_points(geomi, yc)
                  nrst_len <- st_length(nrst)
                  nrst_mn <- which.min(nrst_len)
                  nrst_pt <- st_cast(nrst[nrst_mn], "POINT")[2]
                  return(st_sf(st_drop_geometry(x[i, ]), nrst_pt))
                })
  )
  
  # return only points within max distance
  is_point <- st_geometry(out) %>% st_is("POINT")
  if(sum(!is_point) != 0) warning(paste0(sum(!is_point), "points were outside maximum distance and were removed." ))
  
  return(out[is_point, ])
}

# Splits lines using points that have been snapped to the node points on the line (with snap_points)
# x = line, y = points snapped to x indicating locations where the lines are split
split_and_link <- function(x, y) {
  
  linesplit <- st_split(x, y) %>% st_collection_extract('LINESTRING')
  lineint <- st_intersects(linesplit)
  pointint <- y %>% st_cast("POINT") %>%  st_intersects(linesplit)
  
  for(i in pointint){
    for(j in i) {
      lineint[[j]] <- lineint[[j]][!lineint[[j]] %in% i]
    }
  }
  
  intersecting_lines <- lineint %>% graph.adjlist() %>% components()
  
  multilinestring_list <- split(linesplit, intersecting_lines$membership) %>%
    map(st_geometry) %>%
    map(st_multilinestring)
  
  grouped_multilinestring <- do.call(st_sfc, multilinestring_list) %>% st_set_crs(st_crs(x))  %>% st_sf()
  
  return(grouped_multilinestring)
}

# Selects intersecting objects
select_intersecting <- function(x, y, negate = FALSE){
  touches <- st_intersects(x, y)
  touches_index <- sapply(touches, FUN = function(x)any(!is.na(x)))
  
  if(negate == TRUE) {
    return(x[!touches_index, ])
  } else {
    return(x[touches_index, ])
  }
}

# Select parts of rivernetworks that are connected to sea 
select_connected_to_sea <- function(river_network, selected_dams, mouths) {
  
  # split by dams
  river_network_split <- split_and_link(river_network, selected_dams)
  
  # remove rivermouths with dams  
  river_mouths_no_dams <- select_intersecting(mouths, selected_dams, negate = TRUE)
  
  # Select parts of river network that are connected to sea
  river_network_full_barriers_sea <- select_intersecting(river_network_split, river_mouths_no_dams)
}

# mark which rivers are overlapping the network
annotate_rivers <- function(origd, rivs, annot, acc_col = "accessibility") {
  ints <- st_intersects(origd, rivs)
  ints_index <- sapply(ints, FUN = function(x) any(!is.na(x)))
  origd[[acc_col]][ints_index] <- annot
  return(origd)
}


####   Load raw data   ##################################################################################################################################
# All data are provided by the Finnish Environment Institute SYKE (www.syke.fi) and are open and freely available.
# The River and coastal data are part of Ranta10 data set (https://www.avoindata.fi/data/fi/dataset/ranta10-rantaviiva-1-10-000).
# River network is based on the topographic database of the National Land Survey of Finland in scale of 1:5 000-1:10 000 from years 2000-2008.
# The continuous network has been created in Finnish Environment Institute (SYKE) by combining rivers to the central lines through polygon rivers
# and lakes (https://www.avoindata.fi/data/fi/dataset/ranta10-rantaviiva-1-10-000-ja-uomaverkosto).
# The data about the dams and fishways is also provided by SYKE. The stability of the Esri API is not quaranteed  (https://ckan.ymparisto.fi/dataset/vesistotyot-vesty-rakenteet-ja-toimenpiteet).
######################################################################################################################################################

## 1. Load river data usnig URL pointing to the zip-file in SYKEs download service (API is too slow for the whole data set)
temp_rivers <- paste0(tempdir(), "temp_river.zip")
download.file("http://wwwd3.ymparisto.fi/d3/gis_data/spesific/ranta10joet.zip", destfile = temp_rivers, mode = "wb")
unzip(temp_rivers, exdir = "data")

rivers <- st_read("data/VHS/VHSjoki2016.shp")

## 2. Load rivernetwork (now simply using zipped file, but this also available trough INSPIRE service)
temp_network <- paste0(tempdir(), "temp_network.zip")
download.file("https://wwwd3.ymparisto.fi/d3/gis_data/spesific/uomaverkosto.zip", destfile = temp_network)
unzip(paste0(temp_network), exdir = "data")

river_network <- st_read("data/Uoma10.shp") 
node_endpoints <- st_read("data/Uomasolmupisteet.shp")
river_mouths <- node_endpoints %>% filter(pisteLuokk == "outlet")

## 3. Load dams and fish ways trough Esri API
# Loop created because Esri API allows only 1000 rows to be load wiht one query
# Improvement possibility: Here I'm doing six rounds becasue I simply tested how many rounds are needed, but would be better to quere number of lines.

# Server error??
tmp <- tempfile()
for(i in 0:5) {
  ofs <- i * 1000
  base <- "https://paikkatieto.ymparisto.fi/arcgis/rest/services/Projektit/VESTY_VesistokunnostustoimenpiteetRakenteet/MapServer/16/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson&resultOffset="
  query <- paste0(base, ofs)
  download.file(query, tmp)
  dams_tmp <- geojson_sf(tmp) %>% st_transform(st_crs(rivers))
  if(i == 0){dams <- dams_tmp}
  if(i != 0){dams <- rbind(dams, dams_tmp)}
}

# remove dams tha are no blocking the river
dams <- dams %>% filter(Tila == "KÃ¤ytÃ¶ssÃ¤", KalojenVaellusesteellisyys != "ei este", !is.na(KalojenVaellusesteellisyys), !is.na(Tila))

# writing to disc isn't strictly needed
st_write(dams, "data/dams.gpkg", delete_dsn = T)

### Process the data ######################################################################################################################################
# To cut the lines dam points needs to be snapped to rivers 
# This will take a very long time  (~7 h)
dams_snapped <- snap_points(dams, river_network, 500)
dam_points <- st_geometry(dams_snapped) %>% st_is("POINT")
dams_snapped <- dams_snapped[dam_points, ]
st_write(dams_snapped, "data/dams_snapped_500.gpkg")

# select rivers that are longer than 30 km (This is the approach taken by Hyytinen in his thesis) and snap dams to it for cutting
rivers_30km <- rivers %>%  filter(Shape_len > 30000)
dams_snapped_r30 <- snap_points(dams, rivers_30km, 500)
st_write(dams_snapped_r30, "data/dams_snapped_500_r30.gpkg")

## Identify parts of river network that are reachable from the sea trough different types of dams
river_network_free <- select_connected_to_sea(river_network, dams_snapped, river_mouths)
fish_ways <- dams_snapped %>% filter(KalojenVaellusesteellisyys != "kalatie")
river_network_fish_ways <- select_connected_to_sea(river_network, fish_ways, river_mouths)
fish_ways_or_partials <- dams_snapped %>% filter(KalojenVaellusesteellisyys != "kalatie", KalojenVaellusesteellisyys != "osittainen este")
river_network_fish_ways_partials <- select_connected_to_sea(river_network, fish_ways_or_partials, river_mouths)

st_write(river_network_free, "data/river_network_free.gpkg")
st_write(river_network_fish_ways, "data/river_network_fish_ways.gpkg")
st_write(river_network_fish_ways_partials, "data/river_network_fish_ways_partials.gpkg")


# optionally load old data to speed up analysis 
dams_snapped <- st_read("data/dams_snapped_500.gpkg")
# dams_snapped_r30 <- st_read("data/dams_snapped.gpkg")
# river_network_free <- 
# river_network_fish_ways <- 
# river_network_fish_ways_partials <- 

## Identify accessible parts of the rivers longer than 30 km
# Cut rivers using dams
## Here I added mutate column cause the samething ins ide sf function did not work anymore
rivers_30km_split <- split_and_link(rivers_30km, dams_snapped_r30) %>% st_sf() %>% mutate("accessibility" = "blocked")

# Mark the river by accessibility (following  priority order defined by Hyytinen where fishways take priotity over partial migration barriers)
rivers_30km_split <- annotate_rivers(rivers_30km_split, river_network_fish_ways_partials, annot = "fisway_and_partial")
rivers_30km_split <- annotate_rivers(rivers_30km_split, river_network_fish_ways, annot = "through_fisway")
rivers_30km_split <- annotate_rivers(rivers_30km_split, river_network_free, annot = "free")

### compute basic outputs ############################################################################################################################
## calculate lengths and tabulate
rivers_30km_split$river_length <- st_length(rivers_30km_split)
result_table <- rivers_30km_split %>% st_drop_geometry() %>%
  group_by(accessibility) %>%
  summarise(river_length = sum(river_length)) %>%
  ungroup() %>%
  mutate(pr_tot =  river_length / sum(river_length),
         type = factor(accessibility, levels = c("free", "through_fisway", "fisway_and_partial", "blocked")))

# This is the outputtable 
write_csv(result_table, "rivers_without_barriers_pub/pub_outputs/river_lengths_b.csv")

# TAble of migration barreirtypes
dam_types <- dams_snapped %>% st_drop_geometry() %>%
  group_by(KalojenVaellusesteellisyys) %>% summarise(n = n()) %>%
  ungroup() %>% mutate(type = factor(KalojenVaellusesteellisyys, levels = c("ei tietoa", "kalatie", "osittainen este", "totaalinen este")),
                       pr_tot =  n / sum(n))

write_csv(result_table, "rivers_without_barriers_pub/pub_outputs/barrier_types.csv")


### Create some visualizations #######################################################################################################################
## Create a pie plot showing the proportions
ggplot(result_table, aes(x="", y = drop_units(pr_tot), fill = type))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#0070c0", "#7bba31",  "#ffff00", "#dbcdab"))

# Create a river map, with delineations
st_write(rivers_30km_split, "outputs/rivers_30km_accessibility.gpkg", delete_dsn = T)
mapview(list( rivers_30km_split, dams_snapped), zcol = list("accessibility", NULL))


### Known problems with the method ###################################################################################################################
# - This is fully experiemntal method that follows the aims of the theisis work (original indicator), but uses completely different methodology. Resemblance of the resuts is assumed, but not truly compared.
# - Rivers and dams outside Finald have not been accounted here (The "improved" data should be available somewhere)
# - Time series of the develpoment has not been implemented
# - The rivers are making loops around the dams, which is strage? Are they actually the fisways?
# - there are some small river that does not have entrance to the sea in the topology
# - Snapping the dams to the points in the rivers that are closest to it might generate inaccuracies. Especially now when the riverstreches and the netwrok is snapped separately and 


#### FEO indicators / Biodiversity.fi indicators / Rivers without barriers / Data processing ##########################################################
# Author: Peter Kullberg, peter.kullberg@syke.fi                                                                                                      #
#                                                                                                                                                     #
# This code fetches and processes all data that is needed to compute Rivers without barriers indicator of the Biodiversity.fi website                 #
# For full description of the process see Rivers_without_barriers_indicator.R                                                                         #
#                                                                                                                                                     #
# All data are provided by the Finnish Environment Institute SYKE (www.syke.fi) and are freely available.                                             #  
# The River and coastal data are part of Ranta10 data set (https://www.avoindata.fi/data/fi/dataset/ranta10-rantaviiva-1-10-000).                     #
# River network is based on the topographic database of the National Land Survey of Finland in scale of 1:5 000-1:10 000 from years 2000-2008.        #
# The continuous network has been created in Finnish Environment Institute (SYKE) by combining rivers to the central lines through polygon rivers     #
# and lakes (https://www.avoindata.fi/data/fi/dataset/ranta10-rantaviiva-1-10-000-ja-uomaverkosto).                                                   #
# The data about migration barriers is also provided by SYKE (https://ckan.ymparisto.fi/dataset/vesistotyot-vesty-rakenteet-ja-toimenpiteet).         #
# The stability of the Esri API is not guaranteed.                                                                                                    #
#######################################################################################################################################################

# Libraries
library(sf)
library(dplyr)
library(geojsonsf)
library(httr)
library(jsonlite)

# 1. Load river data
temp_rivers <- paste0(tempdir(), "temp_river.zip")
download.file("http://wwwd3.ymparisto.fi/d3/gis_data/spesific/ranta10joet.zip", destfile = temp_rivers, mode = "wb")
unzip(temp_rivers, exdir = "data")

# 2. Load river network data
temp_network <- paste0(tempdir(), "temp_network.zip")
download.file("https://wwwd3.ymparisto.fi/d3/gis_data/spesific/uomaverkosto.zip", destfile = temp_network)
unzip(paste0(temp_network), exdir = "data")



# 3. Load migration barrier data
## Loop neede because Esri API allows only 1000 rows to be load with one query

## how mny rows?
damn <- GET("https://paikkatieto.ymparisto.fi/arcgis/rest/services/Projektit/VESTY_VesistokunnostustoimenpiteetRakenteet/MapServer/16/query?where=1%3D1&returnCountOnly=true&f=json")
dam_number <- content(damn) %>% fromJSON()

tmp <- tempfile()
for(i in 0:floor(dam_number$count/1000)) {
  ofs <- i * 1000
  base <- "https://paikkatieto.ymparisto.fi/arcgis/rest/services/Projektit/VESTY_VesistokunnostustoimenpiteetRakenteet/MapServer/16/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson&resultOffset="
  query <- paste0(base, ofs)
  download.file(query, tmp)
  dams_tmp <- geojson_sf(tmp) %>% st_transform(st_crs(rivers))
  if(i == 0){dams <- dams_tmp}
  if(i != 0){dams <- rbind(dams, dams_tmp)}
}

# remove dams that are no blocking the river
dams <- dams %>% filter(Tila == "KÃ¤ytÃ¶ssÃ¤", KalojenVaellusesteellisyys != "ei este", !is.na(KalojenVaellusesteellisyys), !is.na(Tila))

# write to disc
st_write(dams, "data/dams.gpkg", delete_dsn = T)






# Load river data set (https://ckan.ymparisto.fi/dataset/%7BAD287567-30F9-4529-9B47-2D6573FCAA9E%7D)
get_rvb_rivers <- function(source_url = "https://wwwd3.ymparisto.fi/d3/gis_data/spesific/VHSvesimuodostumat2022.zip",
                           destination = "data",
                           file_name = "VHSjoki2022.shp") {
  temp_rivers <- paste0(tempdir(), "temp_river.zip")
  download.file(source_url, destfile = temp_rivers, mode = "wb")
  unzip(temp_rivers, exdir = destination)
  st_read(paste0(destination, "/", file_name))
}

get_rvb_river_network <- function(source_url = "https://wwwd3.ymparisto.fi/d3/gis_data/spesific/uomaverkosto.zip", destination = "data", file_name_rivers = "Uoma10.shp", file_name_nodes = "Uomasolmupisteet.shp") {
  temp_network <- paste0(tempdir(), "temp_network.zip")
  download.file(source_url, destfile = temp_network, mode = "wb")
  unzip(paste0(temp_network), exdir = destination)
  
  list(rivers = st_read(paste0(destination, "/", file_name_rivers)), end_points = st_read(paste0(destination, "/", file_name_nodes)))
}

get_rvb_blocked_outlying_endpoints <- function() {
  st_read("data/not_fin_blocked_mouth.gpkg")
}

get_blocked_ru_outlets <- function() {
  st_read("data/blocked_ru_outlets_according_to_hyytia.gpkg") %>% mutate(ru_blocked_outlet = TRUE) %>% select(ru_blocked_outlet)
}

# 3. Load migration barrier data
get_rvb_dams <- function(source_url = "https://paikkatieto.ymparisto.fi/arcgis/rest/services/Projektit/VESTY_VesistokunnostustoimenpiteetRakenteet/MapServer/16") {
  
  # how many rows?
  damn <- GET(paste0(source_url, "/query?where=1%3D1&returnCountOnly=true&f=json"))
  dam_number <- content(damn)$count 
  
  # Create a loop to extract more than 1000 points
  tmp <- tempfile()
  for(i in 0:floor(dam_number/1000)) {
    ofs <- i * 1000
    base <- "https://paikkatieto.ymparisto.fi/arcgis/rest/services/Projektit/VESTY_VesistokunnostustoimenpiteetRakenteet/MapServer/16/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson&resultOffset="
    query <- paste0(base, ofs)
    download.file(query, tmp)
    dams_tmp <- st_read(tmp) # %>% st_transform(st_crs(rivers))
    if(i == 0){dams <- dams_tmp}
    if(i != 0){dams <- rbind(dams, dams_tmp)}
  }
  dams
}

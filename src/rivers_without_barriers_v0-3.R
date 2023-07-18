############################################################################################################################################
### This is a proof-of-concept version the Rivers without barriers indicator  
### published on the biodiversity.fi website (https://www.biodiversity.fi/en/habitats/inland-waters/iw11-rivers-without-barriers).
### The aim is to automate the production of the indicator using only open source software and publicly available data.
### The computation of the indicator was originally described by Reima Hyytiäinen in his masters thesis work
### "SUOMEN JOKIEN PADOT JA KALATIET 1900-2017 - AIKASARJA KALOILLE VAELLUSKELPOISTEN JOKIUOMIEN MUUTOKSISTA" (2019) (https://erepo.uef.fi/handle/123456789/21543, http://urn.fi/urn:nbn:fi:uef-20190863)
### The method here differs from the original publication, but is designed to produce as similar results as possible (although not identical, due to some technical choices).
###  
### This work was inspired by the development of the Finish Ecosystem Observatory (FEO, www.feofinland.fi).
### Here the focus is on the technical proof-of-concept.
### This code could be used as a starting point for a stable version of the Rivers without barriers indicator of Luonnontila.fi. 
### but before that it should be double-checked for errors and some arbitrary choices should be reviewed with experts.
### The code takes roughly an hour to run on my lap top (i5-8265U CPU @ 1.60GHz, 16 GB RAM; data download not included). 
### Author: peter.kullberg@syke.fi

# Some changes compared to Hyytiä 2019:
#   - In Hyytiä 2019 tolerance of 500m was used to snap the dam points to the rivers lines, but here I used 100m. In my visual inspection this seemed to work better.
#   - Automatic snapping will definitely cause some errors to the data. For example dams at the banks of the rivers are considered to be in the the river itself (for example in Kemijoki) 
#   - In Hyytiä 2019 if the construction year of a dam was unknown it was assigned year manually so that the final result looked smoother. 
#     Here I simply considered the dams to be either built on the last year of the analysis or before 1900. The true value lies somewhere between these extremes.
############################################################################################################################################

library(igraph)
library(tidygraph)
library(sfnetworks)
library(sf)
library(geojsonsf)
library(purrr)
library(dplyr)
library(stringr)
library(units)
library(httr)
library(ggplot2)


# functions for loading data
source("src/data_functions.R")

# functions needed in the data analysis
source("src/graph_functions.R")

# some helper function
source("src/helpper_functions.R")


####   Load raw data   ##################################################################################################################################
# All data sets are provided by the Finnish Environment Institute SYKE (www.syke.fi) and are freely available.
# The River and coastal data are part of Ranta10 data set (https://www.avoindata.fi/data/fi/dataset/ranta10-rantaviiva-1-10-000).
# River network is based on the topographic database of the National Land Survey of Finland in scale of 1:5000-1:10000 from years 2000-2008.
# The continuous network has been created in Finnish Environment Institute (SYKE) (https://www.avoindata.fi/data/fi/dataset/ranta10-rantaviiva-1-10-000-ja-uomaverkosto).
# The data about the dams and fishways is also provided by SYKE. The stability of the Esri API is not guaranteed (https://ckan.ymparisto.fi/dataset/vesistotyot-vesty-rakenteet-ja-toimenpiteet).
######################################################################################################################################################

# Load river data
options(timeout=200)
rivers <- get_rvb_rivers()

# Load river network
rivers_and_nodes <- get_rvb_river_network()
river_network <- rivers_and_nodes$rivers
end_points <- rivers_and_nodes$end_points

river_mouths_all <- end_points %>% filter(pisteLuokk == "outlet")

# The original dam data set covers only Finland. Here data about rivers in Russia, Norway and Sweden are added. 
# Data is created manually based on the information give in Hyytiä 2019 (especially FIG2). Further development of the data management is needed.
exclude_mouths <- get_rvb_blocked_outlying_endpoints()
river_mouths <- select_intersecting(river_mouths_all, exclude_mouths, negate = T)
blocked_ru_outlets <- get_blocked_ru_outlets()

# load dams
dams_raw <- get_rvb_dams()



# Filter dams that are not in use or are demolished
# Some arbitrary choices made:
# Handling of NAs in data describing how much dams hinder movement (KalojenVaellusesteellisyys): currently I rely on precautionary principle and consider NAs as fully blocked 
# Handling of NAs in state of the dam (Tila): currently I rely on precaution principle and consider that dams with NA state are in use 
# Note: There seems to be dams that are fully blocking the fish migration, but have "not in use" or "demolished" as state? 
dams_filt <- dams_raw %>% filter(Tila == "Käytössä", KalojenVaellusesteellisyys != "ei este", !is.na(KalojenVaellusesteellisyys), !is.na(Tila)) %>% 
  mutate(Nimi = str_to_lower(Nimi),
         KalojenVaellusesteellisyys = ifelse(is.na(KalojenVaellusesteellisyys), "totaalinen este", KalojenVaellusesteellisyys))

# Corrected status data for some dams collected manually from Hyytiä 2019. Data management of this part needs to be improved.
corrected_years <- read.csv2("data/Hyytiä_dams_years_corrected.csv", encoding = 
                               "latin1") %>% 
  mutate(Pato = str_replace_all(Pato, " $|-", ""),
         Pato = str_to_lower(Pato)) %>% 
  rename(KayttoonottoVuosi_orig = Käyttöön.ottovuosi.patorekiste.rissä,
         KayttoonottoVuosi_uusi_pato = Aikasarjaa.varten.kor.jattu.padon.rakentamis.vuosi,
         KayttoonottoVuosi_uusi_kalatie = Aikasarjaa.varten.kor.jattu.kala.tien.rakenta.misvuosi) %>% 
  select(-Lisätietoja)

dams <- dams_filt %>% left_join(corrected_years, c("Nimi" = "Pato")) %>%
  mutate(KayttoonottoVuosi = if_else(KayttoonottoVuosi != KayttoonottoVuosi_uusi_pato & !is.na(KayttoonottoVuosi_uusi_pato),
                                     KayttoonottoVuosi_uusi_pato, KayttoonottoVuosi)) %>% 
  st_transform(crs = st_crs(rivers))


# writing the "corrected" data to disc isn't strictly needed
st_write(dams, "data/dams_cut.gpkg", delete_dsn = T)


### Data manipulation ##################
### The analysis is done using graph approach mostly because it was fun to do 
### This part might also be done more effectively.

# rivers to graphs
net_graph <- river_network %>% st_cast("LINESTRING") %>% as_sfnetwork()

rivers_nodes <- rivers  %>% st_cast("LINESTRING") %>%
  as_sfnetwork() %>% 
  activate("nodes") %>% 
  st_as_sf() %>% mutate(is_river_node = T) %>% select(is_river_node)

# The rivers that are longer than 30 km are marked to mimic the Hyytiä 2019. 
# I believe that he focused on rivers that are longer than 30 km to speed up the computation, although he doesn't open this choice in the thesis 
# This code works also with full data, so I personally would use all rivers in the analysis
rivers_edges <- rivers  %>% st_cast("LINESTRING") %>%
  as_sfnetwork() %>% 
  activate("edges") %>% 
  st_as_sf() %>%
  mutate(is_river = T, river_length = st_length(geometry), is_river_30km = (river_length > set_units(30, km))) %>%
  select(is_river, is_river_30km)

river_graph <- net_graph %>% activate("edges") %>% st_network_blend(rivers_nodes) %>% st_join(rivers_edges, join = st_intersects) %>%
  filter(!edge_is_multiple())

# Join dams to  network as nodes. Snaps dams to closest point in the river network with 100 m tolerance.
# The original study used 500m tolerance, but this causes problems for example at the mouth of Kemijoki where dams at the banks of the rivers are snapped to river line etc
# The original study said that the 500 limit was chosen based on visual inspection with a aim that no dams should be dropped off. 
# To me it seems that with 500m snapping distance some dams that are actually located in small ditches near the rivers are snapped to the riverline itself.
# Therefore I chose to use 100m distance, but this should be reconsidered before using the indicator!!!
river_graph_and_dams <- st_network_blend(river_graph, dams, tolerance = 100) %>% activate("edges") %>% mutate(weight = edge_length())

# Join outlet info
river_graph_dams_outlets <- river_graph_and_dams %>% activate("nodes") %>%
  st_join(river_mouths) %>%
  st_join(blocked_ru_outlets) %>% 
  mutate(KalojenVaellusesteellisyys = ifelse(!is.na(ru_blocked_outlet), "totaalinen este", KalojenVaellusesteellisyys))


# Identify and mark rivers (edges) that will be used in cutting the network. Practically these are the edges behind different types of dams
river_graph_dams_outlets <- river_graph_dams_outlets %>%
  activate("edges") %>% 
  mutate(fishway_year = .N()$KayttoonottoVuosi_uusi_kalatie[to],
         cut_type = .N()$KalojenVaellusesteellisyys[to],
         status_year = .N()$KayttoonottoVuosi[to])

# Identify rivers that are connected to sea
river_network_f <-
  river_graph_dams_outlets %>%
  morph(to_components) %>%
  activate("edges") %>%
  mutate(free_state = any(!is.na(.N()$pisteLuokk)),
         river_status = ifelse(free_state == T, "free", "not_connected")) %>%
  unmorph()

# this sets the evaluation interval. Now every 10th year from 1900 onwards
year_vect <- as.numeric(c(seq(1900, format(Sys.Date(), "%Y"), 10), format(Sys.Date(), "%Y")))

# results are computed both with and without dams that have unknown year of construction 
status_graphs <- year_vect %>% map(~ yearly_river_status(river_network_f, .x,  include_na_years = FALSE)) # 40 min
status_graphs_na <- year_vect %>% map(~ yearly_river_status(river_network_f, .x,  include_na_years = TRUE)) # 34 min

# Back to spatial data 
status_maps <- status_graphs %>% map(~ st_as_sf(.x, "edges") %>%
                                       select(river_status, status_year, uomaLuokka, weight, valtio, is_river, is_river_30km) %>% 
                                       mutate(status_year = as.numeric(status_year)))

status_maps_na <- status_graphs_na %>% map(~ st_as_sf(.x, "edges") %>%
                                             select(river_status, status_year, uomaLuokka, weight, valtio, is_river, is_river_30km) %>% 
                                             mutate(status_year = as.numeric(status_year)))

# write to disc
walk2(status_maps, year_vect, ~st_write(.x, paste0("outputs/rivernetwork_status_new_", .y, ".gpkg"), delete_dsn = T))
walk2(status_maps_na, year_vect, ~st_write(.x, paste0("outputs/rivernetwork_status_na_new", .y, ".gpkg"), delete_dsn = T))


### Data analysis and plotting ######################
# Finalize the data tables (Need to check if country filter == FI is OK. What happens for example to Teno and some cross border streams?)
# Few different result are computed
# 1. Account only rivers that are longer than 30 km (as was done in Hyytiä 2019). The difference to the original work is that instead of spreading the 
# 2. Use all rivers
# 3  Use all rivers, but exclude the dams with NA construction year

# Rivers 30 km only.Note: dams with NA construction year omitted! 
rivers_result_list_30km <- list()
for(i in seq_along(year_vect)){
  rivers_result_list_30km[[i]] <- status_maps[[i]] %>% st_drop_geometry() %>%
    filter(!is.na(is_river_30km), is_river_30km == T, valtio == "FI", uomaLuokka == "joki tai jokipseudo", river_status != "not_connected") %>%
    group_by(river_status) %>%
    summarise(length = sum(weight)) %>%
    mutate(pr = length / sum(length),
           year = year_vect[[i]])
}

rivers_result_table_30km <- bind_rows(rivers_result_list_30km) %>%
  mutate(river_status = factor(river_status, levels = c("blocked","partly_blocked", "fish_way", "free")))

ggplot(rivers_result_table_30km, aes(x = year, y = drop_units(pr), fill = river_status)) +
  geom_area(alpha = 0.6, col = "gray20") +
  scale_fill_manual(values = c("#dbcdab", "#7bba31",  "#ffff00", "#0070c0"))

write.csv2(rivers_result_table_30km, "outputs/rivers_result_table_30km.csv")

# All rivers but consider all dams with unknown construction year to be built before 1900. This is the "worst case scenario".
rivers_result_list_na <- list()
for(i in seq_along(year_vect)){
  rivers_result_list_na[[i]] <- status_maps_na[[i]] %>% st_drop_geometry() %>%
    filter(!is.na(is_river), valtio == "FI", uomaLuokka == "joki tai jokipseudo", river_status != "not_connected") %>%
    group_by(river_status) %>%
    summarise(length = sum(weight)) %>%
    mutate(pr = length / sum(length),
           year = year_vect[[i]])
}

rivers_result_table_na <- bind_rows(rivers_result_list_na) %>%
  mutate(river_status = factor(river_status, levels = c("blocked","partly_blocked", "fish_way", "free")))

write.csv2(rivers_result_table_na, "outputs/rivers_result_table_na.csv")

ggplot(rivers_result_table_na, aes(x = year, y = drop_units(pr), fill = river_status)) +
  geom_area(alpha = 0.6, col = "gray20") +
  scale_fill_manual(values = c("#dbcdab", "#7bba31",  "#ffff00", "#0070c0"))


# All rivers. 
# If the construction year of a dam is not know it is considered only in the last year of analysis.
rivers_result_list <- list()
for(i in seq_along(year_vect)){
  rivers_result_list[[i]] <- status_maps[[i]] %>% st_drop_geometry() %>%
    filter(!is.na(is_river), valtio == "FI", uomaLuokka == "joki tai jokipseudo", river_status != "not_connected") %>%
    group_by(river_status) %>%
    summarise(length = sum(weight)) %>%
    mutate(pr = length / sum(length),
           year = year_vect[[i]])
}


rivers_result_list[length(rivers_result_list)] <- rivers_result_list_na[length(rivers_result_list)]

rivers_result_table <- bind_rows(rivers_result_list) %>%
  mutate(river_status = factor(river_status, levels = c("blocked","partly_blocked", "fish_way", "free")))

ggplot(rivers_result_table, aes(x = year, y = drop_units(pr), fill = river_status)) +
  geom_area(alpha = 0.6, col = "gray20") +
  scale_fill_manual(values = c("#dbcdab", "#7bba31",  "#ffff00", "#0070c0"))

write.csv2(rivers_result_table, "outputs/rivers_result_table.csv")

# combined plot of the previous results. 
# In this graph the solid background colors shows the best possible scenario (dams with unknown year set the last year of the analysis)
# and the lines the worst case scenario (dams with unknown year built before 1900). 
# The true value lies somewhere between these extremes
# This is not very good graph but simply added here to show what can be said with available data
rivers_result_table_all <- rbind(rivers_result_table_na, rivers_result_table)

ggplot(rivers_result_table, aes(x = year, y = drop_units(pr), fill = river_status)) +
  geom_area(alpha = 0.6, col = "gray20") +
  scale_fill_manual(values = c("#dbcdab", "#7bba31",  "#ffff00", "#0070c0")) +
  geom_line(data = rivers_result_table_na,
            aes(x = year, y = drop_units(pr), col = river_status),
            lwd = 1.2, lty = 1, position = "stack") +
  scale_color_manual(values = c("#dbcdab", "#7bba31",  "#d4c604", "#0070c0"))

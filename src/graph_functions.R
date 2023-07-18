

# spatial ignoring duplicate columns in the second variable
sf_network_join_rm_duplicates <- function(in_sfnet_x, in_sfnet_y){
  # regular spatial join duplicates columns with same name (except geometry)
  # and remove the .y columns
  joined_sfnet <- st_network_join(in_sfnet_x, in_sfnet_y) %>% 
    activate("nodes") %>% 
    select(ends_with(".x"))
  
  #replace .x from the names (rename_with does not seem to work with tidygraphs)
  old_names <- names(st_as_sf(joined_sfnet, "nodes"))
  new_names <- str_replace(old_names, ".x", "")
  names(old_names) <- new_names
  
  joined_sfnet %>% activate("nodes") %>% rename(all_of(old_names)) %>% 
    activate("edges")
}


# check if river stretch is reachable from the see
annot_rivers_net <- function(in_net,
                             dam_type = "totaalinen este",
                             annot = "blocked",
                             cut_year = format(Sys.Date(), "%Y"),
                             include_na_years = TRUE) {
  
  if(include_na_years == TRUE) {
    fill_ins <- in_net %>% filter(((cut_type == !!dam_type &  (status_year <= cut_year | is.na(status_year))) & # This is the main logic of selecting dams with specific type and year
                                     (((fishway_year < cut_year) & dam_type == "kalatie") | is.na(fishway_year))) | # This lines additionally checks if the fishway was constructed after the dam (additional Data from Hyytiäs thesis)
                                    ((status_year < cut_year) & (fishway_year > cut_year) & dam_type == "totaalinen este")) %>% # And this assigns dams as blocked if they are labeled as fishways, but the fishway was constructed only later (according to Hyytiä) 
      mutate(temp_state = NA)
  }else{
    fill_ins <- in_net %>% filter(((cut_type == !!dam_type &  (status_year <= cut_year)) & # This is the main logic of selecting dams with specific type and year
                                     (((fishway_year < cut_year) & dam_type == "kalatie") | is.na(fishway_year))) | # This lines additionally checks if the fishway was constructed after the dam (additional Data from Hyytiäs thesis)
                                    ((status_year < cut_year) & (fishway_year > cut_year) & dam_type == "totaalinen este")) %>% # And this assigns dams as blocked if they are labeled as fishways, but the fishway was constructed only later (according to Hyytiä) 
      mutate(temp_state = NA)
  }
  
  in_net %>%
    mutate(temp_state = NA) %>%
    activate(edges) %>% filter(!from %in% st_as_sf(fill_ins, "edges")$from, !to %in% st_as_sf(fill_ins, "edges")$to) %>% # Filter away the cutting points 
    morph(to_components) %>%
    mutate(temp_state = any(!is.na(.N()$pisteLuokk))) %>%
    unmorph() %>%
    activate("nodes") %>%
    sf_network_join_rm_duplicates(fill_ins) %>%
    activate("edges") %>%
    mutate(river_status = ifelse(temp_state != T & river_status != "not_connected", annot, river_status),
           river_status = ifelse(is.na(temp_state), annot, river_status))
}

yearly_river_status <- function(in_net, years,
                                include_na_years = FALSE){
  in_net %>%
    annot_rivers_net(dam_type = "kalatie", annot = "fish_way", cut_year = years, include_na_years = include_na_years) %>% 
    annot_rivers_net(dam_type = "osittainen este", annot = "partly_blocked", cut_year = years, include_na_years = include_na_years) %>%
    annot_rivers_net(dam_type = "totaalinen este", annot = "blocked", cut_year = years,  include_na_years = include_na_years)
}

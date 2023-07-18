<<<<<<< HEAD

# takes in points (y) and lines (x) and snaps the points to the nodes of the line  
snap_points <- function(x, y, max_dist = 500) {
  
  # only the node points of the lines are considered. This sacrifices some accuracy, but make data easier and faster to manage. 
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

# Splits lines using points that have been snapped to the node points on the line (with snap_points function)
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

# Select parts of the river network that are connected to the sea 
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
=======
#### FEO indicators / Biodiversity.fi indicators / Rivers without barriers / processing fucntions #####################################################
# Author: Peter Kullberg, peter.kullberg@syke.fi                                                                                                      #

# takes in points (y) and lines (x) and snaps the points to the nodes of the line  
snap_points <- function(x, y, max_dist = 500) {
  
  # only the node points of the lines are considered. This sacrifices some accuracy, but make data easier and faster to manage. 
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

# Splits lines using points that have been snapped to the node points on the line (with snap_points function)
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

# Select parts of the river network that are connected to the sea 
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
>>>>>>> 3eb53cbc9356a8c37eb83cb6a3cb2606ce0966bf

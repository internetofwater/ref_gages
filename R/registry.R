build_registry <- function(gl, registry, providers) {
  
  reg <- read_csv(registry)
  
  gl <- left_join(gl, select(providers, provider_int = id, provider), by = "provider")
  
  gl <- select(convert_coords(gl), provider = provider_int, provider_id)
  
  if(nrow(reg) == 1) {
    message("initialize")
    
    reg$id <- as.numeric(reg$id)
    reg$provider <- as.numeric(reg$provider)
    gl$id <- 1000000 + c(1:nrow(gl))
    
  } else if(!all(paste0(gl$provider, gl$provider_id) %in% 
                 paste0(reg$provider, reg$provider_id))) {
    
    gl <- distinct(gl)
    
    if(any(duplicated(gl$provider_id))) stop("found duplicate ids")
    
    gl <- dplyr::filter(gl, 
                        !paste0(gl$provider, gl$provider_id) %in% 
                          paste0(reg$provider, reg$provider_id))
    
    gl$id <- seq(max(reg$id), (max(reg$id) + nrow(gl) - 1))
    
    message(paste("Adding", nrow(gl), "to the registry."))
    
  } else {
    
    return(reg)
    
  }
  
  bind_rows(reg, gl)
  
}

convert_coords <- function(gl) {
  coords <- st_coordinates(gl)
  gl$lon <- as.numeric(coords[, 1])
  gl$lat <- as.numeric(coords[, 2])
  
  st_drop_geometry(gl)
}

#' Get Formatted Registry
#' @description
#' Returns the complete current registry in expanded form including URI and 
#' subjectOf URL from the provider.
#' 
get_registry <- function(registry = "reg/ref_gages.csv",
                         providers = "reg/providers.csv") {
  
  reg <- read_csv(registry)
  pro <- read_csv(providers)
  
  left_join(reg, select(pro, id, provider_url = provider), 
                   by = c("provider" = "id")) |>
    select(-provider) |>
    mutate(uri = paste0("https://geoconnex.us/ref/gages/", id),
           subjectOf = paste0(case_when(
             provider_url == "https://waterdata.usgs.gov" ~ "https://waterdata.usgs.gov/monitoring-location/",
             provider_url == "https://cdec.water.ca.gov" ~ "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=",
             provider_url == "https://dwr.state.co.us" ~ "https://dwr.state.co.us/Tools/Stations/"
           ), provider_id))
  
}

#' @description builds a table of reference locations for gages in the registry
build_reference_location <- function(gl, reference_locations, registry, providers) {
 
  # all registry points from current run
  reg <- read_csv(registry)

  # get our current run gage locations into registry form
  gl <- left_join(gl, select(providers, provider_int = id, provider), by = "provider")

  # get locations into csv form
  loc <- distinct(select(convert_coords(gl), provider = provider_int, 
                         provider_id, lon = lon, lat = lat)) |>
    left_join(reg, by = c("provider", "provider_id"))
      
  # all existing known locations from last run
  existing_locations <- read_csv(reference_locations)
  
  # figure out which ones are new
  loc <- loc[!loc$id %in% existing_locations$id,]  

  # return if none
  if(nrow(loc) == 0) return(existing_locations)
  
  # else return all the old plus some new
  bind_rows(existing_locations, loc)
  
}
  

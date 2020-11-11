build_registry <- function(gage_list, registry, providers) {
  reg <- read_csv(registry)
  
  for(gl in gage_list) {
    gl <- left_join(gl, select(providers, provider_int = id, provider), by = "provider")
    
    gl <- select(convert_coords(gl), provider = provider_int, provider_id)
    
    if(nrow(reg) < nrow(gl)) {

      gl <- dplyr::filter(gl, 
                          !paste0(gl$provider, gl$provider_id) %in% 
                            paste0(reg$provider, reg$provider_id))
      
      gl$id <- seq(max(reg$id), (max(reg$id) + nrow(gl) - 1))
      
      message(paste("Adding", nrow(gl), "to the registry."))
      
    } else if(nrow(reg) == nrow(gl)) {
      return(reg)
    } else {
      reg$id <- as.numeric(reg$id)
      reg$provider <- as.numeric(reg$provider)
      gl$id <- 1000000 + c(1:nrow(gl))
    }
    
    reg <- bind_rows(reg, gl)
  }
  
  reg
  
}

convert_coords <- function(gl) {
  coords <- st_coordinates(gl)
  gl$lon <- coords[, 1]
  gl$lat <- coords[, 2]
  
  st_drop_geometry(gl)
}

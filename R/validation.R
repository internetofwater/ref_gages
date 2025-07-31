validate_ref_gage <- function(registry_csv, reference_file, 
                              reference_locations_csv, 
                              providers_lookup_csv) {
  
  registry <- read_csv(registry_csv)

  reference <- read_sf(reference_file)

  ref_locations <- read_csv(reference_locations_csv)    
  
  providers <- read_csv(providers_lookup_csv)
  
  if(any(!reference$id %in% registry$id)) {
    stop("missing ids in registry")
  }
  
  if(any(!reference$id %in% ref_locations$id)) {
    stop("missing ids in ref locations")
  }
  
  if(any(!reference$provider %in% providers$provider)) {
    stop("missing providers")
  }
  
  if(any(!registry$provider %in% providers$id)) {
    stop("registry has unknown providers")
  }
  
  if(any(sapply(names(registry), \(x) any(is.na(registry[[x]]))))) {
    stop("NAs not allowed in registry")
  }
  
  if(any(!is.na(reference$nhdpv2_COMID) & is.na(reference$nhdpv2_offset_m))) {
    stop("if a comid is identified it must have an offset")
  }
  
  if(any(!is.na(reference$nhdpv2_COMID) & is.na(reference$nhdpv2_REACH_measure))) {
    stop("if a comid is identified it must have a measure") 
  }
  
  if(any(!is.na(reference$nhdpv2_COMID) & is.na(reference$nhdpv2_link_source))) {
    stop("if a comid is identified it must have a link source") 
  }
  
  if(any(sf::st_is_empty(sf::st_geometry(reference)))) {
    stop("all geometry must not be empty")
  }
}
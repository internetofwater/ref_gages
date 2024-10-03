validate_ref_gage <- function(registry_csv, reference_file, 
                              reference_locations_csv, reference_out) {
  
  registry <- read_csv(registry_csv)

  reference <- read_sf(reference_file)

  ref_locations <- read_csv(reference_locations_csv)    
  
  
}
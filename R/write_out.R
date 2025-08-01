write_reference <- function(gage_hydrologic_locations, registry, providers, reference_file, nldi_file,
                            duplicate_locations) {

  duplicate_locations$cluster_string <- unlist(lapply(duplicate_locations$cluster_id, \(x) {
    if(is.null(x)) return("")
    
    if(all(is.na(x))) return("")
    
    out <- paste0("https://geoconnex.us/ref/gages/", x)
    
    out <- out[out != "https://geoconnex.us/ref/gages/NA"]
    
    out <- paste(out, collapse = ",")
    }))
  
  dup <- select(sf::st_drop_geometry(duplicate_locations), id, cluster_string) |>
    filter(cluster_string != "") |>
    mutate(uri = paste0("https://geoconnex.us/ref/gages/", id)) |>
    select(uri, cluster = cluster_string) |>
    distinct()
  
  out <- gage_hydrologic_locations %>%
    select(-id) |>
    mutate(identifier = paste0(provider, provider_id)) %>%
    distinct()
  
  if(any(duplicated(out$identifier))) stop("duplicate identifiers?")

  out$nws_url <- unlist(lapply(out$nws_url, \(x) {
    if(is.null(x)) return(NA_character_)
    paste(x, collapse = ",")
  }))
  
  out <- out |>
    left_join(select(convert_provider_id(registry, providers), 
                     uri, identifier, id), by = "identifier") %>%
    select(id, uri, name, description, subjectOf, 
           provider, provider_id, nhdpv2_REACHCODE, 
           nhdpv2_REACH_measure, nhdpv2_COMID, nhdpv2_totdasqkm, 
           nhdpv2_link_source, nhdpv2_offset_m,
           gage_totdasqkm = drainage_area_sqkm, 
           dasqkm_diff = da_diff, mainstem_uri, nws_url) %>%
    mutate(id = as.integer(id)) |>
    left_join(dup, by = "uri")
  
  write_sf(out, reference_file)
  
  unlink(nldi_file)
  write_sf(out, nldi_file)
  
  reference_file
}

write_usgs_reference <- function(gage_hydrologic_locations, registry, providers, usgs_reference_file, usgs_nldi_file) {
  out <- gage_hydrologic_locations %>%
    select(-id) |>
    mutate(identifier = paste0(provider, provider_id)) %>%
    left_join(select(convert_provider_id(registry, providers), 
                     uri, identifier, id), by = "identifier") %>%
    filter(provider == "https://waterdata.usgs.gov") %>%
    select(id, uri, name, description, subjectOf, provider, provider_id, 
           nhdpv2_REACHCODE, nhdpv2_REACH_measure, nhdpv2_COMID) %>%
    mutate(id = as.integer(id))
  
  out$id <- out$provider_id
  out$uri <- paste0("https://geoconnex.us/usgs/monitoring-location/", out$id)
  
  out$provider_id <- paste0("USGS-", out$provider_id)
  
  write_sf(out, usgs_reference_file)
  
  unlink(usgs_nldi_file)
  write_sf(out, usgs_nldi_file)
  
  out
}

write_registry <- function(registry, registry_file) {
  write_csv(registry, registry_file)
  
  registry_file
}

convert_provider_id <- function(registry, providers) {
  rename(registry, prov_id = provider) %>%
    left_join(select(providers, prov_id = id, provider), 
              by = "prov_id") %>%
    mutate(identifier = paste0(provider, provider_id)) %>%
    mutate(uri = paste0("https://geoconnex.us/ref/gages/", id)) %>%
    select(-prov_id)
}

build_index <- function(reference, index_dir) {
  states <- read_sf("https://reference.geoconnex.us/collections/states/items?f=json&limit=100")
  
  reference <- st_join(reference, select(states, state = id))
  
  for(st in states$id) {
    gages <- filter(reference, .data$state == st) %>%
      select(uri, description, subjectOf) %>%
      mutate(uri = make_link(uri), subjectOf = make_link(subjectOf))
    
    state <- states$NAME[states$id == st]
    
    st_id <- basename(st)
    
    if(nrow(gages) > 0) {
      rmarkdown::render("R/index_template.Rmd", 
                        output_file = file.path("../docs", paste0(st_id, ".html")), 
                        params = list(
                          name = state,
                          ref = gages
                        ))
    }
  }
}

make_link <- function(x) {
  paste0('<a href="', x, '">', x, '</a>')
}



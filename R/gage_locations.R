get_nwis_gage_locations <- function(nwis_gage, streamstats_sites) {
  
  gages <- dplyr::filter(nwis_gage, (site_tp_cd == "ST" | 
                                       site_tp_cd == "ST-CA" |
                                       site_tp_cd == "ST-DCH" |
                                       site_tp_cd == "ST-TS" |
                                       site_tp_cd == "ES" |
                                       site_tp_cd == "LK") & 
                           !is.na(dec_long_va) & 
                           !is.na(dec_lat_va)) %>%
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) %>%
    mutate(description = paste0("USGS NWIS Stream/River/Lake Site ", site_no, ": ", station_nm),
           subjectOf = paste0("https://waterdata.usgs.gov/monitoring-location/", site_no),
           uri = paste0("https://geoconnex.us/ref/gages/", n()),
           provider = "https://waterdata.usgs.gov",
           provider_id = site_no) %>%
    select(name = station_nm, 
           description,
           subjectOf,
           provider,
           provider_id)
  
  # streamstats_only <- dplyr::filter(streamstats_sites, !code %in% gages$provider_id)
}

get_hydrologic_locations <- function(all_gages, hydrologic_locations, nhdpv2_fline) {
  all_gages$nhdpv2_REACHCODE <- NA
  all_gages$nhdpv2_REACH_measure <- NA
  all_gages$nhdpv2_COMID <- NA
  
  for(hl in hydrologic_locations) {
    
    hl$locations <- hl$locations[hl$locations$provider_id %in% all_gages$provider_id, ]
    
    provider_selector <- all_gages$provider %in% hl$provider
    
    matcher <- match(hl$locations$provider_id,
                     all_gages$provider_id[provider_selector]
                     )
    
    all_gages$nhdpv2_REACHCODE[provider_selector][matcher] <- 
      hl$locations$nhdpv2_REACHCODE
    all_gages$nhdpv2_REACH_measure[provider_selector][matcher] <- 
      hl$locations$nhdpv2_REACH_measure
    all_gages$nhdpv2_COMID[provider_selector][matcher] <- 
      hl$locations$nhdpv2_COMID
  }
  
  no_location <- which(is.na(all_gages$nhdpv2_COMID))
  new_hl <- all_gages[no_location, ]
  
  new_hl <- nhdplusTools::get_flowline_index(nhdpv2_fline, 
                                             new_hl, 
                                             search_radius = 100)
  
  all_gages$nhdpv2_REACHCODE[no_location] <- new_hl$REACHCODE
  all_gages$nhdpv2_REACH_measure[no_location] <- new_hl$REACH_meas
  all_gages$nhdpv2_COMID[no_location] <- new_hl$COMID
  
  all_gages
}
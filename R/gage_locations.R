get_nwis_gage_locations <- function(nwis_gage, streamstats_sites) {
  
  sqmi_to_sqkm <- 2.58999
  
  gages <- dplyr::filter(nwis_gage, (site_tp_cd == "ST" | 
                                       site_tp_cd == "ST-CA" |
                                       site_tp_cd == "ST-DCH" |
                                       site_tp_cd == "ST-TS" |
                                       site_tp_cd == "ES" |
                                       site_tp_cd == "LK") & 
                           !is.na(dec_long_va) & 
                           !is.na(dec_lat_va)) %>%
    select(dec_lat_va, dec_long_va, site_no, station_nm, site_no, drain_area_va) %>%
    group_by(site_no) %>% arrange(drain_area_va) %>%
    filter(n() == 1) %>% ungroup() %>%
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) %>%
    mutate(description = paste0("USGS NWIS Stream/River/Lake Site ", site_no, ": ", station_nm),
           subjectOf = paste0("https://waterdata.usgs.gov/monitoring-location/", site_no),
           uri = paste0("https://geoconnex.us/ref/gages/", n()),
           provider = "https://waterdata.usgs.gov",
           provider_id = site_no,
           drainage_area_sqkm = (as.numeric(drain_area_va) * sqmi_to_sqkm)) %>%
    select(name = station_nm, 
           description,
           subjectOf,
           provider,
           provider_id,
           drainage_area_sqkm)
}

get_hydrologic_locations <- function(all_gages, hydrologic_locations, nhdpv2_fline,
                                     da_diff_thresh = 0.5, search_radius_m = 500,
                                     max_matches_in_radius = 5) {
  
  v2_area <- select(nhdplusTools::get_vaa(), 
                    nhdpv2_COMID = comid, 
                    nhdpv2_totdasqkm = totdasqkm)
  
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
  
  all_gages <- left_join(all_gages, v2_area, by = "nhdpv2_COMID")
  
  diff_da <- abs(all_gages$nhdpv2_totdasqkm -
                   all_gages$drainage_area_sqkm) / 
    all_gages$drainage_area_sqkm
  
  bad_da <- all_gages[!is.na(diff_da) & diff_da > da_diff_thresh, ]
  
  update_index <- which(is.na(all_gages$nhdpv2_COMID) | 
                          all_gages$provider_id %in% bad_da$provider_id)
  
  no_location <- all_gages[update_index, ]
  
  
  new_hl <- nhdplusTools::get_flowline_index(nhdpv2_fline, 
                                             no_location, 
                                             search_radius = search_radius_m, 
                                             max_matches = max_matches_in_radius)
  
  
  linked_gages <- st_drop_geometry(select(no_location, provider_id)) %>%
    mutate(id = seq_len(nrow(.))) %>%
    left_join(new_hl, by = "id") %>%
    left_join(select(st_drop_geometry(all_gages), 
                     provider_id, drainage_area_sqkm), 
              by = "provider_id") %>%
    left_join(v2_area, by = c("COMID" = "nhdpv2_COMID")) %>%
    mutate(da_diff = abs(drainage_area_sqkm - nhdpv2_totdasqkm))
  
  linked_gages_dedup <- bind_rows(
    linked_gages %>%
      group_by(provider_id) %>%
      filter(is.na(da_diff)) %>%
      filter(offset == min(offset)) %>%
      ungroup(), 
    linked_gages %>%
      group_by(provider_id) %>%
      filter(!is.na(da_diff)) %>%
      filter(da_diff == min(da_diff)) %>%
      ungroup()) %>%
    group_by(provider_id) %>%
    filter(n() == 1) %>%
    ungroup()
  
  linked_gages <- select(no_location, provider_id) %>%
    mutate(id = seq_len(nrow(.))) %>%
    left_join(select(linked_gages_dedup, 
                     id, COMID, REACHCODE, REACH_meas), 
              by = "id")
  
  all_gages$nhdpv2_REACHCODE[update_index] <- linked_gages$REACHCODE
  all_gages$nhdpv2_REACH_measure[update_index] <- linked_gages$REACH_meas
  all_gages$nhdpv2_COMID[update_index] <- linked_gages$COMID
  
  all_gages
}

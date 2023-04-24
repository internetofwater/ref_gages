# tar_load(c("nwis_gage", "streamstates_sites", "cdec_gage", "co_gage", "pnw_gage"))

get_gage_locations <- function(nwis_gage, streamstats_sites, cdec_gage, co_gage, pnw_gage) {
  
  sqmi_to_sqkm <- 2.58999
  
  gages <- dplyr::filter(nwis_gage, (site_tp_cd == "ST" | 
                                       site_tp_cd == "ST-CA" |
                                       site_tp_cd == "ST-DCH" |
                                       site_tp_cd == "ST-TS" |
                                       site_tp_cd == "ES" |
                                       site_tp_cd == "LK") & 
                           !is.na(dec_long_va) & 
                           !is.na(dec_lat_va)) |>
    select(dec_lat_va, dec_long_va, site_no, station_nm, site_no, drain_area_va) |>
    group_by(site_no) |> arrange(drain_area_va) |>
    filter(n() == 1) |> ungroup() |>
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) |>
    mutate(description = paste0("USGS NWIS Stream/River/Lake Site ", site_no, ": ", station_nm),
           subjectOf = paste0("https://waterdata.usgs.gov/monitoring-location/", site_no),
           provider = "https://waterdata.usgs.gov",
           provider_id = site_no,
           drainage_area_sqkm = (as.numeric(drain_area_va) * sqmi_to_sqkm)) |>
    select(name = station_nm, 
           description,
           subjectOf,
           provider,
           provider_id,
           drainage_area_sqkm)
  
  c_gage <- cdec_gage |>
    mutate(description = paste("Stream Type:", ucdstrmclass, "Status:", sitestatus)) |>
    filter(provider == "https://cdec.water.ca.gov") |>
    select(name = sitename, 
           description = description,
           subjectOf = weblink,
           provider = provider,
           provider_id = id, 
           drainage_area_sqkm = totdasqkm)
  
  co_gage_out <- co_gage |>
    mutate(description = paste("CO DWR Station Type:", `Station Type`, "from data source:", `Data Source`),
           provider = "https://dwr.state.co.us") |>
    select(name = `Station Name`,
           description = description,
           subjectOf = `More Information`,
           provider = provider,
           provider_id = `DWR Abbrev`)
  
  p_gage_provider <- pnw_gage$providers |>
    mutate(url = ifelse(is.na(`Organization Website`), 
                              `Program Website`,
                              `Organization Website`))
  
  p_gage_out <- pnw_gage$data |>
    left_join(select(p_gage_provider, Organization, provider_url = url), 
              by = c("organization" = "Organization")) |>
    mutate(description = paste0(organization, " Streamflow Site")) |>
    mutate(description = ifelse(!`stream type` %in% c("unknown", "NA"),
                                paste0(description, " Type: ", `stream type`),
                                description)) |>
    select(name = Site_Name,
           description, 
           subjectOf = url,
           provider = provider_url,
           provider_id = org_SiteNo)
    
  
  bind_rows(co_gage_out, c_gage, gages)
}

get_cdec_gage_locations <- function(gages) {
  gages |>
    filter(provider == "https://cdec.water.ca.gov") |>
    select(nhdpv2_REACHCODE = rchcd_medres,
           nhdpv2_COMID = comid_medres,
           provider_id = id) |>
    mutate(nhdpv2_REACH_measure = rep(NA_real_, n()),
           nhdpv2_COMID = as.numeric(nhdpv2_COMID))
}

# gages <- targets::tar_read("co_gage")
get_co_gage_locations <- function(gages) {
  
  gages |>
    select(provider_id = `DWR Abbrev`) |>
    mutate(nhdpv2_REACHCODE = rep(NA_character_, n()),
           nhdpv2_COMID = rep(NA_integer_, n()),
           nhdpv2_REACH_measure = rep(NA_real_, n()))
  
}

get_nwis_hydrolocations <- function(nhdpv2_gage, 
                                    swim_gage,
                                    nwis_hydrolocation) {
  nh <- read.csv(nwis_hydrolocation, colClasses = c("character", 
                                                    "integer", 
                                                    "character", 
                                                    "numeric"))
  
  if(any(swim_gage$Gage_no %in% nh$provider_id)) stop("duplicates in override registry")
  
  swim_gage <- sf::st_drop_geometry(swim_gage) |>
    select(provider_id = Gage_no, 
           nhdpv2_COMID = COMID,
           nhdpv2_REACHCODE = REACHCODE,
           nhdpv2_REACH_measure = REACH_meas) |>
    filter(nhdpv2_COMID != -9999)

  nh <- bind_rows(nh, swim_gage)
  
  nhdpv2_gage <- filter(st_drop_geometry(nhdpv2_gage), 
                        !provider_id %in% nh$provider_id)
  
  bind_rows(nhdpv2_gage, nh)
  
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
    
    # Some gages missing reachcode/measure but have COMID
    update_index <- is.na(all_gages$nhdpv2_REACH_measure & !is.na(all_gages$nhdpv2_COMID))
    
    if(any(update_index)) {
      linked_gages <- select(all_gages[update_index, ], provider_id, nhdpv2_COMID) |>
        left_join(select(sf::st_drop_geometry(nhdpv2_fline), COMID, FromMeas), 
                  by = c("nhdpv2_COMID" = "COMID"))
      
      all_gages$nhdpv2_REACH_measure[update_index] <- 
        linked_gages$FromMeas
    }
  }
  
  all_gages <- left_join(all_gages, v2_area, by = "nhdpv2_COMID")
  
  diff_da <- abs(all_gages$nhdpv2_totdasqkm -
                   all_gages$drainage_area_sqkm) / 
    all_gages$drainage_area_sqkm
  
  bad_da <- all_gages[!is.na(diff_da) & diff_da > da_diff_thresh, ]
  
  update_index <- which(is.na(all_gages$nhdpv2_COMID) | 
                          all_gages$provider_id %in% bad_da$provider_id)
  
  no_location <- all_gages[update_index, ]
  
  no_location <- st_transform(no_location, 5070)
  
  new_hl <- nhdplusTools::get_flowline_index(nhdpv2_fline, 
                                             no_location, 
                                             search_radius = units::set_units(
                                               search_radius_m, "m"),
                                             max_matches = max_matches_in_radius)
  
  
  linked_gages <- st_drop_geometry(select(no_location, provider_id)) |>
    mutate(id = seq_len(nrow(.))) |>
    left_join(new_hl, by = "id") |>
    left_join(select(st_drop_geometry(all_gages), 
                     provider_id, drainage_area_sqkm), 
              by = "provider_id") |>
    left_join(v2_area, by = c("COMID" = "nhdpv2_COMID")) |>
    mutate(da_diff = abs(drainage_area_sqkm - nhdpv2_totdasqkm))
  
  linked_gages_dedup <- bind_rows(
    linked_gages |>
      group_by(provider_id) |>
      filter(is.na(da_diff)) |>
      filter(offset == min(offset)) |>
      ungroup(), 
    linked_gages |>
      group_by(provider_id) |>
      filter(!is.na(da_diff)) |>
      filter(da_diff == min(da_diff)) |>
      ungroup()) |>
    group_by(provider_id) |>
    filter(n() == 1) |>
    ungroup()
  
  linked_gages <- select(no_location, provider_id) |>
    mutate(id = seq_len(nrow(.))) |>
    left_join(select(linked_gages_dedup, 
                     id, COMID, REACHCODE, REACH_meas), 
              by = "id")
  
  all_gages$nhdpv2_REACHCODE[update_index] <- linked_gages$REACHCODE
  all_gages$nhdpv2_REACH_measure[update_index] <- linked_gages$REACH_meas
  all_gages$nhdpv2_COMID[update_index] <- linked_gages$COMID
  
  all_gages
}

add_mainstems <- function(gage_hydrologic_locations, mainstems, vaa) {
  mainstems <- mainstems[,c("id", "uri"), drop = TRUE]
  mainstems$id <- as.integer(mainstems$id)
  vaa <- right_join(vaa, mainstems, by = c("levelpathi" = "id"))
  
  vaa <- vaa[,c("comid", "uri")]
  
  names(vaa) <- c("comid", "mainstem_uri")
  
  left_join(gage_hydrologic_locations, vaa, 
            by = c("nhdpv2_COMID" = "comid"))
}
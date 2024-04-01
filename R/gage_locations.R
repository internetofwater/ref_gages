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
  
  p_gage_provider$url[p_gage_provider$Organization == "Warner Basin Habitat Partnership"] <- "https://lakecountywsc.com/warner-basin-fip"
  p_gage_provider$Organization[p_gage_provider$Organization == "Dry Creek Experimental Wastershed"] <- "Dry Creek Experimental Watershed"
  p_gage_provider$Organization[p_gage_provider$Organization == "Preist River Experimental Forest"] <- "Priest River Experimental Forest" 
  p_gage_provider$Organization[p_gage_provider$Organization == "Oregon Water Enhancement Board" ] <- "Oregon Watershed Enhancement Board"
  
  pnw_gage$data$`organization dataset`[pnw_gage$data$`organization dataset` == "R6"] <- "US Forest Service, Region 6"
  pnw_gage$data$`organization dataset`[pnw_gage$data$`organization dataset` == "Warner Basin Habitat Partneship (WBAHP)"] <- "Warner Basin Habitat Partnership"
  pnw_gage$data$`organization dataset`[pnw_gage$data$`organization dataset` == "Columbia SWCD"] <- "Columbia Soil & Water Conservation District"
  
  pnw_gage$data$`organization`[pnw_gage$data$`organization` == "HJ Andrews LTER"] <- "HJ Andrews Long Term Ecological Research Site"
  
  pnw_gage$data$`organization`[pnw_gage$data$`organization` == "Pierce County, WA"] <- "Pierce County" 
  
  # all_orgs <- unique(p_gage_provider$Organization)
  # 
  # not_ds <- all_orgs[!all_orgs %in% pnw_gage$data$`organization dataset`]
  # 
  # not_ds[!not_ds %in% pnw_gage$data$organization]
  
  orgs <- c("Idaho Department of Environmental Quality", 
            "Idaho Department of Water Resources", 
            "Idaho Power", 
            "Oregon Department of Fish and Wildlife", 
            "Oregon Watershed Enhancement Board", 
            "Oregon Water Resources Department", 
            "US Forest Service, Region 6", 
            "Washington Department of Ecology", 
            "Washington Department of Fish and Wildlife")
  
  p_gage_out <- pnw_gage$data |>
    mutate(org = ifelse(`organization dataset` %in% p_gage_provider$Organization, `organization dataset`, `organization`)) |>
    filter(.data$org %in% orgs) |>
    left_join(select(p_gage_provider, Organization, provider_url = url), 
              by = c("org" = "Organization")) |>
    mutate(description = paste0(org, " Streamflow Site")) |>
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

#' hydrologic locations
#' @description takes all gages from this run and all pre-determined hydrologic locations
#' determines the "best" hydrologic location for each gage and snaps to NHDPlusV2
#' if no pre-determined location exists.
get_hydrologic_locations <- function(all_gages, ref_locations, hydrologic_locations, nhdpv2_fline,
                                     da_diff_thresh = 0.5, search_radius_m = 500,
                                     max_matches_in_radius = 5) {
  
  v2_area <- select(nhdplusTools::get_vaa(), 
                    nhdpv2_COMID = comid, 
                    nhdpv2_totdasqkm = totdasqkm)
  
  providers <- readr::read_csv("reg/providers.csv")
  ref_locations$provider <- providers$provider[ref_locations$provider]
  
  all_gages <- ref_locations |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4269) |>
    left_join(select(sf::st_drop_geometry(all_gages), name, description, subjectOf, 
                     provider, provider_id, drainage_area_sqkm),
              by = c("provider", "provider_id"))
  
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
    mutate(id = seq_len(n())) |>
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
    mutate(id = seq_len(n())) |>
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
  
  mainstems <- mainstems[,c("head_nhdpv2_COMID", "uri"), drop = TRUE]
  mainstems$head_nhdpv2_COMID <- as.integer(gsub("https://geoconnex.us/nhdplusv2/comid/", "", 
                                                 mainstems$head_nhdpv2_COMID))
  
  mainstem_lookup <- group_by(vaa, levelpathi) |>
    filter(hydroseq == max(hydroseq)) |>
    ungroup() |>
    select(head_nhdpv2_COMID = comid, levelpathi) |>
    distinct() |>
    left_join(mainstems, by = "head_nhdpv2_COMID") |>
    filter(!is.na(uri)) |>
    select(-head_nhdpv2_COMID) |>
    right_join(select(vaa, comid, levelpathi), 
               by = "levelpathi") |>
    select(-levelpathi, comid, mainstem_uri = uri)
  
  dplyr::left_join(gage_hydrologic_locations, mainstem_lookup, by = c("nhdpv2_COMID" = "comid"))

}

#' find duplicate locations
#' @description finds gages within 100m of eachother then checks if they are 
#' linked to different rivers. Returns reference gages that appear to duplicate
#' other reference gages.
find_duplicate_locations <- function(ghl) {
  
  coords <- sf::st_coordinates(sf::st_transform(ghl, 5070))
  
  future::plan(future::multisession, workers = 13)
  
  clusters <- pbapply::pblapply(split(1:nrow(coords), cut(seq_along(1:nrow(coords)), 500, labels = FALSE)), 
                                function(set, coords) {
                                  lapply(set, function(x, coords) {
                                    dist <- sqrt((coords[x, 1] - coords[, 1]) ^ 2 + (coords[x, 2] - coords[, 2]) ^ 2)
                                    which(dist < 100)
                                  }, coords = coords) 
                                }, coords = coords, cl = "future")
  
  clusters <- unlist(clusters, recursive = FALSE)
  
  # now remove things that shouldn't be called duplicates.
  
  clusters <- pbapply::pblapply(1:length(clusters), function(x) {
    not_same <- clusters[[x]][clusters[[x]] != x]
    
    if(length(not_same) == 0) return(integer())
    
    comid <- ghl$nhdpv2_COMID[x]
    
    if(is.na(comid)) return(unname(not_same))
    
    unname(not_same[ghl$nhdpv2_COMID[not_same] == comid])
  })
  
  clusters <- data.frame(row = 1:length(clusters), cluster = I(clusters))
  
  clusters <- dplyr::filter(clusters, lengths(clusters$cluster) > 0)  
  
  clusters <- tidyr::unnest(clusters, cluster)
  
  ghl$row <- 1:nrow(ghl)
  
  clusters <- dplyr::left_join(clusters, dplyr::select(sf::st_drop_geometry(ghl), row, id), by = "row") |>
    dplyr::left_join(dplyr::select(sf::st_drop_geometry(ghl), row, cluster_id = id), by = c("cluster" = "row")) |>
    dplyr::select(-row, -cluster)
  
  clusters <- dplyr::group_by(clusters, id) |>
    dplyr::summarise(cluster_id = list(unique(.data$cluster_id)))
  
  dplyr::select(ghl, -row) |>
    dplyr::left_join(clusters, by = "id")

}
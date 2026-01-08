# tar_load(c("nwis_gage", "streamstates_sites", "cdec_gage", "co_gage", "pnw_gage"))
# TODO: finish incorporating PNW Gage -- it is not used right now.
get_gage_locations <- function(nwis_gage, streamstats_sites, cdec_gage, co_gage, pnw_gage) {
  
  sqmi_to_sqkm <- 2.58999
  
  gages <- nwis_gage |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    transmute(
      site_no = id,
      station_nm = monitoring_location_name,
      drain_area_va = drainage_area
    ) |>
    group_by(site_no) |>
    arrange(drain_area_va) |>
    filter(n() == 1) |>
    ungroup() |>
    mutate(
      description = paste0(
        "USGS NWIS Stream/River/Lake Site ",
        site_no,
        ": ",
        station_nm
      ),
      subjectOf = paste0(
        "https://waterdata.usgs.gov/monitoring-location/",
        site_no
      ),
      provider = "https://waterdata.usgs.gov",
      provider_id = site_no,
      drainage_area_sqkm = (as.numeric(drain_area_va) * sqmi_to_sqkm)
    ) |>
    select(
      name = station_nm,
      description,
      subjectOf,
      provider,
      provider_id,
      drainage_area_sqkm
    )
  
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
           provider_id = id,
           provider = provider) |>
    mutate(nhdpv2_REACH_measure = rep(NA_real_, n()),
           nhdpv2_COMID = as.numeric(nhdpv2_COMID),
           nhdpv2_link_source = "https://cdec.water.ca.gov")
}

# gages <- targets::tar_read("co_gage")
get_co_gage_locations <- function(gages) {
  
  gages |>
    select(provider_id = `DWR Abbrev`) |>
    mutate(nhdpv2_REACHCODE = rep(NA_character_, n()),
           nhdpv2_COMID = rep(NA_integer_, n()),
           nhdpv2_REACH_measure = rep(NA_real_, n()),
           nhdpv2_link_source = rep(NA_character_, n()),
           provider = rep("https://dwr.state.co.us", n()))
  
}

add_mainstems_and_nws <- function(gage_hydrologic_locations, mainstems, vaa, nws_gages) {
  
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
  
  out <- dplyr::left_join(gage_hydrologic_locations, mainstem_lookup, by = c("nhdpv2_COMID" = "comid"))
  
  nws_table <- nws_gages |>
    dplyr::filter(grepl("geological|usgs", `attribution wording`, ignore.case = TRUE) & !is.na(`usgs id`)) |>
    dplyr::select(usgs_id = `usgs id`, nws_url = `hydrograph page`) |>
    dplyr::distinct() |>
    dplyr::group_by(usgs_id) |>
    dplyr::summarise(nws_url = list(unique(.data$nws_url)))
  
  out <- dplyr::left_join(out, nws_table, by = c("provider_id" = "usgs_id"))

  out
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
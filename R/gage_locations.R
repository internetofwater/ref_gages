get_nwis_gage_locations <- function(nwis_gage) {
  
  gages <- dplyr::filter(nwis_gage, site_tp_cd == "ST" & 
                           !is.na(dec_long_va) & 
                           !is.na(dec_lat_va)) %>%
    st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326) %>%
    mutate(description = paste0("USGS NWIS Stream/River Site ", site_no, ": ", station_nm),
           subjectOf = paste0("https://waterdata.usgs.gov/monitoring-location/", site_no),
           uri = paste0("https://geoconnex.us/ref/gages/", n()),
           provider = "https://waterdata.usgs.gov",
           provider_id = site_no) %>%
    select(name = station_nm, 
           description,
           subjectOf,
           provider,
           provider_id)
  
}

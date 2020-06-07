library(drake)
library(dplyr)
library(sf)
library(nhdplusTools)
library(dataRetrieval)
library(sbtools)

sourced <- sapply(list.files("R", pattern = "*.R", full.names = TRUE), source)

plan <- drake_plan(
  # NHDPlusV2 downloaded with nhdplusTools
  nat_db = download_nhdplusv2("data/nhdp"),
  
  # Only the network flowlines for now -- non-network could be pulled in.
  nhdpv2_fline = read_sf(nat_db, "NHDFlowline_Network"),
  nhdpv2_fline_proc = select(st_transform(nhdpv2_fline, 5070),
                             COMID, REACHCODE, ToMeas, FromMeas),
  
  # This function downloads all NWIS sites from the site file
  nwis_gage = get_nwis_sites(),
  
  # this function filters and renames NWIS Gage Locations
  nwis_gage_locations = get_nwis_gage_locations(nwis_gage),
  
  # This Gage layer from NHDPlusV2 is a basic starting point for
  # NWIS gage locations.
  nhdpv2_gage = select(read_sf(nat_db, "Gage"), 
                       nhdpv2_REACHCODE = REACHCODE, 
                       nhdpv2_REACH_measure = Measure,
                       nhdpv2_COMID = FLComID,
                       provider_id = SOURCE_FEA),
  
  # This function takes a table of all NWIS and more in the future gage
  # locations and a list of provided hydrologic locations. The provider
  # is a way to join on provider and provider_id in the all_gages input.
  # The order that hydrologic locations sources are provided will determine
  # precidence -- last defined wins.
  gage_hydrologic_locations = get_hydrologic_locations(
    all_gages = nwis_gage_locations,
    hydrologic_locations = list(
      list(provider = "https://waterdata.usgs.gov",
           locations = nhdpv2_gage)),
    nhdpv2_fline = nhdpv2_fline_proc))

make(plan, memory_strategy = "autoclean", garbage_collection = TRUE)

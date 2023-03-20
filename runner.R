library(targets)

tar_option_set(packages = c("nhdplusTools", "sf", "dplyr", "dataRetrieval", 
                            "sbtools", "readr", "knitr", "mapview"),
               memory = "transient", garbage_collection = TRUE)

reference_file <- "out/ref_gages.gpkg"

# this is a set of location overrides
nwis_hydrolocation <- "data/nwis_hydrolocations.csv"

# These are generated for a USGS namespace in geoconnex.
usgs_reference_file <- "out/usgs_gages.gpkg"
usgs_nldi_file <- "out/usgs_nldi_gages.geojson"

pid_file <- "out/ref_gages_pid.csv"
nldi_file <- "out/nldi_gages.geojson"

index_dir <- "docs/"

sourced <- sapply(list.files("R", pattern = "*.R$", full.names = TRUE), source)

list(
  # NHDPlusV2 downloaded with nhdplusTools
  tar_target("nat_db", download_nhdplusv2("data/nhdp")),
  
  # Only the network flowlines for now -- non-network could be pulled in.
  tar_target("nhdpv2_fline", read_sf(nat_db, "NHDFlowline_Network")),
  tar_target("nhdpv2_fline_proc", select(st_transform(nhdpv2_fline, 5070),
                                        COMID, REACHCODE, ToMeas, FromMeas)),
  tar_target("mainstems", get_all_mainstems("data/mainstems/")),
  tar_target("vaa", get_vaa(atts = c("comid", "levelpathi"),
                           updated_network = TRUE)),
  # This function downloads all NWIS sites from the site file
  tar_target("nwis_gage", get_nwis_sites()),
  
  # This function downloads all cdec sites
  tar_target("cdec_gage", get_cdec_data()),
  
  # This functions loads locally stored streamstats sites.
  tar_target("streamstats_sites", get_streamstats_sites()),
  
  # This function loads the SWIMS gage locations.
  tar_target("swims_gage", get_swim_data()),
  
  # this function filters and renames gage locations to a common table
  tar_target("gage_locations", get_gage_locations(nwis_gage,
                                                  streamstats_sites, 
                                                  cdec_gage)),
  
  # This Gage layer from NHDPlusV2 is a basic starting point for
  # NWIS gage locations.
  tar_target("nhdpv2_gage", select(read_sf(nat_db, "Gage"), 
                                   nhdpv2_REACHCODE = REACHCODE, 
                                   nhdpv2_REACH_measure = Measure,
                                   nhdpv2_COMID = FLComID,
                                   provider_id = SOURCE_FEA)),
  
  tar_target("nwis_gage_hydro_locatons", get_nwis_hydrolocations(nhdpv2_gage,
                                                                 swims_gage,
                                                                 nwis_hydrolocation)),
  
  tar_target("cdec_gage_address", get_cdec_gage_locations(cdec_gage)),
  
  # This function takes a table of all NWIS and more in the future gage
  # locations and a list of provided hydrologic locations. The provider
  # is a way to join on provider and provider_id in the all_gages input.
  # The order that hydrologic locations sources are provided will determine
  # precidence -- last defined wins.
  tar_target("gage_hydrologic_locations", get_hydrologic_locations(
    all_gages = gage_locations,
    hydrologic_locations = list(
      list(provider = "https://waterdata.usgs.gov",
           locations = nwis_gage_hydro_locatons),
      list(provider = "https://cdec.water.ca.gov",
           locations = cdec_gage_address)),
    nhdpv2_fline = nhdpv2_fline_proc)),
  
  tar_target("gage_hydrologic_locations_with_mainstems", add_mainstems(gage_hydrologic_locations,
                                                                       mainstems, vaa)),
  
  # Each entry will have a provider and provider_id that acts as a unique
  # primary key. The existing registry file will have a unique attribute
  # that contains that primary key. 
  tar_target("providers", read_csv(file_in("reg/providers.csv"))),
  
  
  tar_target("registry", build_registry(gage_locations,
                                        registry = file_in("reg/ref_gages.csv"),
                                        providers = providers)),
  
  # Creates an output for USGS namespace reference locations
  tar_target("usgs_reference_out", write_usgs_reference(gage_hydrologic_locations_with_mainstems, 
                                                        registry, providers, usgs_reference_file, 
                                                        usgs_nldi_file)),
  
  tar_target("reference_out", write_reference(gage_hydrologic_locations_with_mainstems, 
                                              registry, providers, reference_file, 
                                              nldi_file)),
  tar_target("registry_out", write_registry(registry, "reg/ref_gages.csv")))
# index" build_index(reference_out, index_dir))

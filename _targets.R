library(targets)

tar_option_set(packages = c("nhdplusTools", "sf", "dplyr", "dataRetrieval", 
                            "sbtools", "readr", "knitr", "mapview", "data.table"),
               memory = "transient", garbage_collection = TRUE)

# primary output file for geoconnex reference server
reference_file <- "out/ref_gages.gpkg"

# registry csv file which is checked in
registry_csv <- "reg/ref_gages.csv"

# locations for all known reference gages
# https://github.com/internetofwater/ref_gages/issues/33
reference_locations_csv <- "reg/ref_locations.csv"

# contains information for each gage provider
providers_lookup_csv <- "reg/providers.csv"

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
  tar_target("vaa", get_vaa(atts = c("comid", "levelpathi", "hydroseq"),
                           updated_network = TRUE)),
  
  ### Downloaders ###
  # This function downloads all NWIS sites from the site file
  tar_target("nwis_gage", get_nwis_sites()),
  
  # This function downloads all cdec sites
  tar_target("cdec_gage", get_cdec_data()),
  
  # This function downloads all CO gage data
  tar_target("co_gage", get_co_data()),
  
  # This function downloads all gages from the PNW catalog
  # WIP: will be incorporated in the future.
  tar_target("pnw_gage", get_pnw_data()),
  
  # This functions loads locally stored streamstats sites.
  tar_target("streamstats_sites", get_streamstats_sites()),
  
  # This function loads the SWIMS gage locations.
  tar_target("swims_gage", get_swim_data()),
  
  ### metadata integration ###
  # this function filters and renames gage locations to a common table
  # It does not handle location information and duplicates are fine at this stage.
  tar_target("gage_locations", get_gage_locations(nwis_gage,
                                                  streamstats_sites, 
                                                  cdec_gage,
                                                  co_gage,
                                                  pnw_gage)),
  
  ### location normalization ###
  # these targets generate a normalized form set of gages from each source.
  
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
  
  
  tar_target("co_gage_address", get_co_gage_locations(co_gage)),
  
  ### Registry ###
  # Each entry will have a provider and provider_id that acts as a unique
  # primary key. The existing registry file will have a unique attribute
  # that contains that primary key. 
  tar_target("providers_csv", providers_lookup_csv, format = "file"),
  tar_target("providers", read_csv(providers_csv)),
  
  
  tar_target("registry", build_registry(gage_locations,
                                        registry = registry_csv,
                                        providers = providers)),
  
  # Also create a table of reference locations for the registered gages.
  # unlike the registry, this may update to have the "best" location of a gage. 
  tar_target("ref_locations", build_reference_location(gage_locations, 
                                                       reference_locations = reference_locations_csv, 
                                                       registry = registry_csv, 
                                                       providers = providers)),
  
  ### spatial integration ###
  # This function takes a table of all ref_locations, latest all_gages
  # locations and a list of provided hydrologic locations. The provider
  # is a way to join on provider and provider_id in the all_gages input.
  # The order that hydrologic locations sources are provided will determine
  # precidence -- last defined wins.
  tar_target("gage_hydrologic_locations", get_hydrologic_locations(
    all_gages = gage_locations,
    ref_locations = ref_locations,
    hydrologic_locations = list(
      list(provider = "https://waterdata.usgs.gov",
           locations = nwis_gage_hydro_locatons),
      list(provider = "https://cdec.water.ca.gov",
           locations = cdec_gage_address),
      list(provider = "https://dwr.state.co.us",
           locations = co_gage_address)),
    nhdpv2_fline = sf::st_zm(nhdpv2_fline_proc))),
  
  tar_target("gage_hydrologic_locations_with_mainstems", add_mainstems(gage_hydrologic_locations,
                                                                       mainstems, vaa)),
  
  # based on all known gages from all providers, find potential duplicates.
  # starts by finding gages within 100m of eachother then checks if they are on different flowlines.
  tar_target("duplicate_locations", find_duplicate_locations(gage_hydrologic_locations_with_mainstems)),
  
  ### output ###
  # Creates an output for USGS namespace reference locations
  tar_target("usgs_reference_out", write_usgs_reference(gage_hydrologic_locations_with_mainstems,
                                                        registry, providers, usgs_reference_file,
                                                        usgs_nldi_file)),
  
  tar_target("reference_out", write_reference(gage_hydrologic_locations_with_mainstems, 
                                              registry, providers, reference_file, 
                                              nldi_file,
                                              duplicate_locations = duplicate_locations)),
  tar_target("registry_out", write_registry(registry, registry_csv)),
  
  tar_target("validation", validate_ref_gage(registry_csv, reference_file, 
                                             reference_locations_csv, 
                                             providers_lookup_csv,
                                             reference_out)))

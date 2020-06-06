library(drake)
library(dplyr)
library(sf)
library(nhdplusTools)
library(dataRetrieval)
library(sbtools)

sourced <- sapply(list.files("R", pattern = "*.R", full.names = TRUE), source)

plan <- drake_plan(nat_db = download_nhdplusv2("data/nhdp"),
                   nwis_gage = get_nwis_sites(),
                   nhdpv2_gage = read_sf(nat_db, "Gage"))

make(plan)

get_nwis_sites <- function() {
  hu02 <- c(paste0("0", c(1:9)), paste0("1", c(0:9)), "20", "21")
  
  sites <- do.call(rbind, lapply(hu02, function(x) {
    importRDB1(paste0("https://waterservices.usgs.gov/nwis/site/?site_output=expanded&format=rdb&huc=",
                      x))
  }))
}
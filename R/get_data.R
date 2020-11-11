get_nwis_sites <- function() {

  st_code <- c(paste0("0", c(1:9)), as.character(c(10:98)))
  
  sites <- do.call(rbind, lapply(st_code, function(x) {
    url <- paste0("https://waterservices.usgs.gov/nwis/site/?site_output=expanded&format=rdb&stateCd=", x)
    
    message(url)
    
    try(importRDB1(url))
  }))
  
  sites
}

get_streamstats_sites <- function() {
  base_url <- "https://test.streamstats.usgs.gov/gagestatsservices/stations?pageCount=1000&page="
  
  check <- TRUE
  page <- 1
  dat <- rep(list(list()), 1000)
  
  while(check) {
    json <- jsonlite::fromJSON(paste0(base_url, page))
    
    if(length(json) == 0) {
      check <- FALSE
      dat <- dat[1:page - 1]
    } else {
      dat[[page]] <- json
      page <- page + 1
    }
  }
  
  streamstats <- lapply(dat, function(x) {
    x$geometry <- x$location$coordinates
    x <- dplyr::select(x, -location)
    x$geometry <- lapply(x$geometry, sf::st_point)
    sf::st_sf(x, crs = sf::st_crs(4326))
  })
  
  do.call(rbind, streamstats)
}
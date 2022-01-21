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

get_cdec_data <- function() {
  url <- "https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items?f=json&limit=10000"
  
  sf::read_sf(url)
}

get_swim_data <- function() {
  sb <- "5ebe92af82ce476925e44b8f"
  
  out_dir <- tempdir(check = TRUE)
  
  item <- item_get(sb)
  
  facet_files <- 
    lapply(item$facets, function(x) 
    {
      list(name = x$name, 
           files = lapply(x$files, function(y, out_path)
           {
             
             out_file <- file.path(out_path, y$name)
             dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
             download.file(y$downloadUri, out_file, mode = "wb")
             
             list(fname = y$name,
                  url = y$downloadUri,
                  path = out_file)
             
           }, 
           out_path = file.path(out_dir, x$name)))
    })
  
  shp <- sapply(facet_files[[1]]$files, function(x) x$path)
  
  shp <- shp[grepl("shp$", shp)]
  
  sf::read_sf(shp)
  
}
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
  base_url <- "https://streamstats.usgs.gov/gagestatsservices/stations?pageCount=1000&page="
  
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
  
  dplyr::bind_rows(streamstats)
}

get_cdec_data <- function() {
  url <- "https://sb19.linked-data.internetofwater.dev/collections/ca_gages/items?f=json&limit=10000"
  
  sf::read_sf(url)
}

get_co_data <- function() {
  # https://data.colorado.gov/Water/Current-Surface-Water-Conditions-in-Colorado/4yw9-a5y6/data
  
 url <- "https://data.colorado.gov/api/views/4yw9-a5y6/rows.csv?accessType=DOWNLOAD"
 
 d <- readr::read_csv(url) 
 
 d$y <- sapply(strsplit(d$Location, split = ", "), 
               function(x) as.numeric(gsub("\\(", "", x[1])), USE.NAMES = FALSE)
 d$x <- sapply(strsplit(d$Location, split = ", "), 
               function(x) as.numeric(gsub("\\)", "", x[2])), USE.NAMES = FALSE)
 
 d <- d[!is.na(d$Location), ]
 
 d <- select(d, `Station Type`, `Data Source`, `Station Name`, `More Information`, `DWR Abbrev`, x, y) |>
   group_by(`DWR Abbrev`) |>
   summarise(`Station Type` = paste(`Station Type`, collapse = " - "), 
             `Data Source` = `Data Source`[1], 
             `Station Name` = `Station Name`[1], 
             `More Information` = `More Information`[1], 
             x = x[1], y = y[1])
 
 sf::st_as_sf(d, coords = c("x", "y"), crs = 4326)
}

get_pnw_data <- function() {
  # "https://doi.org/10.18122/redi_data.2.boisestate"
  
  zip <- "https://scholarworks.boisestate.edu/context/redi_data/article/1001/type/native/viewcontent"
  
  f <- tempfile(fileext = ".zip")
  
  download.file(zip, f, mode = "wb")
  
  contents <- zip::unzip(f, exdir = dirname(f))
  
  f <- list.files(dirname(f), pattern = "Streamflow_Catalog_2023-02-06.xlsx", full.names = TRUE)
  
  d <- readxl::read_xlsx(f, 1)
  p <- readxl::read_xlsx(f, 2)
  
  d <- sf::st_as_sf(d, coords = c("long", "lat"), crs = 4326)
  
  list(data = d, providers = p)
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

get_all_mainstems <- function(outdir) {
  url <- "https://www.hydroshare.org/resource/4a22e88e689949afa1cf71ae009eaf1b/data/contents/mainstems.gpkg"
  
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  f <- file.path(outdir, basename(url))
  
  if(!file.exists(f)) {
    download.file(url, destfile = f, mode = "wb")
  }
  
  sf::read_sf(f)
}

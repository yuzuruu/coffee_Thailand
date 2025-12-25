#########################################################################
# Coffee shops location and stocks in Chiang Mai, Thailand
# Part 1 Obtaining location
# 
# First: 25th. December 2025
# Revised: 
# 
# by Yuzuru Utsunomiya, Ph. D.
# (faculty of Economics, Nagasaki University)
#########################################################################
# 
# ---- read.library ----
library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)  # avoid malfunction
library(raster)
library(osmdata)
library(googleway)
library(progressr)
# handlers(global = TRUE)  
# 
# ---- make.mesh ----
# NOTE
# Once downloading, NO need to run again.
# make a target folder to save
dir.create("data/gadm", recursive = TRUE, showWarnings = FALSE)
# download shapefiles directly if necessary
url  <-
  "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_THA_shp.zip"
dest <-
  "data/gadm/gadm41_THA_shp.zip"
download.file(
  url,
  dest,
  mode = "wb"
  )
# unzip
unzip(
  dest,
  exdir = "data/gadm/tha"
  )
# read a province-level shapefiles
tha_level_1 <- 
  sf::st_read("data/gadm/tha/gadm41_THA_1.shp")
# pick up Chiang Mai province
chiangmai_raw <- 
  tha_level_1 %>%
  sf::st_as_sf() %>%
  dplyr::filter(
    NAME_1 == "Chiang Mai"
    )
# 
# transform crs
chiangmai_utm <-
  sf::st_transform(
    chiangmai_raw,
    crs = 32647
  )
# split Chiang Mai province by 500m-square mesh
grid_500_raw <-
  sf::st_make_grid(
    chiangmai_utm,
    cellsize = 500,
    square   = TRUE
  )
# add ID and clip boundary meshes
grid_500_sf <-
  sf::st_sf(
    grid_id = seq_along(grid_500_raw),
    geometry = grid_500_raw
  ) %>%
  sf::st_intersection(., chiangmai_utm) %>%
  sf::st_transform(., 4326)
# save results
# geojson
sf::st_write(
  grid_500_sf,
  "output/chiangmai_grid_500m.geojson",
  delete_dsn = TRUE
)
sf::st_write(
  sf::st_transform(grid_500_sf, 4326),
  "output/chiangmai_boundary.geojson",
  delete_dsn = TRUE
)
# kml
sf::st_write(
  grid_500_sf,
  "output/chiangmai_grid_500m.kml",
  delete_dsn = TRUE
)
sf::st_write(
  sf::st_transform(grid_500_sf, 4326),
  "output/chiangmai_boundary.kml",
  delete_dsn = TRUE
)
# 
# ---- cafe.specification.osm ----
# obtain boundary box of Chiang Mai province
bb <- 
  sf::st_bbox(
    sf::st_transform(
      chiangmai_utm,  
      4326
      )
    )
# download cafe shop information from OSM
q <- 
  osmdata::opq(bbox = bb)  %>% 
  osmdata::add_osm_feature(
    key = "amenity", 
    value = "cafe"
    )
osm_cafe <- 
  osmdata::osmdata_sf(q)
# pickup point and polygon of the cafe shops
cafe_points  <- 
  osm_cafe$osm_points
cafe_polys   <- 
  osm_cafe$osm_polygons
# obtain centroid of the polygon
cafe_points_centroids <- 
  cafe_points %>% 
  sf::st_centroid() %>% 
  sf::st_as_sf()
cafe_poly_centroids <- 
  cafe_polys %>% 
  sf::st_centroid() %>% 
  sf::st_as_sf()
# select necessary columns
# names of target columns' names
cols_keep <- 
  c(
    "osm_id", "name","website", "url","facebook", "instagram","phone", "opening_hours","geometry"
)
cafe_points_sel <- 
  cafe_points %>% 
  dplyr::select(
    # any comuns including the target names 
    dplyr::any_of(cols_keep),     
    dplyr::starts_with("addr:")   
  )
cafe_poly_sel <- 
  cafe_polys %>% 
  dplyr::select(
    # any comuns including the target names 
    dplyr::any_of(cols_keep),     
    dplyr::starts_with("addr:")   
  ) %>% 
  sf::st_centroid() %>%     # ここで POINT に
  sf::st_as_sf()
# merge point and polygon
cafe_osm_chiangmai <- 
  dplyr::bind_rows(cafe_points_sel, cafe_poly_sel) %>% 
  sf::st_join(
    sf::st_transform(chiangmai_utm, 4326), join = st_within, left = FALSE) %>% 
  dplyr::mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2],
    any_url = coalesce(website, url, facebook)
  ) %>% 
  sf::st_drop_geometry()  %>% 
  dplyr::select(
    osm_id, name, lon, lat,
    any_url, opening_hours
  )
# save
# .csv
readr::write_excel_csv(
  cafe_osm_chiangmai, 
  "output/cafes_osm_chiangmai_nearby.csv"
  )
# .rds
readr::write_rds(
  cafe_osm_chiangmai,
  "data/cafes_osm_chiangmai_nearby.rds"
)

# 
# ---- cafe.specification.google ----
# 
google_api_key <- Sys.getenv("GOOGLE_API_KEY")
googleway::set_key(google_api_key)
# UTM に変換（zone 47N）
chiangmai_utm <- 
  sf::st_transform(chiangmai_raw, 32647)
# make 7,500m square mesh
grid_7k5 <- 
  sf::st_make_grid(
    chiangmai_utm,
    cellsize = 7500,
    square   = TRUE
  ) %>%
  sf::st_sf(
    grid_id = seq_along(.), 
    geometry = .
    )
# remove boundary grids
grid_7k5_clip <- 
  st_intersection(
    grid_7k5, 
    chiangmai_utm
    )
# obtain centroids of the grids
grid_centers <- 
  grid_7k5_clip %>%
  sf::st_centroid() %>%
  # UTM -> WGS
  sf::st_transform(4326) %>%
  dplyr::mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>%
  sf::st_drop_geometry()
# obtain N. of tiles
n_tiles <- 
  nrow(grid_centers)
# 
# function to obtain name and location of cafe shops
# radius (=5,000) refers to size of searching area (5,000m) 
fetch_nearby_cafes <- function(lat, lon, radius = 5000, place_type = "cafe",
                               max_pages = 3, page_sleep = 2.2) {
  
  flatten_results_with_coords <- function(results) {
    results2 <- results %>%
      tidyr::unnest_wider(geometry, names_sep = ".")
    
    if ("geometry.location" %in% names(results2)) {
      results3 <- results2 %>%
        tidyr::unnest_wider(geometry.location, names_sep = ".")
    } else {
      results3 <- results2
    }
    
    if (!all(c("geometry.location.lat", "geometry.location.lng") %in% names(results3))) {
      stop("Could not find geometry.location.lat / lng")
    }
    results3
  }
  
  # --- helper: one page request (token may be NULL) ---
  get_page <- function(token = NULL) {
    try(
      googleway::google_places(
        location   = c(lat, lon),
        radius     = radius,
        place_type = place_type,
        page_token = token
      ),
      silent = TRUE
    )
  }
  
  # --- page 1 ---
  res1 <- get_page(NULL)
  
  if (inherits(res1, "try-error")) {
    return(list(status = "TRY_ERROR", df = tibble::tibble(), next_page_token = NA_character_))
  }
  
  status1 <- res1$status
  if (!status1 %in% c("OK", "ZERO_RESULTS")) {
    return(list(status = status1, df = tibble::tibble(), next_page_token = res1$next_page_token %||% NA_character_))
  }
  if (identical(status1, "ZERO_RESULTS") || is.null(res1$results) || nrow(res1$results) == 0) {
    return(list(status = status1, df = tibble::tibble(), next_page_token = res1$next_page_token %||% NA_character_))
  }
  
  all_results <- res1$results
  token <- res1$next_page_token
  
  # --- pages 2..max_pages ---
  if (max_pages > 1) {
    for (i in 2:max_pages) {
      if (is.null(token) || is.na(token) || token == "") break
      Sys.sleep(page_sleep)  # next_page_token activation wait
      
      resi <- get_page(token)
      if (inherits(resi, "try-error")) break
      if (!identical(resi$status, "OK")) break
      if (is.null(resi$results) || nrow(resi$results) == 0) break
      
      all_results <- dplyr::bind_rows(all_results, resi$results)
      token <- resi$next_page_token
    }
  }
  
  # --- flatten geometry once at the end ---
  results_flat <- try(flatten_results_with_coords(all_results), silent = TRUE)
  if (inherits(results_flat, "try-error")) {
    return(list(status = "NO_GEOM_COL", df = tibble::tibble(), next_page_token = token %||% NA_character_))
  }
  
  df <- dplyr::tibble(
    place_id = results_flat$place_id,
    name     = results_flat$name,
    lat      = as.numeric(results_flat$geometry.location.lat),
    lon      = as.numeric(results_flat$geometry.location.lng),
    rating   = results_flat$rating,
    user_ratings_total = results_flat$user_ratings_total
  ) %>%
    dplyr::distinct(place_id, .keep_all = TRUE)
  
  list(
    status = "OK",
    df = df,
    next_page_token = token %||% NA_character_
  )
}

# helper for %||% (NULL coalesce)
`%||%` <- function(x, y) if (is.null(x)) y else x

# run function and obtain 
with_progress(
  {
    p <- progressor(along = 1:n_tiles)
  # run
    res_list <- 
      purrr::map2(
        .x = grid_centers$lat,
        .y = grid_centers$lon,
        .f = 
          ~ 
          {
            out <- fetch_nearby_cafes(lat = .x, lon = .y, radius = 5000, max_pages = 3, page_sleep = 2.2)
            Sys.sleep(0.3)   # regulation
            p()              # 
            out
            }
      )
    }
  )
# reshape and save the results
cafes_google_chiangmai <- 
  # bind all the list obtained
  purrr::map_dfr(
    .x = res_list,
    .f = ~{
      # skip void .x$df 
      if (nrow(.x$df) == 0) return(NULL)
      
      .x$df %>%
        mutate(status = .x$status)
    },
    .id = "tile_id"
  ) %>%
  # pick up unique cases by place_id
  dplyr::distinct(
    place_id, .keep_all = TRUE
    ) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  # merge results with province-level shapefiles
  sf::st_join(
    chiangmai_raw, 
    join = st_within, 
    left = FALSE
    ) %>%
  # obtain lat / lon
  dplyr::mutate(
    lon = sf::st_coordinates(geometry)[, 1],
    lat = sf::st_coordinates(geometry)[, 2]
  ) %>%
  # remove geomerty
  sf::st_drop_geometry()
# save
# .csv
readr::write_excel_csv(
  cafes_google_chiangmai,
  "output/cafes_google_chiangmai_nearby.csv"
)
# .rds
readr::write_rds(
  cafes_google_chiangmai,
  "data/cafes_google_chiangmai_nearby.rds"
)

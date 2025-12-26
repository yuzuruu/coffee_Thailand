#########################################################################
# Coffee shops location and stocks in Chiang Mai, Thailand
# Part 2 matching results + road/built analyses (single-file version)
#
# First: 25 Dec 2025
# by Yuzuru Utsunomiya, Ph.D.
# 
# Description:
# Coffee shops location and stocks in Chiang Mai, Thailand
# Part 2: Matching (Google vs OSM) + Road proximity (A) + Built-up proxy (B)
# Single-file version with explicit namespaces and detailed comments
#
# Notes:
# - Use %>% pipes (not |>)
# - Avoid st_area(geometry) pitfalls by always using sf::st_geometry(.)
# - Fix invalid OSM polygons before st_intersection (TopologyException)
#########################################################################

# ---- read.library.osm.google ----
library(tidyverse)
library(sf)
library(osmdata)
library(googleway)
library(progressr)

# Use s2 for lon/lat operations; projected distances will use GEOS anyway
sf::sf_use_s2(TRUE)

# ---- 1) config ----
cfg <- list(
  # input
  g_path = "output/cafes_google_chiangmai_nearby.csv",
  o_path = "output/cafes_osm_chiangmai_nearby.csv",
  # CRS
  crs_ll  = 4326,
  crs_utm = 32647,  # UTM zone for Chiang Mai
  # matching (distance-based)
  dist_thr_m = 50,
  dist_vec_m = c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150),
  # My Maps split limit (<2000 points)
  max_mymaps = 1999,
  # OSM bbox expand (degree; for overpass query robustness)
  bbox_expand_deg = 0.05,
  # roads: major / minor definition using OSM "highway" tag
  major_vals = c("motorway","trunk","primary","secondary","tertiary"),
  minor_vals = c("unclassified","residential","service","living_street"),
  # built-up proxy from OSM landuse polygons
  built_vals = c("residential","commercial","retail","industrial"),
  # grid for built_share
  grid_cell_m = 1000,
  # urban threshold by built_share for match-rate stratification
  urban_thr = 0.50
)
# 
# ---- helpers.function ----

# Bind only non-NULL sf objects (e.g., osm_lines + osm_multilines)
sf_bind_compact <- 
  function(...) {
    purrr::compact(list(...)) %>% 
      dplyr::bind_rows()
    }

# Expand bbox in degrees (lon/lat) to avoid edge truncation
expand_bbox <- 
  function(bb, expand_deg = 0.05){
    bb["xmin"] <- bb["xmin"] - expand_deg
    bb["xmax"] <- bb["xmax"] + expand_deg
    bb["ymin"] <- bb["ymin"] - expand_deg
    bb["ymax"] <- bb["ymax"] + expand_deg
    bb
    }
# Ensure required columns exist (create with NA if missing)
ensure_cols <- 
  function(df, cols){
    for (nm in cols) {
      if (!nm %in% names(df)) df[[nm]] <- NA
      }
    df
    }
# Fix invalid polygons (common in OSM); required before st_intersection()
# Strategy:
#  1) st_make_valid()
#  2) extract polygons (drop GEOMETRYCOLLECTION noise)
#  3) optional st_buffer(., 0) rescue if still invalid
#  4) drop remaining invalid/empty geometries
fix_polygons_valid <- 
  function(sf_poly){
    out <- 
      sf_poly %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_cast("MULTIPOLYGON", warn = FALSE)

    if (any(!sf::st_is_valid(out))) {
      out <- out %>%
        dplyr::mutate(
          geometry = sf::st_buffer(sf::st_geometry(.), 0)
          ) %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("MULTIPOLYGON", warn = FALSE)
      }
    out %>%
    dplyr::filter(
      sf::st_is_valid(sf::st_geometry(.)),
      !sf::st_is_empty(sf::st_geometry(.))
    )
    }

# Distance from points to nearest feature (both must be projected for meters)
dist_to_nearest <- 
  function(pts_utm, feat_utm){
    if (is.null(feat_utm) || nrow(feat_utm) == 0) return(rep(NA_real_, nrow(pts_utm)))
    nn <- 
      sf::st_nearest_feature(pts_utm, feat_utm)
    as.numeric(sf::st_distance(pts_utm, feat_utm[nn, ], by_element = TRUE))
    }

# Make a square grid (UTM meters) around points extent
make_grid_utm <- 
  function(pts_utm, cellsize = 1000){
    bb <- sf::st_as_sfc(sf::st_bbox(pts_utm))
    sf::st_make_grid(bb, cellsize = cellsize, square = TRUE) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(
        cell_id = dplyr::row_number()
      )
    }
# 
# ---- matching.Google.OSM ----
# Split sets by distance threshold (meters) using nearest-neighbor matching
# - Start: Google -> nearest OSM
# - Keep only pairs within dist_thr_m
# - Enforce 1-to-1 on OSM side (keep minimum distance for each osm_id)
split_by_distance <- 
  function(g_sf, o_sf, dist_thr_m = 50){
    # Identify nearest OSM feature for each Google point
    nn_idx <- sf::st_nearest_feature(g_sf, o_sf)
    # Compute distances in lon/lat; uses s2 under sf_use_s2(TRUE)
    d_m <- as.numeric(sf::st_distance(g_sf, o_sf[nn_idx, ], by_element = TRUE))
    # Candidate pairs within threshold
    pairs0 <- 
      tibble::tibble(
        place_id = g_sf$place_id,
        osm_id   = o_sf$osm_id[nn_idx],
        dist_m   = d_m
        ) %>%
      dplyr::filter(
        !is.na(osm_id), 
        dist_m <= dist_thr_m
        )
    # Enforce 1-to-1 on OSM side: keep the closest Google for each osm_id
    pairs <- 
      pairs0 %>%
      dplyr::group_by(osm_id) %>%
      dplyr::slice_min(dist_m, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    # Matched IDs
    matched_google <- 
      pairs %>% dplyr::distinct(place_id)
    matched_osm    <- 
      pairs %>% dplyr::distinct(osm_id)
  
  # Build a "matched" table with both Google and OSM attributes attached
  matched <- pairs %>%
    dplyr::left_join(sf::st_drop_geometry(g_sf), by = "place_id") %>%
    dplyr::left_join(sf::st_drop_geometry(o_sf), by = "osm_id", suffix = c("_g", "_o"))
  # Unmatched (set difference)
  # Google
  google_only <- g_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::anti_join(matched_google, by = "place_id") %>%
    dplyr::mutate(match_status = "google_only")
  # OSM
  osm_only <- o_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::anti_join(matched_osm, by = "osm_id") %>%
    dplyr::mutate(match_status = "osm_only")
  # 
  list(
    dist_m = dist_thr_m,
    pairs = pairs,          # only IDs + distance
    matched = matched,      # attributes attached
    google_only = google_only,
    osm_only = osm_only
  )
  }

# Evaluate overlap summary for multiple thresholds
eval_sets_by_distances <- function(g_sf, o_sf, dist_vec_m){
  res <- 
    purrr::map(
      dist_vec_m, 
      ~ 
        split_by_distance(
          g_sf, 
          o_sf, 
          dist_thr_m = .x
          )
      )
  summary <- 
    purrr::map_dfr(
      res, 
      function(x){
        tibble::tibble(
          dist_m = x$dist_m,
          n_google = nrow(g_sf),
          n_osm = nrow(o_sf),
          n_pairs = nrow(x$pairs),
          n_google_in_intersection = dplyr::n_distinct(x$pairs$place_id),
          n_osm_in_intersection = dplyr::n_distinct(x$pairs$osm_id),
          n_only_google = nrow(x$google_only),
          n_only_osm = nrow(x$osm_only),
          n_union = nrow(x$google_only) + nrow(x$osm_only) + nrow(x$pairs)
          )
        }
      )
  details <- 
    tibble::tibble(
      dist_m = dist_vec_m,
      pairs = purrr::map(res, "pairs"),
      matched = purrr::map(res, "matched"),
      google_only = purrr::map(res, "google_only"),
      osm_only = purrr::map(res, "osm_only")
      )
  # 
  list(summary = summary, details = details)
}
# Make a "union" table (matched + google_only + osm_only) in a flat schema
make_union_table <- function(spl){
  # Matched: coalesce key fields, keep distance, keep rating fields (from Google)
  m <- 
    spl$matched %>%
    ensure_cols(c(
      "place_id","osm_id","dist_m",
      "name_g","name_o","lat_g","lat_o","lon_g","lon_o",
      "rating","user_ratings_total",
      "any_url","website","opening_hours"
    )) %>%
    dplyr::mutate(
      place_id = as.character(place_id),
      osm_id   = as.character(osm_id)
    )
  # 
  matched_union <- 
    m %>%
    dplyr::transmute(
      match_status = "matched",
      place_id, osm_id, dist_m,
      name = dplyr::coalesce(name_g, name_o),
      lat  = dplyr::coalesce(lat_g,  lat_o),
      lon  = dplyr::coalesce(lon_g,  lon_o),
      rating,
      user_ratings_total,
      any_url = dplyr::coalesce(any_url, website),
      opening_hours
    )
  # Google-only
  g <- spl$google_only %>%
    ensure_cols(c("place_id","name","lat","lon","rating","user_ratings_total")) %>%
    dplyr::mutate(place_id = as.character(place_id))
  google_union <- g %>%
    dplyr::transmute(
      match_status = "google_only",
      place_id,
      osm_id = NA_character_,
      dist_m = NA_real_,
      name, lat, lon,
      rating, user_ratings_total,
      any_url = NA_character_,
      opening_hours = NA_character_
    )
  # OSM-only
  o <- spl$osm_only %>%
    ensure_cols(c("osm_id","name","lat","lon","any_url","website","opening_hours")) %>%
    dplyr::mutate(osm_id = as.character(osm_id))
  osm_union <- o %>%
    dplyr::transmute(
      match_status = "osm_only",
      place_id = NA_character_,
      osm_id,
      dist_m = NA_real_,
      name, lat, lon,
      rating = NA_real_,
      user_ratings_total = NA_real_,
      any_url = dplyr::coalesce(any_url, website),
      opening_hours
    )
  # available to merge more tan 3 object!!
  dplyr::bind_rows(
    matched_union, 
    google_union, 
    osm_union
    )
}

# Prepare My Maps compatible CSV columns
prep_mymaps_csv <- function(df){
  df %>%
    dplyr::transmute(
      Name = dplyr::coalesce(name, "NA"),
      Latitude = lat,
      Longitude = lon,
      MatchStatus = match_status,
      PlaceID = place_id,
      OsmID = osm_id,
      Dist_m = dist_m,
      Rating = rating,
      UserRatingsTotal = user_ratings_total,
      URL = any_url,
      OpeningHours = opening_hours
    ) %>%
    dplyr::filter(!is.na(Latitude), !is.na(Longitude))
}

# Split a My Maps CSV into chunks (<2000 points each)
split_write_mymaps <- 
  function(df, status, prefix, max_n = 1999) {
    x <- 
      df %>%
      dplyr::filter(MatchStatus == status) %>%
      dplyr::arrange(Latitude, Longitude) %>%
      dplyr::mutate(chunk = ((dplyr::row_number() - 1) %/% max_n) + 1L)
    # 
    if (nrow(x) == 0) {
      return(tibble::tibble(status = status, n_points = 0L, n_files = 0L, files = list(character(0))))
      }
  # 
    files <- x %>%
      dplyr::group_split(chunk) %>%
      purrr::imap(function(d, i){
        fn <- sprintf("%s_%02d.csv", prefix, i)
        readr::write_excel_csv(dplyr::select(d, -chunk), fn)
        fn
        }
        )
    
    tibble::tibble(
      status = status,
      n_points = nrow(x),
      n_files = max(x$chunk, na.rm = TRUE),
      files = list(files)
    )
    }

# ---- OSM.fetchers ----
# remove list columns other than geometry
drop_listcols_except_geometry <- 
  function(x) {
    geom_col <- attr(x, "sf_column")
    keep <- names(x)[
      !vapply(x, is.list, logical(1)) | names(x) == geom_col
      ]
    x %>% dplyr::select(dplyr::all_of(keep))
}
# merge sf
sf_rbind_compact <- function(...) {
  xs <- list(...)
  # remove NULL
  xs <- xs[!vapply(xs, is.null, logical(1))]
  # remove other than sf
  xs <- xs[vapply(xs, inherits, logical(1), "sf")]
  # remove 0 column
  xs <- xs[!vapply(xs, function(z) nrow(z) == 0, logical(1))]
  if (length(xs) == 0) return(NULL)
  if (length(xs) == 1) return(xs[[1]])
  
  # list列（geometry以外）を落とす
  xs <- purrr::map(xs, drop_listcols_except_geometry)
  
  # Z/M次元が混ざると結合で落ちることがあるので落とす
  xs <- purrr::map(xs, ~ sf::st_zm(.x, drop = TRUE, what = "ZM"))
  
  # 共通列に揃える（geometry列も含む）
  common <- Reduce(intersect, purrr::map(xs, names))
  xs <- purrr::map(xs, ~ .x %>% dplyr::select(dplyr::all_of(common)))
  
  # sfのrbind（dplyr::bind_rowsより安全）
  out <- do.call(rbind, xs)
  
  out
}
# Fetch roads by highway tag values; returns sf lines
fetch_osm_roads <- 
  function(bb_ll, highway_vals) {
    q <- osmdata::opq(bbox = bb_ll) %>%
      osmdata::add_osm_feature(key = "highway", value = highway_vals)
    raw <- osmdata::osmdata_sf(q)
    roads <- 
      sf_rbind_compact(raw$osm_lines, raw$osm_multilines)
  if (is.null(roads)) return(NULL)
    roads
}
# Fetch built-up proxy polygons by landuse tag values; returns sf polygons
fetch_osm_landuse_built <- 
  function(bb_ll, built_vals) {
    q <- osmdata::opq(bbox = bb_ll) %>%
      osmdata::add_osm_feature(key = "landuse", value = built_vals)
    raw <- osmdata::osmdata_sf(q)
    built <- sf_rbind_compact(raw$osm_polygons, raw$osm_multipolygons)
    if (is.null(built)) return(NULL)

    built
}
# 
# ---- read.inputs.and.match.export ----
# main dish
# Read source point tables (must contain: lat, lon; plus ids)
google <- 
  readr::read_csv(cfg$g_path, show_col_types = FALSE)
osm <- 
  readr::read_csv(cfg$o_path, show_col_types = FALSE)
# Convert to sf (lon/lat)
g_sf <- 
  sf::st_as_sf(google, coords = c("lon", "lat"), crs = cfg$crs_ll, remove = FALSE)
o_sf <- 
  sf::st_as_sf(osm,    coords = c("lon", "lat"), crs = cfg$crs_ll, remove = FALSE)
# Evaluate multiple distance thresholds (optional diagnostics)
out <- 
  eval_sets_by_distances(g_sf, o_sf, cfg$dist_vec_m)
print(out$summary)
# Choose a single threshold and split
spl <- 
  split_by_distance(g_sf, o_sf, dist_thr_m = cfg$dist_thr_m)
# Union table
union_tbl <- make_union_table(spl)
# Create output dir if not exists
if (!dir.exists("output")) dir.create("output", recursive = TRUE)
# save
readr::write_excel_csv(union_tbl, sprintf("output/union%dm.csv", cfg$dist_thr_m))
# 
# My Maps exports (single files)
u <- prep_mymaps_csv(union_tbl)
readr::write_excel_csv(dplyr::filter(u, MatchStatus == "matched"),
                 sprintf("output/mymaps_matched_%dm.csv", cfg$dist_thr_m))
readr::write_excel_csv(dplyr::filter(u, MatchStatus == "google_only"),
                 "output/mymaps_google_only.csv")
readr::write_excel_csv(dplyr::filter(u, MatchStatus == "osm_only"),
                 "output/mymaps_osm_only.csv")

# My Maps exports (split to <2000 points each)
res_google <- 
  split_write_mymaps(u, "google_only", "output/mymaps_google_only", max_n = cfg$max_mymaps)
res_osm    <- 
  split_write_mymaps(u, "osm_only",    "output/mymaps_osm_only",    max_n = cfg$max_mymaps)

# ---- road.proximity ----

# Points to analyze: union output (My Maps table) -> sf
pts <- 
  u %>%
  dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
  sf::st_as_sf(coords = c("Longitude","Latitude"), crs = cfg$crs_ll, remove = FALSE)
# Expand bbox for OSM queries
bb2 <- 
  expand_bbox(sf::st_bbox(pts), cfg$bbox_expand_deg)
# Fetch all roads needed (major+minor), then split
roads <- 
  fetch_osm_roads(bb2, unique(c(cfg$major_vals, cfg$minor_vals)))
if (is.null(roads) || nrow(roads) == 0) {
  stop("No road features returned from OSM for the given bbox/highway values.")
}

stopifnot("highway" %in% names(roads))
stopifnot("highway" %in% names(roads))

roads_major <- 
  roads %>% 
  dplyr::filter(highway %in% cfg$major_vals)
roads_minor <- 
  roads %>% 
  dplyr::filter(highway %in% cfg$minor_vals)

# Project to UTM for meter distances
pts_utm <- sf::st_transform(pts, cfg$crs_utm)
roads_major_utm <- sf::st_transform(roads_major, cfg$crs_utm)
roads_minor_utm <- sf::st_transform(roads_minor, cfg$crs_utm)

# Compute distances (m)
pts_dist2 <- pts %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    dist_to_major_m = dist_to_nearest(pts_utm, roads_major_utm),
    dist_to_minor_m = dist_to_nearest(pts_utm, roads_minor_utm)
  )

# Summary by group
summ_roads <- pts_dist2 %>%
  dplyr::group_by(MatchStatus) %>%
  dplyr::summarise(
    n = dplyr::n(),
    median_major = stats::median(dist_to_major_m, na.rm = TRUE),
    p90_major    = as.numeric(stats::quantile(dist_to_major_m, 0.90, na.rm = TRUE)),
    share_major_le25 = mean(dist_to_major_m <= 25, na.rm = TRUE),
    median_minor = stats::median(dist_to_minor_m, na.rm = TRUE),
    p90_minor    = as.numeric(stats::quantile(dist_to_minor_m, 0.90, na.rm = TRUE)),
    share_minor_le25 = mean(dist_to_minor_m <= 25, na.rm = TRUE),
    .groups = "drop"
  )
# 
# ---- built-up.metrics ----
# Fetch built-up proxy polygons
built_poly_ll <- 
  fetch_osm_landuse_built(bb2, cfg$built_vals)
stopifnot(nrow(built_poly_ll) > 0)
# Project to UTM and fix invalid polygons BEFORE intersections
built_utm <- 
  built_poly_ll %>%
  sf::st_transform(cfg$crs_utm) %>%
  fix_polygons_valid()
# Make grid (UTM meters) for built_share
grid <- make_grid_utm(pts_utm, cellsize = cfg$grid_cell_m)

# Compute built area within each grid cell:
# - intersect (built polygons) x (grid cells)
# - compute area of intersected pieces
# - sum by cell
built_int <- 
  suppressWarnings(sf::st_intersection(
    built_utm %>% dplyr::select(landuse),
    grid
))

built_area_by_cell <- 
  built_int %>%
  dplyr::mutate(a = as.numeric(sf::st_area(sf::st_geometry(.)))) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(cell_id) %>%
  dplyr::summarise(
    built_area_m2 = sum(a, na.rm = TRUE),
    .groups = "drop"
  )

# Attach built_area and compute built_share = built_area / cell_area
grid2 <- 
  grid %>%
  dplyr::left_join(built_area_by_cell, by = "cell_id") %>%
  dplyr::mutate(
    built_area_m2 = dplyr::coalesce(built_area_m2, 0),
    cell_area_m2  = as.numeric(sf::st_area(sf::st_geometry(.))),
    built_share   = built_area_m2 / cell_area_m2
  )

# 1. inside built polygon?
inside_mat <- sf::st_intersects(pts_utm, built_utm, sparse = FALSE)
inside_built <- apply(inside_mat, 1, any)

# 2. distance to nearest built polygon (meters)
dist_to_built_m <- dist_to_nearest(pts_utm, built_utm)
dist_to_built_m[inside_built] <- 0

pts_built <- pts %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(
    inside_built = inside_built,
    dist_to_built_m = dist_to_built_m
  )

# Join built_share to points using grid membership
pts_cell <- sf::st_join(sf::st_as_sf(pts_utm), grid2, join = sf::st_within) %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(MatchStatus, built_share)

# Summarise B by group (distances + built_share distribution)
summ_B <- 
  pts_built %>%
  dplyr::group_by(MatchStatus) %>%
  dplyr::summarise(
    n = dplyr::n(),
    share_inside_built  = mean(inside_built, na.rm = TRUE),
    median_dist_built_m = stats::median(dist_to_built_m, na.rm = TRUE),
    p90_dist_built_m    = as.numeric(stats::quantile(dist_to_built_m, 0.90, na.rm = TRUE)),
    share_dist_le_50m   = mean(dist_to_built_m <= 50,  na.rm = TRUE),
    share_dist_le_200m  = mean(dist_to_built_m <= 200, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    pts_cell %>%
      dplyr::group_by(MatchStatus) %>%
      dplyr::summarise(
        median_built_share = stats::median(built_share, na.rm = TRUE),
        p90_built_share    = as.numeric(stats::quantile(built_share, 0.90, na.rm = TRUE)),
        share_cell_ge_0.25 = mean(built_share >= 0.25, na.rm = TRUE),
        share_cell_ge_0.50 = mean(built_share >= 0.50, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "MatchStatus"
  )

print(summ_B, width = Inf)

# ---- A.B.integration ----

# Keep the row-wise alignment (pts_dist2 and pts_built created from same 'pts')
stopifnot(nrow(pts_dist2) == nrow(pts_built))

ab <- 
  pts_dist2 %>%
  dplyr::transmute(MatchStatus, dist_to_minor_m, dist_to_major_m) %>%
  dplyr::bind_cols(pts_built %>% dplyr::select(inside_built, dist_to_built_m)) %>%
  dplyr::bind_cols(pts_cell %>% dplyr::select(built_share))

# 5x5 quantile grid for built_share x minor-road distance
ab2 <- 
  ab %>%
  dplyr::mutate(
    q_built = dplyr::ntile(built_share, 5),
    q_minor = dplyr::ntile(dist_to_minor_m, 5)
  ) %>%
  dplyr::count(MatchStatus, q_built, q_minor, name = "n") %>%
  dplyr::group_by(MatchStatus) %>%
  dplyr::mutate(share = n / sum(n)) %>%
  dplyr::ungroup()

readr::write_excel_csv(ab2, "output/ab2.csv")
# 
# ---- bands.on.ab2  ----
summ_by_band <- function(df, band_name, cond_expr){
  df %>%
    dplyr::filter({{cond_expr}}) %>%
    dplyr::group_by(MatchStatus) %>%
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    dplyr::mutate(
      band = band_name,
      total = sum(n),
      share = n / total
    ) %>%
    dplyr::select(band, MatchStatus, n, total, share)
}

urban <- summ_by_band(ab2, "urban_q4_5", q_built %in% c(4,5))
rural <- summ_by_band(ab2, "rural_q1_2", q_built %in% c(1,2))
near_minor <- summ_by_band(ab2, "minor_near_q1_2", q_minor %in% c(1,2))
far_minor  <- summ_by_band(ab2, "minor_far_q4_5",  q_minor %in% c(4,5))

print(dplyr::bind_rows(urban, rural))
print(dplyr::bind_rows(near_minor, far_minor))

# ---- urban.rural.match.rates ----
# using built_share threshold (>= 0.5)
# Here we stratify by "grid2 built_share" attached to each original point set (Google/OSM)

# Attach built_share to each Google/OSM point based on grid2 membership
g_utm <- sf::st_transform(g_sf, cfg$crs_utm)
o_utm <- sf::st_transform(o_sf, cfg$crs_utm)

g_tag <- sf::st_join(g_utm, grid2, join = sf::st_within) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(place_id, built_share) %>%
  dplyr::mutate(band = dplyr::if_else(built_share >= cfg$urban_thr, "urban", "rural"))

o_tag <- sf::st_join(o_utm, grid2, join = sf::st_within) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(osm_id, built_share) %>%
  dplyr::mutate(band = dplyr::if_else(built_share >= cfg$urban_thr, "urban", "rural"))

# Label each matched pair as urban/rural/mixed depending on both sides
pairsX <- spl$pairs %>%
  dplyr::left_join(g_tag, by = "place_id") %>% dplyr::rename(band_g = band) %>%
  dplyr::left_join(o_tag, by = "osm_id")   %>% dplyr::rename(band_o = band) %>%
  dplyr::mutate(band = dplyr::if_else(band_g == band_o, band_g, "mixed"))

# Compute match rates within each band
summ_band <- function(band_label){
  
  g_ids <- g_tag %>% dplyr::filter(band == band_label) %>% dplyr::pull(place_id) %>% unique()
  o_ids <- o_tag %>% dplyr::filter(band == band_label) %>% dplyr::pull(osm_id)   %>% unique()
  p <- pairsX %>% dplyr::filter(band == band_label)
  
  tibble::tibble(
    band = band_label,
    dist_m = cfg$dist_thr_m,
    n_google = length(g_ids),
    n_osm = length(o_ids),
    n_pairs = nrow(p),
    share_google_matched = ifelse(length(g_ids) > 0, nrow(p) / length(g_ids), NA_real_),
    share_osm_matched    = ifelse(length(o_ids) > 0, nrow(p) / length(o_ids), NA_real_),
    n_mixed_pairs = sum(pairsX$band == "mixed", na.rm = TRUE)

  )
}
out_urban <- summ_band("urban")
out_rural <- summ_band("rural")
out_mixed <- summ_band("mixed")

print(dplyr::bind_rows(out_urban, out_rural, out_mixed))




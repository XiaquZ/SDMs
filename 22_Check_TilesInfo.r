library(terra)

terraOptions(progress = 1)
Sys.setenv(GDAL_NUM_THREADS = "1", OMP_NUM_THREADS = "1")

# ---------------------------------------------------------
# folders
# ---------------------------------------------------------
cur_dir <- "/lustre1/scratch/348/vsc34871/output/Binary_CurrentActual_RedList_tiles"
fut_dir <- "/lustre1/scratch/348/vsc34871/output/Binary_FutureReachable_RedList_tiles"

cur_files <- sort(list.files(cur_dir, pattern = "^tile_current_\\d+\\.tif$", full.names = TRUE))
fut_files <- sort(list.files(fut_dir, pattern = "^tile_future_\\d+\\.tif$", full.names = TRUE))

# set this to the expected number of species layers
expected_nlyr <- 48   # change if needed

# ---------------------------------------------------------
# helper
# ---------------------------------------------------------
get_tile_id <- function(x) {
  sub(".*_(\\d+)\\.tif$", "\\1", basename(x))
}

# ---------------------------------------------------------
# large-tile safe checker
# ---------------------------------------------------------
check_tile_large <- function(f, expected_nlyr = NA_integer_) {
  
  info <- data.frame(
    file = basename(f),
    tile_id = get_tile_id(f),
    exists = file.exists(f),
    size_bytes = if (file.exists(f)) file.info(f)$size else NA_real_,
    readable = FALSE,
    nrow = NA_integer_,
    ncol = NA_integer_,
    nlyr = NA_integer_,
    ncell = NA_real_,
    xmin = NA_real_,
    xmax = NA_real_,
    ymin = NA_real_,
    ymax = NA_real_,
    xres = NA_real_,
    yres = NA_real_,
    total_non_na = NA_real_,
    total_positive = NA_real_,
    min_non_na_layer = NA_real_,
    max_non_na_layer = NA_real_,
    min_positive_layer = NA_real_,
    max_positive_layer = NA_real_,
    min_value = NA_real_,
    max_value = NA_real_,
    n_empty_layers = NA_integer_,
    n_layers_without_positive = NA_integer_,
    binary_range_ok = NA,
    status = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (!isTRUE(info$exists)) {
    info$status <- "missing"
    return(info)
  }
  
  if (is.na(info$size_bytes) || info$size_bytes == 0) {
    info$status <- "zero_size_file"
    return(info)
  }
  
  r <- try(terra::rast(f), silent = TRUE)
  if (inherits(r, "try-error")) {
    info$status <- "cannot_open"
    return(info)
  }
  
  info$readable <- TRUE
  info$nrow <- nrow(r)
  info$ncol <- ncol(r)
  info$nlyr <- nlyr(r)
  info$ncell <- ncell(r)
  
  e <- ext(r)
  info$xmin <- e$xmin
  info$xmax <- e$xmax
  info$ymin <- e$ymin
  info$ymax <- e$ymax
  
  rs <- res(r)
  info$xres <- rs[1]
  info$yres <- rs[2]
  
  if (is.na(info$nrow) || is.na(info$ncol) || info$nrow == 0 || info$ncol == 0) {
    info$status <- "invalid_geometry"
    return(info)
  }
  
  # per-layer non-NA counts
  non_na_per_layer <- try(
    sapply(seq_len(nlyr(r)), function(i) {
      x <- terra::global(!is.na(r[[i]]), "sum", na.rm = TRUE)[1, 1]
      if (is.na(x)) 0 else x
    }),
    silent = TRUE
  )
  
  if (inherits(non_na_per_layer, "try-error")) {
    info$status <- "cannot_count_non_na"
    return(info)
  }
  
  # per-layer positive counts
  positive_per_layer <- try(
    sapply(seq_len(nlyr(r)), function(i) {
      x <- terra::global(r[[i]] > 0, "sum", na.rm = TRUE)[1, 1]
      if (is.na(x)) 0 else x
    }),
    silent = TRUE
  )
  
  if (inherits(positive_per_layer, "try-error")) {
    info$status <- "cannot_count_positive"
    return(info)
  }
  
  # per-layer min/max
  rmin <- try(terra::global(r, "min", na.rm = TRUE), silent = TRUE)
  rmax <- try(terra::global(r, "max", na.rm = TRUE), silent = TRUE)
  
  min_vals <- rep(NA_real_, nlyr(r))
  max_vals <- rep(NA_real_, nlyr(r))
  
  if (!inherits(rmin, "try-error")) {
    min_vals <- rmin[, 1]
    min_vals[!is.finite(min_vals)] <- NA_real_
  }
  if (!inherits(rmax, "try-error")) {
    max_vals <- rmax[, 1]
    max_vals[!is.finite(max_vals)] <- NA_real_
  }
  
  info$total_non_na <- sum(non_na_per_layer, na.rm = TRUE)
  info$total_positive <- sum(positive_per_layer, na.rm = TRUE)
  info$min_non_na_layer <- min(non_na_per_layer, na.rm = TRUE)
  info$max_non_na_layer <- max(non_na_per_layer, na.rm = TRUE)
  info$min_positive_layer <- min(positive_per_layer, na.rm = TRUE)
  info$max_positive_layer <- max(positive_per_layer, na.rm = TRUE)
  info$n_empty_layers <- sum(non_na_per_layer == 0, na.rm = TRUE)
  info$n_layers_without_positive <- sum(positive_per_layer == 0, na.rm = TRUE)
  
  if (all(is.na(min_vals))) {
    info$min_value <- NA_real_
  } else {
    info$min_value <- min(min_vals, na.rm = TRUE)
  }
  
  if (all(is.na(max_vals))) {
    info$max_value <- NA_real_
  } else {
    info$max_value <- max(max_vals, na.rm = TRUE)
  }
  
  # for binary rasters, this is a lightweight proxy check:
  # all non-NA values should lie between 0 and 1
  info$binary_range_ok <- isTRUE(
    (is.na(info$min_value) || info$min_value >= 0) &&
    (is.na(info$max_value) || info$max_value <= 1)
  )
  
  # final classification
  if (is.na(info$total_non_na) || info$total_non_na == 0) {
    info$status <- "all_na"
  } else if (!is.na(expected_nlyr) && info$nlyr != expected_nlyr) {
    info$status <- "wrong_layer_count"
  } else if (info$n_empty_layers > 0) {
    info$status <- "some_layers_all_na"
  } else if (!isTRUE(info$binary_range_ok)) {
    info$status <- "values_outside_0_1_range"
  } else if (info$total_positive == 0) {
    info$status <- "all_zero_but_valid"
  } else {
    info$status <- "ok"
  }
  
  info
}

# ---------------------------------------------------------
# run checks
# ---------------------------------------------------------
cur_check <- do.call(rbind, lapply(cur_files, check_tile_large, expected_nlyr = expected_nlyr))
fut_check <- do.call(rbind, lapply(fut_files, check_tile_large, expected_nlyr = expected_nlyr))

# ---------------------------------------------------------
# suspicious tiles
# ---------------------------------------------------------
cur_problems <- subset(cur_check, status != "ok")
fut_problems <- subset(fut_check, status != "ok")

print(cur_problems)
print(fut_problems)

# ---------------------------------------------------------
# save outputs
# ---------------------------------------------------------
write.csv(cur_check,
          file = file.path(cur_dir, "check_current_tiles_large_safe.csv"),
          row.names = FALSE)

write.csv(fut_check,
          file = file.path(fut_dir, "check_future_tiles_large_safe.csv"),
          row.names = FALSE)
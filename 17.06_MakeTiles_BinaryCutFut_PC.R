library(terra)
library(sf)

# -----------------------------
# Settings
# -----------------------------
terraOptions(
  memfrac = 0.7,
  progress = 1,
  threads = 1
)

Sys.setenv(
  GDAL_NUM_THREADS = "1",
  OMP_NUM_THREADS  = "1"
)

# -----------------------------
# Read Europe shape
# -----------------------------
eu_shape <- read_sf("I:/EUshap/Europe.shp")
eu_shape <- st_buffer(eu_shape, 1)

# Use much smaller tiles than 900 km
cellsize <- 6e5   # 200 km; try 1e5 or 1.5e5 if still slow

grid <- st_make_grid(eu_shape, cellsize = c(cellsize, cellsize)) |>
  st_as_sf()

grid_vect <- vect(grid)
plot(eu_shape$geometry, main = "Grid over Europe")
plot(grid_vect, add = TRUE, border = "blue", lwd = 0.5)
# -----------------------------
# Input files
# -----------------------------
cur_files <- list.files(
  "I:/DATA/SDM_current/results/Binary_SpeciesCurrentAtual/",
  pattern = "\\.tif$",
  full.names = TRUE
)

fut_files <- list.files(
  "I:/DATA/SDM_future/Binary_FutureReachableDis/",
  pattern = "\\.tif$",
  full.names = TRUE
)

to_species_key <- function(x) {
  x <- basename(x)
  x <- sub("\\.tif$", "", x, ignore.case = TRUE)
  x <- sub("^binary[_ ]+", "", x, ignore.case = TRUE)
  x <- sub("(_CurrentActual|_CurrentAtual|_Current)$", "", x, ignore.case = TRUE)
  x <- sub("(_FuturePotentialReachable|_FuturePotential|_FutureReachable|_Future)$", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

cur_key <- to_species_key(cur_files)
fut_key <- to_species_key(fut_files)

if (!setequal(cur_key, fut_key)) {
  stop(
    "Species sets differ.\n",
    "Missing in future: ", paste(setdiff(cur_key, fut_key), collapse = ", "), "\n",
    "Missing in current: ", paste(setdiff(fut_key, cur_key), collapse = ", ")
  )
}

fut_files <- fut_files[match(cur_key, fut_key)]

# Lazy raster stacks
cur_stack <- rast(cur_files)
fut_stack <- rast(fut_files)

if (!compareGeom(cur_stack, fut_stack, stopOnError = FALSE)) {
  stop("Current and future stacks do not have identical geometry.")
}

# -----------------------------
# Output
# -----------------------------
out_dir_cur <- "I:/DATA/SDM_current/results/Binary_CurrentActual_tiles/"
out_dir_fut <- "I:/DATA/SDM_future/Binary_FutureReachable_tiles/"

dir.create(out_dir_cur, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_fut, recursive = TRUE, showWarnings = FALSE)

wopt <- list(
  datatype = "INT1U",
  gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
)

# -----------------------------
# Get tile extents only (no writing yet)
# -----------------------------
exts <- getTileExtents(cur_stack, grid_vect)

# -----------------------------
# Helper: jointly empty?
# For binary rasters, max <= 0 means no presence.
# all NA means empty / outside raster support.
# -----------------------------
is_jointly_empty <- function(r1, r2) {
  mx1 <- suppressWarnings(global(r1, "max", na.rm = TRUE)[1, ])
  mx2 <- suppressWarnings(global(r2, "max", na.rm = TRUE)[1, ])

  empty1 <- all(is.na(mx1) | mx1 <= 0)
  empty2 <- all(is.na(mx2) | mx2 <= 0)

  empty1 && empty2
}

# -----------------------------
# Process one tile at a time
# -----------------------------
for (i in seq_along(exts)) {
  message("Tile ", i, " / ", length(exts))

  e <- exts[[i]]

  # Crop each stack only for this tile
  cur_tile <- crop(cur_stack, e, snap = "out")
  fut_tile <- crop(fut_stack, e, snap = "out")

  # Skip if both are empty
  if (is_jointly_empty(cur_tile, fut_tile)) {
    message("  skipped jointly empty tile: ", i)
    rm(cur_tile, fut_tile)
    gc()
    next
  }

  fcur <- file.path(out_dir_cur, sprintf("tile_current_%03d.tif", i))
  ffut <- file.path(out_dir_fut, sprintf("tile_future_%03d.tif", i))

  writeRaster(cur_tile, fcur, overwrite = TRUE, wopt = wopt)
  writeRaster(fut_tile, ffut, overwrite = TRUE, wopt = wopt)

  rm(cur_tile, fut_tile)
  gc()
}
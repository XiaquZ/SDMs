library(terra)

terraOptions(memfrac = 0.8, progress = 1, threads = 1)

# -----------------------------
# Files and folders
# -----------------------------
cur_tile_dir <- "/lustre1/scratch/348/vsc34871/output/Binary_CurrentActual_tiles/"
fut_files_dir <- "/lustre1/scratch/348/vsc34871/Binary_futActual/"
out_dir_fut <- "/lustre1/scratch/348/vsc34871/output/Binary_FutureReachable_tiles/"

dir.create(out_dir_fut, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Stable species ordering
# -----------------------------
to_species_key <- function(x) {
  x <- basename(x)
  x <- sub("\\.tif$", "", x, ignore.case = TRUE)
  x <- sub("^binary[_ ]+", "", x, ignore.case = TRUE)
  x <- sub(
    "(_FuturePotentialReachable|_FuturePotential|_FutureReachable|_Future)$",
    "",
    x,
    ignore.case = TRUE
  )
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# -----------------------------
# Load all future species rasters
# -----------------------------
fut_files <- list.files(
  fut_files_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

if (length(fut_files) == 0) {
  stop("No future .tif files found in: ", fut_files_dir)
}

fut_files <- fut_files[order(to_species_key(fut_files))]
fut_stack <- rast(fut_files)

# -----------------------------
# Rebuild one future tile
# using the CURRENT tile as template
# -----------------------------
rebuild_future_tile_from_current_template <- function(tile_num,
                                                      fut_stack,
                                                      cur_tile_dir,
                                                      out_dir,
                                                      overwrite = TRUE) {
  stopifnot(length(tile_num) == 1)

  tile_id <- sprintf("%03d", as.integer(tile_num))

  cur_tile_file <- file.path(cur_tile_dir, paste0("tile_current_", tile_id, ".tif"))
  out_file <- file.path(out_dir, paste0("tile_future_", tile_id, ".tif"))

  if (!file.exists(cur_tile_file)) {
    stop("Current tile template not found: ", cur_tile_file)
  }

  message("Rebuilding future tile ", tile_id)
  message("Using current tile template: ", cur_tile_file)

  # Current tile is the exact geometry template we want
  cur_template <- rast(cur_tile_file)

  # Crop future stack to the exact current tile extent
  tile_r <- crop(fut_stack, cur_template, snap = "near")

  # Check geometry matches exactly
  if (!compareGeom(cur_template, tile_r, stopOnError = FALSE)) {
    message("Current template geometry:")
    print(cur_template)
    message("Rebuilt future tile geometry:")
    print(tile_r)
    stop("Rebuilt future tile geometry does not match current tile template for tile ", tile_id)
  }

  # Optional: keep names aligned to current tile
  if (nlyr(cur_template) == nlyr(tile_r)) {
    names(tile_r) <- names(cur_template)
  }

  writeRaster(
    tile_r,
    out_file,
    overwrite = overwrite,
    wopt = list(
      datatype = "INT1U",
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
    )
  )

  message("Saved: ", out_file)
  invisible(out_file)
}

# -----------------------------
# Rebuild only tiles 14 and 17
# -----------------------------
rebuild_future_tile_from_current_template(
  tile_num = 14,
  fut_stack = fut_stack,
  cur_tile_dir = cur_tile_dir,
  out_dir = out_dir_fut,
  overwrite = TRUE
)

rebuild_future_tile_from_current_template(
  tile_num = 17,
  fut_stack = fut_stack,
  cur_tile_dir = cur_tile_dir,
  out_dir = out_dir_fut,
  overwrite = TRUE
)

# -----------------------------
# Optional checks
# -----------------------------
tile14_cur <- rast(file.path(cur_tile_dir, "tile_current_014.tif"))
tile14_fut <- rast(file.path(out_dir_fut, "tile_future_014.tif"))
compareGeom(tile14_cur, tile14_fut, stopOnError = FALSE, messages = TRUE)

tile17_cur <- rast(file.path(cur_tile_dir, "tile_current_017.tif"))
tile17_fut <- rast(file.path(out_dir_fut, "tile_future_017.tif"))
compareGeom(tile17_cur, tile17_fut, stopOnError = FALSE, messages = TRUE)
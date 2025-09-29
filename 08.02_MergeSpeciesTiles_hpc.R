library(terra)

# --- Config ---
in_dir    <- "/lustre1/scratch/348/vsc34871/SDM_current/Results/"
out_dir   <- "/lustre1/scratch/348/vsc34871/SDM_current/Species_final/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

target_crs <- "EPSG:3035"
overwrite  <- TRUE

# --- Gather files and group by species prefix (before `_tile`) ---
files   <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
species <- sub("_tile.*$", "", basename(files))
groups  <- split(files[order(files)], species[order(files)])

for (sp in names(groups)) {
  sp_files <- groups[[sp]]

  # Only proceed if exactly 16 tiles
  if (length(sp_files) != 16) {
    cat("Skipping", sp, "- has", length(sp_files), "tiles (expected 16)\n")
    next
  }

  cat("Processing:", sp, "\n")

  # Build a VRT mosaic (super cheap in RAM)
  vrt_file <- file.path(out_dir, paste0(sp, ".vrt"))
  r <- try(
    vrt(sp_files, filename = vrt_file, crs = target_crs, overwrite = TRUE),
     silent = TRUE)
  if (inherits(r, "try-error")) {
    cat("  ❌ Failed to build VRT for", sp, "\n")
    next
  }

   # Make sure CRS is attached
  if (is.na(crs(r))) {
    crs(r) <- target_crs
    cat("  ℹ️  CRS assigned:", target_crs, "\n")
  }

  # Write GeoTIFF with good defaults
  out_tif <- file.path(out_dir, paste0(sp, "_3035_current.tif"))
  # Streamed write with good defaults
  gdal_opts <- c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=IF_SAFER")

  tryCatch({
    writeRaster(r, out_tif, overwrite = overwrite, gdal = gdal_opts)
    cat("  ✅ Wrote:", out_tif, "\n")
  }, error = function(e) {
    cat("  ❌ Write failed for", sp, ":", conditionMessage(e), "\n")
  })
 
}

library(terra)

# Merge all tiles per species folder and save to an output folder
merge_species_tiles <- function(tiles_root,
                                out_dir,
                                pattern = "\\.tif$",
                                
                                crs_epsg = "EPSG:3035",
                                overwrite = TRUE) {
  # make sure output folder exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # list immediate subfolders; each subfolder = one species
  species_dirs <- list.dirs(tiles_root, recursive = FALSE, full.names = TRUE)

  if (length(species_dirs) == 0) {
    message("No species folders found in: ", tiles_root)
    return(invisible(NULL))
  }

  for (sd in species_dirs) {
    species <- basename(sd)
    t.lst <- list.files(sd, pattern = pattern, full.names = TRUE)

    if (length(t.lst) == 0) {
      message("Skipping '", species, "': no matching tiles.")
      next
    }

    message("Merging ", length(t.lst), " tiles for species: ", species)

    # Build a virtual mosaic and write out a real raster
    # (vrt() avoids loading all tiles into memory at once)
    r <- try(vrt(t.lst), silent = TRUE)
    if (inherits(r, "try-error")) {
      message("  ❌ Failed to build VRT for '", species, "'. Skipping.")
      next
    }

    # Round and set CRS
    crs(r) <- crs_epsg

    # Output file path
    fout <- file.path(out_dir, paste0(species, ".tif"))

    # Write; this materializes the VRT mosaic to disk
    tryCatch({
      writeRaster(r, fout, overwrite = overwrite)
      message("  ✅ Wrote: ", fout)
    }, error = function(e) {
      message("  ❌ Failed to write '", species, "': ", conditionMessage(e))
    })
  }

  invisible(NULL)
}

# ---------- Use it ----------
# Each subfolder under 'Species_tiles' should be a species name containing its tiles
tiles_root <- "D:/SDMs/SDMs_current/Results/Species_tiles"
out_dir    <- "D:/SDMs/SDMs_current/Results/Species_final"

merge_species_tiles(tiles_root, out_dir)

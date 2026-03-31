library(terra)

in_dir  <- "/lustre1/scratch/348/vsc34871/output/tile_output_beta/"
out_dir <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU/"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

get_tile_num <- function(x) {
  as.integer(sub("^tile_(\\d+)_.*\\.tif$", "\\1", basename(x)))
}

merge_one <- function(pattern, out_file, datatype) {
  files <- list.files(in_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No files found for pattern: ", pattern)
  }

  tile_nums <- get_tile_num(files)

  if (any(is.na(tile_nums))) {
    bad <- basename(files[is.na(tile_nums)])
    stop("Could not extract tile number from: ", paste(bad, collapse = ", "))
  }

  files <- files[order(tile_nums)]

  v <- terra::vrt(files)

  terra::writeRaster(
    v,
    filename = file.path(out_dir, out_file),
    overwrite = TRUE,
    wopt = list(
      datatype = datatype,
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
    )
  )

  message("Written: ", file.path(out_dir, out_file))
}

merge_one("^tile_\\d+_richness_current\\.tif$", "EU_richness_current.tif", "INT2U")
merge_one("^tile_\\d+_richness_future\\.tif$", "EU_richness_future.tif", "INT2U")
merge_one("^tile_\\d+_richness_change\\.tif$", "EU_richness_change_future_minus_current.tif", "INT2S")
merge_one("^tile_\\d+_richness_ratio\\.tif$", "EU_richness_ratio.tif", "FLT4S")
merge_one("^tile_\\d+_abc\\.tif$", "EU_abc_shared_loss_gain.tif", "INT2U")
merge_one("^tile_\\d+_beta\\.tif$", "EU_beta_Baselga2010_sor_sim_nes.tif", "FLT4S")
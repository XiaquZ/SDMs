#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(terra)
})

# ---------------------------------------------------------
# HPC terra / GDAL settings
# ---------------------------------------------------------
set_terra_hpc <- function() {
  terra::terraOptions(
    memfrac = 0.8,
    progress = 1,
    threads = 1
  )
  Sys.setenv(
    GDAL_NUM_THREADS = "1",
    OMP_NUM_THREADS  = "1"
  )
  invisible(TRUE)
}

# ---------------------------------------------------------
# Standard write options
# ---------------------------------------------------------
wopt_int2u <- function() {
  list(
    datatype = "INT2U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
}

wopt_int2s <- function() {
  list(
    datatype = "INT2S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
}

wopt_flt4s <- function() {
  list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
}

# ---------------------------------------------------------
# Rename raster layers and save to a new file
# ---------------------------------------------------------
rename_raster_stack <- function(infile,
                                outfile,
                                new_names,
                                overwrite = TRUE,
                                wopt = NULL) {
  set_terra_hpc()

  if (!file.exists(infile)) {
    stop("Input file does not exist: ", infile)
  }

  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)

  r <- terra::rast(infile)

  message("Input file: ", infile)
  message("Number of layers found: ", terra::nlyr(r))
  message("Current names: ", paste(names(r), collapse = ", "))

  if (length(new_names) != terra::nlyr(r)) {
    stop(
      "Length of 'new_names' (", length(new_names),
      ") does not match number of layers in raster (", terra::nlyr(r), ")."
    )
  }

  names(r) <- new_names

  message("New names: ", paste(names(r), collapse = ", "))
  message("Writing renamed raster to: ", outfile)

  if (is.null(wopt)) {
    terra::writeRaster(r, outfile, overwrite = overwrite)
  } else {
    terra::writeRaster(r, outfile, overwrite = overwrite, wopt = wopt)
  }

  message("Finished writing: ", outfile)
  message("--------------------------------------------------")

  invisible(outfile)
}

# ---------------------------------------------------------
# Example 1: Rename merged beta raster
# ---------------------------------------------------------
beta_in  <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU/EU_beta_Baselga2010_sor_sim_nes.tif"
beta_out <- "/lustre1/scratch/348/vsc34871/output/Merge_Beta_EU/EU_beta_Baselga2010_sor_sim_nes_named.tif"

rename_raster_stack(
  infile    = beta_in,
  outfile   = beta_out,
  new_names = c("beta_sor", "beta_sim", "beta_nes"),
  overwrite = TRUE,
  wopt      = wopt_flt4s()
)

# ---------------------------------------------------------
# Example 2: Rename merged abc raster
# ---------------------------------------------------------
abc_in  <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU/EU_abc_shared_loss_gain.tif"
abc_out <- "/lustre1/scratch/348/vsc34871/output/Merge_Beta_EU/EU_abc_named.tif"

rename_raster_stack(
  infile    = abc_in,
  outfile   = abc_out,
  new_names = c("a_shared", "b_losses", "c_gains"),
  overwrite = TRUE,
  wopt      = wopt_int2u()
)

# ---------------------------------------------------------
# Optional check
# ---------------------------------------------------------
beta_check <- terra::rast(beta_out)
abc_check  <- terra::rast(abc_out)

message("beta_out names: ", paste(names(beta_check), collapse = ", "))
message("abc_out names: ", paste(names(abc_check), collapse = ", "))
library(future)
library(future.apply)
library(terra)

#############################################
#### Future potential and reachale range ####
#############################################
# ---- HPC temp dir (define ONCE) ----
tmpdir <- Sys.getenv("SLURM_TMPDIR", unset = tempdir())
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

# Optional: quiet + avoid weird progress in logs
terraOptions(progress = 0)

# 1. Paths
conh_dir   <- "/lustre1/scratch/348/vsc34871/input/Convexhull_Buffer/"
binary_dir <- "/lustre1/scratch/348/vsc34871/input/BinaryMaps_original/"
out_dir    <- "/lustre1/scratch/348/vsc34871/SDM_fut/results/BinaryMaps_FuturePotentialRechableDist/"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sp_lst <- list.files(binary_dir, pattern = "\\.tif$", full.names = TRUE)

# ---- worker function ----
process_one_species <- function(sp, conh_dir, out_dir, tmpdir) {
  # load terra inside worker (good practice for multisession)
  suppressPackageStartupMessages(library(terra))

  # terra temp in each worker (important!)
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  terraOptions(tempdir = tmpdir, progress = 0, threads = 1)
  Sys.setenv(GDAL_NUM_THREADS = "1")
  
  sp_name <- gsub("^binary_|\\.tif$", "", basename(sp))
  
  conh_path <- file.path(conh_dir, paste0(sp_name, "_ConvexHullBuffer.shp"))
  if (!file.exists(conh_path)) {
    return(list(species = sp_name, ok = FALSE, msg = "Convex hull not found"))
  }
  
  out_file <- file.path(out_dir, paste0(sp_name, "_FuturePotentialReachable.tif"))
  if (file.exists(out_file)) {
    return(list(species = sp_name, ok = TRUE, msg = "Output already exists (skipped)", out = out_file))
  }
  
  # Read data
  conh_poly <- vect(conh_path)
  bin_rast  <- rast(sp)
  
  # # 1) original NA mask
  # orig_na <- is.na(bin_rast)
  
  # # mask within hull
  # actual_dist <- mask(bin_rast, conh_poly)
  
  # # outside hull -> 0
  # zero_raster <- actual_dist
  # values(zero_raster) <- 0
  # actual_dist02 <- cover(actual_dist, zero_raster)
  
  # # restore original NA everywhere
  # actual_dist02[orig_na] <- NA

   # --- Memory-lean approach ---
  # Set cells OUTSIDE hull to 0, but KEEP original NA values
  # Default updateNA=FALSE => NA cells remain NA (this replaces your orig_na + cover workflow)
  actual_dist02 <- mask(
    x = bin_rast,
    mask = conh_poly,
    inverse = TRUE,
    updatevalue = 0
  )

  # write
  writeRaster(actual_dist02, out_file, overwrite = TRUE)

  # help GC in long runs
  rm(conh_poly, bin_rast, actual_dist02)
  gc()
  
  list(species = sp_name, ok = TRUE, msg = "saved", out = out_file)
}

# ---- run in parallel ----
# Use Slurm allocation if present
slurm_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = NA))
if (is.na(slurm_cpus)) {
  # fallback for non-slurm runs
  slurm_cpus <- parallel::detectCores(logical = TRUE)
}

n_workers <- min(12, max(1, slurm_cpus - 1))

# On Linux HPC, multicore is usually best INSIDE a Slurm job allocation
plan(multisession, workers = n_workers)

# If you hit "future.globals.maxSize" errors (large rasters), increase this:
#options(future.globals.maxSize = 8 * 1024^3)  # 8 GB

results <- future_lapply(
  sp_lst,
  FUN = process_one_species,
  conh_dir = conh_dir,
  out_dir  = out_dir,
  tmpdir   = tmpdir
)
# show failures
fail <- Filter(function(x) !isTRUE(x$ok), results)
if (length(fail) > 0) {
  cat("\nFailures:\n")
  for (x in fail) cat("-", x$species, ":", x$msg, "\n")
}

# Optional: quick summary
ok <- vapply(results, `[[`, logical(1), "ok")
cat("Done. Success:", sum(ok), " / ", length(ok), "\n")
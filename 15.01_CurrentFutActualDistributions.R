library(future)
library(future.apply)
library(terra)

#### Current Actual Distributions ####

# 1. Paths
conh_dir   <- "I:/DATA/output/ConvexHull"
binary_dir <- "G:/SDMs/SDMs_current/Results/OriginalBinaryMaps_aligned"
out_dir    <- "G:/SDMs/SDMs_current/Results/Binary_SpeciesCurrentAtualDistri"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sp_lst <- list.files(binary_dir, pattern = "\\.tif$", full.names = TRUE)

# ---- worker function ----
process_one_species <- function(sp, conh_dir, out_dir) {
  # load terra inside worker (good practice for multisession)
  library(terra)
  
  sp_name <- gsub("^binary_|\\.tif$", "", basename(sp))
  sp_name <- gsub(" ", "_", sp_name)
  
  conh_path <- file.path(conh_dir, paste0(sp_name, "_ConvexHull.shp"))
  if (!file.exists(conh_path)) {
    return(list(species = sp_name, ok = FALSE, msg = "Convex hull not found"))
  }
  
  out_file <- file.path(out_dir, paste0(sp_name, "_CurrentActual.tif"))
  if (file.exists(out_file)) {
    return(list(species = sp_name, ok = TRUE, msg = "Output already exists (skipped)", out = out_file))
  }
  
  # Read data
  conh_poly <- vect(conh_path)
  bin_rast  <- rast(sp)
  
  # 1) original NA mask
  orig_na <- is.na(bin_rast)
  
  # mask within hull
  actual_dist <- mask(bin_rast, conh_poly)
  
  # outside hull -> 0
  zero_raster <- actual_dist
  values(zero_raster) <- 0
  actual_dist02 <- cover(actual_dist, zero_raster)
  
  # restore original NA everywhere
  actual_dist02[orig_na] <- NA
  
  # write
  writeRaster(actual_dist02, out_file, overwrite = TRUE)
  
  list(species = sp_name, ok = TRUE, msg = "saved", out = out_file)
}

# ---- run in parallel ----
# Choose number of workers (leave 1–2 cores free)
n_workers <- min(8, max(2, floor(parallel::detectCores(logical = TRUE) * 0.25)))
plan(multisession, workers = n_workers)

# If you hit "future.globals.maxSize" errors (large rasters), increase this:
#options(future.globals.maxSize = 8 * 1024^3)  # 8 GB

results <- future_lapply(
  sp_lst,
  FUN = process_one_species,
  conh_dir = conh_dir,
  out_dir  = out_dir,
  future.seed = TRUE
)

# back to sequential when done
plan(sequential)

#### Future potential and reachale range ####

# 1. Paths
conh_dir   <- "I:/DATA/output/ConvexHull_Buffer"
binary_dir <- "N:/SDMs/SDMs_future/Results/BinaryMaps_original"
out_dir    <- "N:/SDMs/SDMs_future/Results/Binary_FuturePotentialRechableDist"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sp_lst <- list.files(binary_dir, pattern = "\\.tif$", full.names = TRUE)
# test code below.
sp <- sp_lst[[1]]

# ---- worker function ----
process_one_species <- function(sp, conh_dir, out_dir) {
  # load terra inside worker (good practice for multisession)
  library(terra)
  
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
  
  # 1) original NA mask
  orig_na <- is.na(bin_rast)
  
  # mask within hull
  actual_dist <- mask(bin_rast, conh_poly)
  
  # outside hull -> 0
  zero_raster <- actual_dist
  values(zero_raster) <- 0
  actual_dist02 <- cover(actual_dist, zero_raster)
  
  # restore original NA everywhere
  actual_dist02[orig_na] <- NA
  
  # write
  writeRaster(actual_dist02, out_file, overwrite = TRUE)
  
  list(species = sp_name, ok = TRUE, msg = "saved", out = out_file)
}

# ---- run in parallel ----
# Choose number of workers (leave 1–2 cores free)
n_workers <- min(8, max(2, floor(parallel::detectCores(logical = TRUE) * 0.25)))
plan(multisession, workers = n_workers)

# If you hit "future.globals.maxSize" errors (large rasters), increase this:
#options(future.globals.maxSize = 8 * 1024^3)  # 8 GB

results <- future_lapply(
  sp_lst,
  FUN = process_one_species,
  conh_dir = conh_dir,
  out_dir  = out_dir,
  future.seed = TRUE
)

# back to sequential when done
plan(sequential)

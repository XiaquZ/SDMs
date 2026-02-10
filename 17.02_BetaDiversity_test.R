library(terra)

## Load belgium shapefile.
be_shp <- vect("G:/EuropeShapefile/Shapefiles/Belgium.shp")
input_dir <- "G:/SDMs/SDMs_future/Results/BinaryMaps_original/"
output_dir <- "G:/SDMs/SDMs_future/Results/Binary_SpeciesFutReachable_be/"

## List all .tif files in the input directory.
tif_files <- list.files(
  path = input_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)
tif_file <- tif_files[[1]]
## Process each .tif file.
for (tif_file in tif_files) {
    # Load the raster file.
    raster_data <- rast(tif_file)
    
    # Crop and mask the raster using the Belgium shapefile.
    cropped_raster <- crop(raster_data, be_shp)
    masked_raster <- mask(cropped_raster, be_shp)
    
    plot(masked_raster)
    # Create output file path.
    output_file <- file.path(output_dir, basename(tif_file))
    
    # Save the processed raster to the output directory.
    writeRaster(masked_raster, filename = output_file, overwrite = TRUE)
}




#### Koenraad chatgpt test code ####
# Temporal beta diversity per pixel (current vs future)
# Sørensen dissimilarity split into turnover + nestedness (Baselga 2010)
# Parallel + disk-backed via terra::app() chunk processing
 
library(terra)
 
# ---- inputs ----
# C <- current_stack   # SpatRaster with 140 layers (0/1)
# F <- future_stack    # SpatRaster with 140 layers (0/1)
 
# Or from files (order MUST match species):
# cur_files <- list.files("path/to/current/", pattern="\\.tif$", full.names=TRUE)
# fut_files <- list.files("path/to/future/",  pattern="\\.tif$", full.names=TRUE)
# stopifnot(length(cur_files) == length(fut_files))
# C <- rast(cur_files)
# F <- rast(fut_files)
 
# ---- settings ----
cores <- max(1, parallel::detectCores() - 1)
 
# terra-wide options (threads + scratch)
terraOptions(cores = cores, memfrac = 0.8, progress = 10)
# Optional: speed up GeoTIFF writing (also helps on network drives)
gdal_threads <- max(1, cores)
Sys.setenv(GDAL_NUM_THREADS = gdal_threads)  # used by some GDAL ops/writing
 
# ---- checks ----
stopifnot(inherits(C, "SpatRaster"), inherits(F, "SpatRaster"))
stopifnot(nlyr(C) == nlyr(F))
stopifnot(compareGeom(C, F, stopOnError = FALSE))
 
# ---- ensure binary presence/absence (0/1) ----
C <- ifel(is.na(C), NA, as.int(C > 0))
F <- ifel(is.na(F), NA, as.int(F > 0))
 
S <- nlyr(C)
 
# ---- compute a,b,c in one parallel pass ----
# app() processes in blocks and can use multiple threads (cores=)
X <- c(C, F)
 
abc_fun <- function(v) {
  cur <- v[1:S]
  fut <- v[(S + 1):(2 * S)]
  a <- sum(cur == 1 & fut == 1)
  b <- sum(cur == 1 & fut == 0)
  c <- sum(cur == 0 & fut == 1)
  c(a, b, c)
}
 
abc <- app(
  X, abc_fun,
  cores = cores,
  filename = "abc_shared_loss_gain.tif",
  overwrite = TRUE,
  wopt = list(
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)
names(abc) <- c("a_shared", "b_losses", "c_gains")
 
a <- abc[[1]]
b <- abc[[2]]
c_ <- abc[[3]]
 
# ---- Baselga (2010) partitioning ----
den_sor  <- (2 * a + b + c_)
beta_sor <- (b + c_) / den_sor
 
m        <- pmin(b, c_)
den_sim  <- (a + m)
beta_sim <- m / den_sim
 
beta_sne <- beta_sor - beta_sim
 
# ---- handle empty/undefined cases (e.g., a=b=c=0) ----
beta_sor[!is.finite(beta_sor)] <- 0
beta_sim[!is.finite(beta_sim)] <- 0
beta_sne[!is.finite(beta_sne)] <- 0
 
names(beta_sor) <- "beta_sor"
names(beta_sim) <- "beta_sim_turnover"
names(beta_sne) <- "beta_sne_nestedness"
 
# ---- write outputs (disk-backed, fast GeoTIFF) ----
writeRaster(beta_sor, "beta_sor.tif", overwrite = TRUE,
            wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")))
writeRaster(beta_sim, "beta_turnover_beta_sim.tif", overwrite = TRUE,
            wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")))
writeRaster(beta_sne, "beta_nestedness_beta_sne.tif", overwrite = TRUE,
            wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")))
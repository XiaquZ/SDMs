library(terra)

#### Crop binary maps to Belgium extent ####
## Load belgium shapefile. ##
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




#### chatgpt test code ####
# Temporal beta diversity per pixel (current vs future)
# Sørensen dissimilarity split into turnover + nestedness (Baselga 2010)
# Parallel + disk-backed via terra::app() chunk processing
 
library(terra)
 
cur <- list.files(
  path = "N:/SDMs/SDMs_current/Results/Binary_SpeciesCurrentAtualDistri/",
  pattern = "\\.tif$",
  full.names = TRUE
)
cur <- rast(cur)

fut <- list.files(
  path = "N:/SDMs/SDMs_future/Results/BinaryMaps_original/",
  pattern = "\\.tif$",
  full.names = TRUE
)
fut <- rast(fut)

## Speedup the process.##
dir.create("N:/terra_tmp", showWarnings = FALSE)
dir.create("N:/beta_tmp", showWarnings = FALSE)

src_to_species <- function(x) {
  x <- basename(x)
  x <- sub("\\.tif$", "", x, ignore.case = TRUE)
  x <- sub("^binary[_ ]+", "", x, ignore.case = TRUE)
  x <- sub("_CurrentActual$", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+", "_", x)
  x
}


names(cur) <- src_to_species(sources(cur))
names(fut) <- src_to_species(sources(fut))

if (!setequal(names(cur), names(fut))) stop("Species sets differ.")
fut <- fut[[names(cur)]]
stopifnot(identical(names(cur), names(fut)))

# Convert float to byte
cur_file <- "N:/beta_tmp/curActualBinary_140_INT1U.tif"
fut_file <- "N:/beta_tmp/futTestBinary_140_INT1U.tif"

writeRaster(cur, cur_file, overwrite = TRUE,
            wopt = list(datatype = "INT1U"),
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))

writeRaster(fut, fut_file, overwrite = TRUE,
            wopt = list(datatype = "INT1U"),
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))


# Check if species are in the same order in both raster stacks.
names(cur)[[130]]
names(fut)[[130]]
if (!identical(names(cur), names(fut))) {
  stop("Layer names of current and future stacks do not match exactly.")
}

stopifnot(nlyr(cur) == nlyr(fut))
if (!compareGeom(cur, fut, stopOnError = FALSE)) {
  stop("Current and future stacks are not aligned (extent/resolution/CRS). Align them first.")
}

 
# ---- settings ----
cores <- max(1, parallel::detectCores() - 25)
 
# terra-wide options (threads + scratch)
terraOptions(cores = cores, memfrac = 0.8, progress = 10)
# Optional: speed up GeoTIFF writing (also helps on network drives)
gdal_threads <- max(1, cores)
Sys.setenv(GDAL_NUM_THREADS = gdal_threads)  # used by some GDAL ops/writing
 
# ---- checks ----
C <- cur
F_ <- fut
stopifnot(inherits(C, "SpatRaster"), inherits(F_, "SpatRaster"))
stopifnot(nlyr(C) == nlyr(F_))
stopifnot(compareGeom(C, F_, stopOnError = FALSE))
 
# ---- ensure binary presence/absence (0/1) ----
# C <- ifel(is.na(C), NA, as.int(C > 0))
# F <- ifel(is.na(F), NA, as.int(F > 0))
 
S <- nlyr(C)
 
# ---- compute a,b,c in one parallel pass ----
# app() processes in blocks and can use multiple threads (cores=)
X <- c(C, F_) 
# stack 1.current and then 2.future. Order matters for the function below.

abc_fun <- local({
  S_local <- S
  function(v) {
    cur <- v[1:S_local]
    fut <- v[(S_local + 1):(2 * S_local)]

    # If this pixel is NA in all layers (your non-forest background), keep NA
    if (all(is.na(cur)) && all(is.na(fut))) {
      return(c(NA, NA, NA))
    }
    a <- sum(cur == 1 & fut == 1, na.rm = TRUE) 
    # number of species show up in both time period.
    b <- sum(cur == 1 & fut == 0, na.rm = TRUE)
    # b is the number of species that show up in current but not in future, i.e., losses.
    c_ <- sum(cur == 0 & fut == 1, na.rm = TRUE)
    # c is the number of species that show up in future but not in current, i.e., gains.
    c(a, b, c_)
  }
})
 

abc <- app(
  X, abc_fun,
  cores = cores,
  filename = "N:/beta_tmp/abc_shared_loss_gain.tif",
  overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"))
)
names(abc) <- c("a_shared", "b_losses", "c_gains")
 
a <- abc[[1]]
b <- abc[[2]]
c_ <- abc[[3]]
 
# ---- Baselga (2010) partitioning ----
beta_fun <- function(v) {
  v <- as.numeric(v)  # make sure scalar numerics

  a  <- v[1]
  b  <- v[2]
  c_ <- v[3]

  # Rule 1: NA background stays NA
  if (all(is.na(v[1:3]))) return(c(NA, NA, NA))

  # Rule 2: valid but empty (a=b=c=0) -> return NA (undefined)
  if ((a + b + c_) == 0) return(c(NA, NA, NA))

  # Baselga (2010)
  den_sor <- 2*a + b + c_
  beta_sor <- (b + c_) / den_sor

  m <- min(b, c_)
  den_sim <- a + m
  beta_sim <- if (den_sim == 0) 0 else (m / den_sim) # Dissimilarity due to turnover only.

  beta_sne <- beta_sor - beta_sim

  # Handle any remaining non-finite safely
  if (!is.finite(beta_sor)) beta_sor <- 0
  if (!is.finite(beta_sim)) beta_sim <- 0
  if (!is.finite(beta_sne)) beta_sne <- 0

  c(beta_sor, beta_sim, beta_sne)
}

# Compute all three in one pass and write to one 3-band GeoTIFF
beta_all <- app(
  abc, beta_fun,
  cores = cores,
  filename = "beta_baselga_all.tif",
  overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"))
)

names(beta_all) <- c("beta_sor", "beta_sim_turnover", "beta_sne_nestedness")

# Split into separate rasters (cheap, just references bands)
beta_sor <- beta_all[[1]]
beta_sim <- beta_all[[2]]
beta_sne <- beta_all[[3]]

# Write three separate files
wopt <- list(gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"))

writeRaster(beta_sor, "beta_sor.tif", overwrite = TRUE, wopt = wopt)
writeRaster(beta_sim, "beta_turnover_beta_sim.tif", overwrite = TRUE, wopt = wopt)
writeRaster(beta_sne, "beta_nestedness_beta_sne.tif", overwrite = TRUE, wopt = wopt)


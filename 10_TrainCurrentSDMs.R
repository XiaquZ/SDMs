library(terra) # Modern alternative to raster
library(dplyr)
library(MASS)
library(ENMeval)
library(ecospat) # for ecospat.boyce()
library(sf)

####################################################################################
########################              Setup Paths               ######################
####################################################################################

# Define directory paths
base_dir <- "F:/SDMs/SDMs_current"
predict_path <- file.path(base_dir, "Predictors")
occ_path <- file.path(base_dir, "Occurrences_cleaned")
shp_path <- file.path(base_dir, "Shapefiles")
output_dir <- file.path(base_dir, "Results")
save_rasters <- TRUE

set.seed(16)

####################################################################################
########################              Data Loading              ######################
####################################################################################

# Load shapefile
Europe <- vect(shp_path)

# Load environmental variables
pred_vars <- c(
  "Micro_BIO5_EU_CHELSAbased_2000-2020",
  "Micro_BIO6_EU_CHELSAbased_2000-2020",
  "CHELSA_bio12_EU_2000-2019",
  "CHELSA_bio15_EU_2000-2019",
  "cec",
  "clay",
  "Slope",
  "Elevation"
  )
pred_files <- file.path(predict_path, paste0(pred_vars, ".tif"))
covariates_pred <- rast(pred_files)
names(covariates_pred) <- pred_vars
covariates_pred

# Occurrence files
occurrence.files <- list.files(occ_path, pattern = "\\.csv$", full.names = TRUE)
occurrence.names <- list.files(occ_path, pattern = "\\.csv$", full.names = FALSE)
occurrence.tif <- gsub(".csv", ".tif", occurrence.names)


rm(list = ls()[!ls() %in% c(
  "covariates_pred", "occurrence.files", "occurrence.names",
  "occurrence.tif", "Europe",
  "pred_vars", "metrics", "save_rasters", "output_dir"
)])

####################################################################################
########################              SDM Loop                  ######################
####################################################################################
#  #Aggregate to 500m resolution to improve the speed
temp_rast <- covariates_pred[[1]]
temp_rast <- terra::aggregate(temp_rast, fact = 10) # aggregate resolution.

for (r in seq_along(occurrence.files)) {
  cat("Processing:", occurrence.names[r], "\n")

species_df <- read.csv(occurrence.files[r])
occs_coords <- species_df[, c("Longitude", "Latitude")]

 #### Split into internal (80%) and external (20%)
n_occs <- nrow(occs_coords)
test_idx <- sample(seq_len(n_occs), size = ceiling(0.2 * n_occs))
external <- occs_coords[test_idx, ]
internal <- occs_coords[-test_idx, ]

#   #### Method 1: Kernel density (use raster resolution directly without ncol/nrow)
#  #Aggregate to 500m resolution to improve the speed
# temp_rast <- covariates_pred[[1]]
# temp_rast <- terra::aggregate(temp_rast, fact = 10) # aggregate resolution.
# 
# # Create kernel density estimate
# bias <- kde2d(internal[, "Longitude"], internal[, "Latitude"], 
#               n = c(ncol(temp_rast), nrow(temp_rast)),
#               lims = c(
#                 ext(temp_rast)[1], ext(temp_rast)[2], # xmin, xmax
#                 ext(temp_rast)[3], ext(temp_rast)[4]  # ymin, ymax
#               )) # limits of density funtion
# bias.ras <- rast(bias)
# ext(bias.ras) <- ext(temp_rast)
# crs(bias.ras) <- crs(temp_rast)
# bias.ras <- mask(bias.ras, temp_rast)
# 
# # Background points sampled with KDE bias
# bg_n <- nrow(internal)
# bg_xy <- spatSample(bias.ras, bg_n, method = "weights", as.points = TRUE)
# bg_coords <- as.data.frame(crds(bg_xy))
# colnames(bg_coords) <- c("Longitude", "Latitude")

#### Methods 2: Background thickening method for bg data. ####
# convert internal to sf (for buffering etc.)
occ_sf <- st_as_sf(internal, coords = c("Longitude","Latitude"),
                   crs = crs(covariates_pred))
thickening_radius = 50000



# Create buffer around each presence point
presence_buffers <- st_buffer(occ_sf, dist = thickening_radius)

# Convert thickened area to SpatVector for terra
thickened_area_vect <- vect(presence_buffers)
plot(thickened_area_vect)

# Count how many buffer are overlaped together as pixel values.
bias_rast <- rasterize(thickened_area_vect, temp_rast, fun = "sum")

# mask the bias surface so we only sample where environment vars have data
bias_rast  <- mask(bias_rast, temp_rast) #mask the bias surface so we only sample where mask_raster had data

# Sample background points within thickened area
set.seed(16)
n_bg <- 10000 #fix number of 10,000

# sample background points
bg_pts <- spatSample(
  bias_rast,
  size      = n_bg,
  method    = "weights",
  as.points = TRUE
) 
# extract coords back to data.frame
bg_coords <- as.data.frame(crds(bg_pts))
colnames(bg_coords) <- c("Longitude","Latitude")


# Pre-extract predictor values for occurrences and background
occs.z <- cbind(
  internal,
  terra::extract(covariates_pred, internal, ID = FALSE)
) %>% na.omit()

# Bg thickening
bg.z <- cbind(
  bg_coords,
  terra::extract(covariates_pred, bg_coords, ID = FALSE)
) %>% na.omit()

occs.z <- as.data.frame(occs.z)
bg.z <- as.data.frame(bg.z)
names(occs.z) <- make.names(names(occs.z))
names(bg.z) <- make.names(names(bg.z))


# ENMeval modeling (SWD, no raster input)
# We use the blocked method and the feature classes "linear", "quadratic", "product"
# No raster data (a.k.a, samples with data, or SWD): no full model raster predictions created, so will run faster!
# Afterwards predict to raster for the best model only
e.swd <- ENMevaluate(
  occs = occs.z,
  bg = bg.z,
  algorithm = "maxnet",
  partitions = "block",
  partition.settings = list(orientation = "lat_lon"),
  tune.args = list(
    fc = c("L", "Q", "P", "LQ", "QP", "LP", "LQP"),
    rm = c(0.5, 1, 2, 3, 4, 5)
  ),
  parallel = TRUE,
  numCores = 10
)
fn <- file.path(output_dir,
                paste0(gsub(".csv", "_ENMeval_swd.RData",
                                            occurrence.names[[r]])))
save(e.swd, file = fn)
}

#### Produce matrix ####
metrics <- data.frame(Species = character(), CBI = numeric(), Sensitivity = numeric())

# Select best model by delta.AICc
res <- eval.results(e.swd)

best.idx <- which(res$delta.AICc == min(res$delta.AICc))
best.models <- e.swd@models[best.idx]
n_best_models <- length(best.idx)

# Predict best model(s) to raster with low memory use
if (n_best_models == 1) {
  cat("  - Single best model found\n")
  out_file <- file.path(output_dir, paste0("logistic_", occurrence.tif[r]))
  pred_ras <- ENMeval::maxnet.predictRaster(
    mod = best.models[[1]],
    envs = covariates_pred,
    pred.type = "cloglog",
    doClamp = TRUE,
  ) 
  pred_ras <- pred_ras * 100
  pred_ras <- round(pred_ras, digits = 1)
  # Save to disk
  writeRaster(pred_ras, out_file, overwrite = TRUE)
  
} else {
  cat("  - Multiple best models found (", n_best_models, "), averaging predictions\n")
  tmp_rasters <- lapply(best.models, function(mod) {
    ENMeval::maxnet.predictRaster(
      mod = mod,
      envs = covariates_pred,
      pred.type = "cloglog",
      doClamp = TRUE
    )
  })
  ras_stack <- rast(tmp_rasters)
  pred_ras <- app(ras_stack, fun = mean)
}

# Scale and round predictions
pred_ras <- pred_ras * 100
pred_ras <- round(pred_ras, digits = 1)
# Save continuous prediction raster
if (save_rasters) {
  out_file <- paste0(output_dir, "/Average_", length(best.idx), "_mdls_", occurrence.tif[r])
  writeRaster(pred_ras, out_file, overwrite = TRUE)
}
 

### === Threshold (10 percentile training presence) === ###
occs_probs <- terra::extract(pred_ras, internal, ID = FALSE)
or.10p.avg <- quantile(occs_probs, probs = 0.1, na.rm = TRUE)

binary_ras <- pred_ras
binary_ras[binary_ras <= or.10p.avg] <- 0
binary_ras[binary_ras > or.10p.avg] <- 1
plot(binary_ras)

if (save_rasters) {
  bin_file <- file.path(output_dir, paste0("binary_", occurrence.tif[r]))
  writeRaster(binary_ras, bin_file, overwrite = TRUE)
}

### === CBI (using external data) === ###
obs_probs <- na.omit(terra::extract(pred_ras, external, ID = FALSE))
bg_probs <- spatSample(
  pred_ras,
  size = min(1e7, ncell(pred_ras)),
  method = "random",
  na.rm = TRUE,
  exhaustive=TRUE)

CBI <- ecospat.boyce(fit = bg_probs, obs = obs_probs)$cor

### === Sensitivity (using external data) === ###
ext_probs <- na.omit(terra::extract(pred_ras, external))
predicted_classes <- ifelse(ext_probs <= or.10p.avg, 0, 1)
sensitivity <- sum(predicted_classes == 1) / length(predicted_classes)

### === Store metrics === ###
metrics <- rbind(metrics, data.frame(
  Species = occurrence.names[r],
  CBI = CBI,
  Sensitivity = sensitivity,
  Threshold_10pct = or.10p.avg
))

# Clean memory
rm(list = ls()[!ls() %in% c(
  "covariates_pred", "occurrence.files", "occurrence.names",
  "occurrence.tif", "Europe", "metrics",
  "save_rasters", "output_dir", "pred_vars"
)])


# Save metrics summary
write.csv(metrics, file.path(output_dir, "SDMs_metrics_summary.csv"), row.names = FALSE)

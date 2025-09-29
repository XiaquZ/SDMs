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
base_dir <- "D:/SDMs/SDMs_current"
predict_path <- file.path(base_dir, "Predictors")
occ_path <- file.path(base_dir, "Occurrences_cleaned")
shp_path <- file.path(base_dir, "Shapefiles")
output_dir <- file.path(base_dir, "Results/Metrics")
species_final <- file.path(base_dir, "Results/Species_final")
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
  "Elevation",
  "TWI",
  "phh2o_0_30_WeightedMean"
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
########################              SDM Loop                ######################
####################################################################################
#  #Aggregate to 500m resolution to improve the speed
temp_rast <- covariates_pred[[1]]
temp_rast <- terra::aggregate(temp_rast, fact = 10) # aggregate resolution.

for (r in seq_along(occurrence.files)) {
  cat("Processing:", occurrence.names[r], "\n")

species_df <- read.csv(occurrence.files[r])
occs_coords <- species_df[, c("Longitude", "Latitude")]

 #### Split into internal (80%) and external (20%)
# 80% for internal calibration + cross-validation
# 20% for external validation + metric calculations
set.seed(16)
n_occs <- nrow(occs_coords)
test_idx <- sample(seq_len(n_occs), size = ceiling(0.2 * n_occs))
external <- occs_coords[test_idx, ]
internal <- occs_coords[-test_idx, ]

########################
#### Produce matrix ####
########################
metrics <- data.frame(
    Species = character(),
    CBI = numeric(),
    Sensitivity = numeric(),
    Threshold_10pct = numeric())
### === Threshold (10 percentile training presence) === ###
 # Path to prediction raster
  pred_file <- file.path(species_final, occurrence.tif[r])

  # Check if raster file exists
  if (!file.exists(pred_file)) {
    cat(" → Raster not found, skipping:", occurrence.names[r], "\n")
    next  # Skip to next species
  }
pred_ras <- rast(pred_file)
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
write.csv(metrics,
file.path(output_dir, "SDMs_metrics_summary.csv"),
row.names = FALSE)
}

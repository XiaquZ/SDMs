library(terra)  # Modern alternative to raster
library(dplyr)
library(MASS)
library(ENMeval)
library(prodlim)
library(sf)  # For vector data

####################################################################################
########################              Setup Paths               ######################
####################################################################################

# Define directory paths
base_dir <- "I:/SMDs_be"
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
pred_vars <- c("be_bio5", "be_bio6", "be_bio12", "be_bio15", "cec_be", "clay_be")
pred_files <- file.path(predict_path, paste0(pred_vars, ".tif"))
covariates_macro <- rast(pred_files)
names(covariates_macro) <- pred_vars

# Occurrence files
occurrence.files <- list.files(occ_path, pattern = "\\.csv$", full.names = TRUE)
occurrence.names <- list.files(occ_path, pattern = "\\.csv$", full.names = FALSE)
occurrence.tif <- gsub(".csv", ".tif", occurrence.names)

# Template raster indices
sel.pr <- complete.cases(values(covariates_macro))
indices <- which(sel.pr)

rm(list=ls()[! ls() %in% c("covariates_macro", "occurrence.files", "occurrence.names", 
                           "occurrence.tif", "Europe", "indices", 
                           "pred_vars", "metrics", "save_rasters", "output_dir")])

####################################################################################
########################              SDM Loop                  ######################
####################################################################################

metrics <- data.frame(Species = character(), CBI = numeric(), Sensitivity = numeric())

for(r in seq_along(occurrence.files)) {
  cat("Processing:", occurrence.names[r], "\n")

  species_df <- read.csv(occurrence.files[r])
  train_idx <- sample(seq_len(nrow(species_df)), size = 0.8 * nrow(species_df))
  internal <- species_df[train_idx, ]
  external <- species_df[-train_idx, ]

  # Kernel density
  bias <- kde2d(internal$Longitude, internal$Latitude,
                n = c(ncol(covariates_macro), nrow(covariates_macro)),
                lims = c(range(xFromCell(covariates_macro, 1:ncell(covariates_macro))),
                         range(yFromCell(covariates_macro, 1:ncell(covariates_macro)))))
  bias_ras <- rast(bias$z, extent = ext(covariates_macro))
  crs(bias_ras) <- crs(covariates_macro)
  bias_ras <- mask(bias_ras, vect(Europe))
  bias_ras[is.na(bias_ras)] <- 0

  # Occurrences and background
  occs.z <- cbind(internal[, 2:3], terra::extract(covariates_macro, internal[, 2:3])) %>% na.omit()

  bg_ratio <- 3
  bg_n <- bg_ratio * nrow(occs.z)
  bg_xy <- spatSample(bias_ras, bg_n, method = "weights", as.points = TRUE)
  bg_df <- terra::extract(covariates_macro, bg_xy) %>% cbind(st_coordinates(bg_xy)) %>% na.omit()
  colnames(bg_df) <- c(pred_vars, "Longitude", "Latitude")
  bg_df <- bg_df[sample(nrow(bg_df), nrow(occs.z)), c("Longitude", "Latitude", pred_vars)]

  # ENMeval modeling
  e.mx <- ENMevaluate(occs = occs.z, bg = bg_df, algorithm = "maxnet", partitions = "block",
                      tune.args = list(fc = c("L","Q","P","LQ","QP","LP","LQP"),
                                       rm = c(0.5, 1, 2, 3, 4, 5)),
                      parallel = TRUE, numCores = 10)

  res <- eval.results(e.mx)
  opt.seq <- filter(res, delta.AICc == min(delta.AICc))
  best.mdl <- row.match(opt.seq, res)

  # Predicting
  logistic_rasters <- lapply(best.mdl, function(i) {
    predict(e.mx@models[[i]], envs = covariates_macro, type = "logistic") * 100
  })
  pred_ras <- Reduce(`+`, logistic_rasters) / length(logistic_rasters)
  pred_ras <- round(pred_ras)

  # Threshold
  occs.s <- cbind(internal[, 2:3], terra::extract(pred_ras, internal[, 2:3]))
  colnames(occs.s) <- c("Longitude", "Latitude", "Probability")
  or.10p.avg <- quantile(occs.s$Probability, probs = 0.1, na.rm = TRUE)

  binary_ras <- pred_ras
  binary_ras[binary_ras <= or.10p.avg] <- 0
  binary_ras[binary_ras > or.10p.avg] <- 1

  if (save_rasters) {
    writeRaster(pred_ras, file.path(output_dir, paste0("logistic_", occurrence.tif[r])), overwrite = TRUE)
    writeRaster(binary_ras, file.path(output_dir, paste0("binary_", occurrence.tif[r])), overwrite = TRUE)
  }

  # CBI
  obs <- na.omit(terra::extract(pred_ras, external[, c("Longitude", "Latitude")]))
  logistic.vct <- values(pred_ras)[sample(seq_len(ncell(pred_ras)), 1e7)]
  CBI <- ecospat.boyce(fit = logistic.vct, obs = obs)$cor

  # Sensitivity
  sensitivity_df <- cbind(external[, 2:3], terra::extract(pred_ras, external[, 2:3])) %>% na.omit()
  colnames(sensitivity_df) <- c("Longitude", "Latitude", "Predicted")
  sensitivity_df$Observed <- 1
  sensitivity_df$Predicted <- ifelse(sensitivity_df$Predicted <= or.10p.avg, 0, 1)
  conf_matrix <- table(sensitivity_df$Observed, sensitivity_df$Predicted)
  sensitivity <- conf_matrix[2] / (sum(conf_matrix) + 1e-6)

  metrics <- rbind(metrics, data.frame(Species = occurrence.names[r], CBI = CBI, Sensitivity = sensitivity))

  rm(list=ls()[! ls() %in% c("covariates_macro", "occurrence.files", "occurrence.names", 
                             "occurrence.tif", "Europe", "indices", 
                             "pred_vars", "metrics", "save_rasters", "output_dir")])
}

# Optionally save metrics
# write.csv(metrics, file.path(output_dir, "SDM_metrics_summary.csv"), row.names = FALSE)

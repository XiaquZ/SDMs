library(terra)
library(flexsdm)
# pred_ras already defined
# internal: training presences (SpatVector or coords)
# external: independent presences for evaluation

## 1) Extract suitability values
p_train <- na.omit(terra::extract(pred_ras, internal, ID = FALSE)[,1])   # presences (train)
p_test  <- na.omit(terra::extract(pred_ras, external, ID = FALSE)[,1])   # presences (test)

## If you don't have true absences, use background as 'a' (presence–background evaluation)
## Sample background points and get their suitability
bg_pts  <- spatSample(pred_ras,
                      size = min(1e5, ncell(pred_ras)),
                      method = "random",
                      as.points = TRUE,
                      na.rm = TRUE,
                      exhaustive = TRUE)
bg_vals <- terra::extract(pred_ras, bg_pts, ID = FALSE)[,1]

## 2) TRAINING-EVALUATION to obtain the p10-like threshold and metrics
##    - Use the "sensitivity" threshold with sens = 0.9 (i.e., 10% omission)
##    - Set bg = bg_vals so BOYCE is computed presence vs background (as recommended)
eval_train <- sdm_eval(
  p   = p_train,
  a   = bg_vals,                     # use background as pseudo-absences
  bg  = bg_vals,                     # used only for BOYCE
  thr = c("sensitivity", sens = "0.9")
)

## Grab the threshold value (this is your p10-equivalent threshold)
thr_row   <- subset(eval_train, threshold == "sensitivity")
thr_p10   <- thr_row$thr_value[1]

## Continuous Boyce Index (CBI) from the same evaluation
CBI <- thr_row$BOYCE[1]

## 3) Binarize the raster at that threshold and (optionally) save
binary_ras <- classify(
  pred_ras,
  rcl = matrix(c(-Inf, thr_p10, 0,
                 thr_p10, Inf, 1),
               ncol = 3, byrow = TRUE)
)
plot(binary_ras)

if (save_rasters) {
  bin_file <- file.path(output_dir, paste0("binary_", occurrence.tif[r]))
  writeRaster(binary_ras, bin_file, overwrite = TRUE)
}

## 4) Sensitivity on EXTERNAL data at the TRAINING threshold
##    External set contains only presences, so sensitivity is just the proportion predicted 1
predicted_ext <- ifelse(p_test > thr_p10, 1, 0)
sensitivity_external <- mean(predicted_ext == 1)

## (Optional) If you ALSO have external absences/pseudo-absences:
## a_test <- ... # vector of suitability at absences/pseudo-absences
## Then you could build a confusion matrix at thr_p10, or call sdm_eval on the external split:
## BUT NOTE: sdm_eval() will recompute the threshold on the data you pass.
## eval_test <- sdm_eval(p = p_test, a = a_test, bg = bg_vals, thr = c("sensitivity", sens = "0.9"))
## That’s fine if you *want* the threshold re-estimated on the test set; otherwise use thr_p10 above.

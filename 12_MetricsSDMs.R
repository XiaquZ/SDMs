library(terra) # Modern alternative to raster
library(dplyr)
library(MASS)
library(ENMeval)
library(ecospat) # for ecospat.boyce()
library(sf)
library(tools) 

####################################################################################
########################              Setup Paths               ######################
####################################################################################

# Define directory paths
base_dir <- "E:/SDMs/SDMs_current/"
occ_path <- file.path(base_dir, "Occurrences_cleaned")
output_dir <- file.path(base_dir, "Results/Metrics")
species_final <- file.path(base_dir, "Results/Species_final")
save_rasters <- TRUE

set.seed(16)

####################################################################################
########################              Data Loading              ######################
####################################################################################

# Occurrence files
occurrence.files <- list.files(occ_path, pattern = "\\.csv$", full.names = TRUE)
occurrence.names <- list.files(occ_path, pattern = "\\.csv$", full.names = FALSE)
occurrence.tif <- gsub(".csv", ".tif", occurrence.names)
species_names <- file_path_sans_ext(basename(occurrence.tif))

rm(list = ls()[!ls() %in% c(
   "occurrence.files", "occurrence.names",
  "occurrence.tif", 
  "metrics", "save_rasters", "output_dir","species_final",
  "species_names"
)])

####################################################################################
########################              SDM Loop                ######################
####################################################################################
escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\])", "\\\\\\1", x)

find_species_raster_unique <- function(sp, root_dir, prefer = c("_current", "_present", "")) {
  all_rasters <- list.files(
    root_dir, pattern = "\\.(tif|tiff)$",
    recursive = TRUE, full.names = TRUE, ignore.case = TRUE
  )
  if (!length(all_rasters)) stop("No rasters found under: ", root_dir)
  
  # Match: filename MUST start with species name, then end OR a separator OR extension
  pat <- paste0("^", escape_regex(sp), "(?:\\.|[_ -]|$)")
  cand <- all_rasters[grepl(pat, basename(all_rasters), ignore.case = TRUE, perl = TRUE)]
  
  if (!length(cand)) stop("No file matching species name: '", sp, "'.")
  
  # If more than one, try preference keywords (e.g., choose *_current* first)
  if (length(cand) > 1) {
    for (p in prefer) {
      pick <- cand[grepl(p, basename(cand), ignore.case = TRUE)]
      if (length(pick) == 1) return(pick)
      if (length(pick) > 1) cand <- pick  # narrow and continue
    }
  }
  
  if (length(cand) == 1) return(cand)
  
  stop(
    "Multiple files start with species name '", sp, "':\n  - ",
    paste(cand, collapse = "\n  - "),
    "\nMake them unique or adjust `prefer=`."
  )
}

# Create empty data.frame for storing metrics output.
# 1) init once
# metrics <- data.frame(
#   Species = character(),
#   CBI = numeric(),
#   Sensitivity = numeric(),
#   Threshold_10pct = numeric(),
#   stringsAsFactors = FALSE
# )

## If already had metric files in the folder:
metrics <- read.csv(paste0(output_dir, "/SDMs_metrics_summary.csv"))

get_species_tif <- function(sp, dir) {
  sp_clean <- sub("\\.csv$", "", sp)  # drop .csv if present
  all_tifs <- list.files(dir, pattern = "\\.(tif|tiff)$", full.names = TRUE, ignore.case = TRUE)
  base     <- basename(all_tifs)
  
  # allow space or underscore between words
  core <- gsub("\\s+", "[ _]+", sp_clean)
  
  # key change: allow boundary OR underscore/space/digit after species
  patt <- paste0("(?i)\\b", core, "(?=\\b|_|\\s|\\d|\\.)")  # no need to reassert \\.tif$ since we list.files on tif*
  hits <- grep(patt, base, perl = TRUE)
  
  if (!length(hits)) return(NA_character_)
  all_tifs[hits[1]]
}

for (r in seq_along(occurrence.files)) {
  cat("Processing:", species_names[r], "\n")

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

### === Threshold (10 percentile training presence) === ###
 # Path to prediction raster
# ✅ pass the species name (includes ".csv"; helper will strip it)
pred_file <- get_species_tif(species_names[r], species_final)
cat("  matched tif:", pred_file, "\n")  # quick sanity print

if (is.na(pred_file) || !file.exists(pred_file)) {
  cat(" → Raster not found, skipping:", occurrence.names[r], "\n")
  next
}

pred_ras <- rast(pred_file)
occs_probs <- terra::extract(pred_ras, internal, ID = FALSE)
or.10p.avg <- as.numeric(quantile(occs_probs, probs = 0.1, na.rm = TRUE))

 # Produce binary maps
binary_ras <- pred_ras
binary_ras[binary_ras <= or.10p.avg] <- 0
binary_ras[binary_ras > or.10p.avg] <- 1
names(binary_ras) <- paste0("Binary_", species_names[r])

if (save_rasters) {
  bin_file <- file.path(output_dir, paste0("binary_", species_names[r], ".tif"))
  writeRaster(
    binary_ras,
    bin_file,
    datatype  = "INT1U",
    gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "PREDICTOR=2"),
    overwrite = TRUE)
}

### === CBI (using external data) === ###
obs_probs <- na.omit(terra::extract(pred_ras, external, ID = FALSE))
bg_probs <- spatSample(
  pred_ras,
  size = min(1e5, ncell(pred_ras)),
  method = "random",
  na.rm = TRUE,
  exhaustive=TRUE)

CBI <- ecospat.boyce(fit = bg_probs, obs = obs_probs)$cor

### === Sensitivity (using external data) === ###
ext_probs <- na.omit(terra::extract(pred_ras, external))
predicted_classes <- ifelse(ext_probs <= or.10p.avg, 0, 1)
sensitivity <- sum(predicted_classes == 1) / length(predicted_classes)

# #### Use flexsdms for TSS. ####
# ## 1) Extract suitability values
# p_train <- na.omit(terra::extract(pred_ras, internal, ID = FALSE)[,1])   # presences (train)
# p_test  <- na.omit(terra::extract(pred_ras, external, ID = FALSE)[,1])   # presences (test)
# 
# ## If you don't have true absences, use background as 'a' (presence–background evaluation)
# ## Sample background points and get their suitability
# bg_pts  <- spatSample(pred_ras,
#                       size = min(1e5, ncell(pred_ras)),
#                       method = "random",
#                       as.points = TRUE,
#                       na.rm = TRUE,
#                       exhaustive = TRUE)
# bg_vals <- terra::extract(pred_ras, bg_pts, ID = FALSE)[,1]
# 
# ## 2) TRAINING-EVALUATION to obtain the p10-like threshold and metrics
# ##    - Use the "sensitivity" threshold with sens = 0.9 (i.e., 10% omission)
# ##    - Set bg = bg_vals so BOYCE is computed presence vs background (as recommended)
# eval_train <- sdm_eval(
#   p   = p_train,
#   a   = bg_vals,                     # use background as pseudo-absences
#   bg  = bg_vals,                     # used only for BOYCE
#   thr = c("sensitivity", sens = "0.9")
# )

### === Store metrics === ###
metrics <- rbind(metrics, data.frame(
  Species = species_names[r],
  CBI = CBI,
  Sensitivity = sensitivity,
  Threshold_10pct = or.10p.avg
))

# Clean memory
rm(list = ls()[!ls() %in% c(
  "covariates_pred", "occurrence.files", "occurrence.names",
  "occurrence.tif", "Europe",
  "pred_vars", "metrics", "save_rasters", "output_dir","species_final",
  "species_names","get_species_tif"
)])

}
# Save metrics summary
write.csv(metrics,
          file = file.path(output_dir, "SDMs_Metrics_Summary.csv"),
          row.names = FALSE)
r_test <- rast("D:/SDMs/SDMs_current/Results/Species_final/Brachypodium sylvaticum.tif")

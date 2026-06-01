library(terra)
library(tibble)
mf_av_all <- rast("N:/SDMs/Multifunctionality_140species/MF_average_4MIs.tif")
mf_single08_all <- rast("N:/SDMs/Multifunctionality_140species/MF_threshold_count_08_4MIs.tif")
mf_av_rl <- rast("N:/SDMs/Multifunctionality_RedList/MF_average_4MIs_RedList.tif")
mf_single08_rl <- rast("N:/SDMs/Multifunctionality_RedList/MF_threshold_count_08_4MIs_RedList.tif")

# Extract sample data.
set.seed(123)
mf_samples <- spatSample(mf_av_all,
    10000,
    na.rm = TRUE,
    xy = TRUE,
    as.df = TRUE,
    exhaustive = TRUE
)
mf_samples <- mf_samples[complete.cases(mf_samples), ]
colnames(mf_samples)[3] <- "MF_av_AllSpecies"

# Save file.
saveRDS(mf_samples, "I:/DATA/output/MF_SDMs/10000SamplesMF_v1.rds")

# Load file:
mf_samples <- readRDS("I:/DATA/output/MF_SDMs/10000SamplesMF_v1.rds")

# Extract the singleT MF by using the xy.
xy <- as.matrix(mf_samples[, c("x", "y")])

### Start extracting values.####
ext_single08 <- terra::extract(mf_single08_all, xy)

# Check results
head(ext_single08)
str(ext_single08)

# Add extracted values back to mf_samples.
mf_samples$MF_single08_AllSpecies <- ext_single08$sum
head(mf_samples)
saveRDS(mf_samples, "I:/DATA/output/MF_SDMs/10000SamplesMF_v1.rds")

## Extract another MF value of Red List.
ext_av_RL <- terra::extract(mf_av_rl, xy)

# Rename the first extracted value column
names(ext_av_RL)[1] <- "MF_av_RL"

# Check results
head(ext_av_RL)
str(ext_av_RL)

# Add extracted values back to mf_samples
mf_samples$MF_av_RL <- ext_av_RL[["MF_av_RL"]]
head(mf_samples)
saveRDS(mf_samples, "I:/DATA/output/MF_SDMs/10000SamplesMF_v1.rds")

## Extract the MF single-threshold RL.
ext_single08_RL <- terra::extract(mf_single08_rl, xy)

# Rename the first extracted value column
names(ext_single08_RL)[1] <- "MF_single08_RL"

# Check results
head(ext_single08_RL)
str(ext_single08_RL)

# Add extracted values back to mf_samples
mf_samples$MF_single08_RL <- ext_single08_RL[["MF_single08_RL"]]
head(mf_samples)
saveRDS(mf_samples, "I:/DATA/output/MF_SDMs/10000SamplesMF_AllSpeciesRL.rds")

colSums(is.na(mf_samples[, c("MF_av_RL", "MF_single08_RL")]))
## 494 rows out of 10000 rows do not have RL values.

####################################
#### Extract predictors' values.####
####################################
# Load data
mf_samples <- readRDS("I:/DATA/output/MF_SDMs/10000SamplesMF_AllSpeciesRL.rds")

coast <- rast("N:/Input/Predictors_microclimate_Stef/coast.tif")
elevation <- rast("N:/Input/Predictors_microclimate_Stef/elevation.tif")
rela_eleva <- rast("N:/Input/Predictors_microclimate_Stef/relative_elevation.tif")
slope <- rast("N:/Input/Predictors_microclimate_Stef/slope.tif")
twi <- rast("N:/Input/Predictors_microclimate_Stef/TWI.tif")
eastness <- rast("N:/Input/Predictors_microclimate_Stef/eastness.tif")
northness <- rast("N:/Input/Predictors_microclimate_Stef/northness.tif")
type <- rast("N:/Input/Predictors_microclimate_Stef/type.tif")
cover <- rast("N:/Input/Predictors_microclimate_Stef/cover.tif")

# Stack predictors
pred_stack <- c(
  coast,
  elevation,
  rela_eleva,
  slope,
  twi,
  eastness,
  northness,
  type,
  cover
)

# Rename predictor layers
names(pred_stack) <- c(
  "coast",
  "elevation",
  "relative_elevation",
  "slope",
  "twi",
  "eastness",
  "northness",
  "type",
  "cover"
)

# Get coordinates
head(mf_samples)
xy <- as.matrix(mf_samples[, c("x", "y")])

# Extract predictor values
pred_values <- terra::extract(pred_stack, xy)

# Check output
head(pred_values)
str(pred_values)
names(pred_values)

# Add extracted predictors to mf_samples
mf_samples <- cbind(mf_samples, pred_values)

# Check final data
head(mf_samples)
str(mf_samples)

# Check missing values in predictors
colSums(is.na(mf_samples[, names(pred_values)]))
# Save the final dataset
# Check duplicated column names
names(mf_samples)[duplicated(names(mf_samples))]
mf_samples <- mf_samples[, !duplicated(names(mf_samples))]
str(mf_samples)
names(mf_samples)[duplicated(names(mf_samples))]

saveRDS(
  mf_samples,
  file = "I:/DATA/output/MF_SDMs/10000samples_orig_MF_9predicts.rds"
)

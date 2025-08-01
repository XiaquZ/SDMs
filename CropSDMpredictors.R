#### Crop bioclim to run SDMs on smaller scale. #####
#### Testing the scripts.####
library(terra)
# Load belgium shapefile. #
region <- vect("I:/SDMs_France/Shapefiles/France.shp")
plot(region)

# Bioclim data
micro_bio5 <- rast(
  "D:/PhD/Data/SDMs_current/Predictors/Micro_BIO5_EU_CHELSAbased_2000-2020.tif"
  )
micro_bio6 <- rast(
  "D:/PhD/Data/SDMs_current/Predictors/Micro_BIO6_EU_CHELSAbased_2000-2020.tif"
)
chelsa_bio12 <- rast(
  "D:/PhD/Data/SDMs_current/Predictors/CHELSA_bio12_EU_2000-2019.tif"
)
chelsa_bio15 <- rast(
  "D:/PhD/Data/SDMs_current/Predictors/CHELSA_bio15_EU_2000-2019.tif"
)
cec <- rast("D:/PhD/Data/SDMs_current/Predictors/cec.tif")
clay <- rast("D:/PhD/Data/SDMs_current/Predictors/clay.tif")

slope <- rast("F:/SDMs/SDMs_current/Predictors/Slope.tif")
elevation <- rast("F:/SDMs/SDMs_current/Predictors/Elevation.tif")

plot(cec)
plot(clay)

# Crop all variables for belgium
region_bio5 <- crop(micro_bio5, region)
region_bio5 <- mask(region_bio5, region)
writeRaster(
  region_bio5,
  "I:/SDMs_France/Predictors/fr_bio5.tif",
  overwrite = TRUE
)

region_bio6 <- crop(micro_bio6, region)
region_bio6 <- mask(region_bio6, region)
writeRaster(
  region_bio6,
  "I:/SDMs_France/Predictors/fr_bio6.tif",
  overwrite = TRUE
)

region_bio12 <- crop(chelsa_bio12, region)
region_bio12 <- mask(region_bio12, region)
writeRaster(
  region_bio12,
  "I:/SDMs_France/Predictors/fr_bio12.tif",
  overwrite = TRUE
)

region_bio15 <- crop(chelsa_bio15, region)
region_bio15 <- mask(region_bio15, region)
writeRaster(
  region_bio15,
  "I:/SDMs_France/Predictors/fr_bio15.tif",
  overwrite = TRUE
)

cec_region <- crop(cec, region)
cec_region <- mask(cec_region, region)
writeRaster(
  cec_region,
  "I:/SDMs_France/Predictors/cec_fr.tif",
  overwrite = TRUE
)

clay_region <- crop(clay, region)
clay_region <- mask(clay_region, region)
plot(clay_region)
writeRaster(
  clay_region,
  "I:/SDMs_France/Predictors/clay_fr.tif",
  overwrite = TRUE
)

# Crop the topography predictors to EU forests.
# First test the correlation between predictors.
elevation <- rast("G:/Input/Predictors_microclimate/elevation.tif")
relat_elev <- rast("G:/Input/Predictors_microclimate/relative_elevation.tif")
slope <- rast("G:/Input/Predictors_microclimate/slope.tif")
twi <- rast("G:/Input/Predictors_microclimate/TWI.tif")
cec <- rast("G:/SDMs/SDMs_current/Predictors/cec.tif")
clay <- rast("G:/SDMs/SDMs_current/Predictors/clay.tif")
micro_bio5 <- rast("G:/SDMs/SDMs_current/Predictors/Micro_BIO5_EU_CHELSAbased_2000-2020.tif")
micro_bio6 <- rast("G:/SDMs/SDMs_current/Predictors/Micro_BIO6_EU_CHELSAbased_2000-2020.tif")
chelsa_bio12 <- rast("G:/SDMs/SDMs_current/Predictors/CHELSA_bio12_EU_2000-2019.tif")
chelsa_bio15 <- rast("G:/SDMs/SDMs_current/Predictors/CHELSA_bio15_EU_2000-2019.tif")

# 2. bundle them in a named list
r_list <- list(
  elevation    = elevation,
  relat_elev   = relat_elev,
  slope        = slope,
  twi          = twi,
  cec          = cec,
  clay         = clay,
  micro_bio5   = micro_bio5,
  micro_bio6   = micro_bio6,
  chelsa_bio12 = chelsa_bio12,
  chelsa_bio15 = chelsa_bio15
)

# set seed for reproducibility
set.seed(42)

# 3. sample coordinates from your “base” raster (here: elevation)
# sample 50k points, returning x/y coords and no NA values
samp <- spatSample(
  micro_bio5, 
  size    = 5000, 
  method  = "random", 
  as.df    = TRUE, 
  xy      = TRUE,     # <-- include coordinates
  na.rm   = TRUE,
  exhaustive = TRUE
)

# 4. extract values from *every* raster in your list at those x/y
#    extract() returns a data.frame with ID + one column per layer,
#    so we pull out the second column to get the values vector.
vals_df <- lapply(r_list, function(r){
  ex <- extract(r, samp[, c("x","y")])
  ex[[2]]
})
vals_df <- as.data.frame(vals_df)

# 5. combine coords + all predictor values
sample_df <- cbind(samp[, c("x","y")], vals_df)
# 6. Compute Pearson correlation on the predictor columns
corr_mat_spear <- cor(
  sample_df[, names(r_list)],    # your predictor columns
  use    = "pairwise.complete.obs",
  method = "spearman"
)

library(corrplot)

# assume corr_mat exists as in your previous steps
# Draw a hierarchical‐clustered, upper‐triangle plot with coefficients
corrplot(
  corr_mat_spear,
  method      = "color",
  type        = "upper",
  order       = "hclust",
  addCoef.col = "black",
  tl.col      = "black",
  tl.srt      = 45,
  diag        = FALSE,
  title       = "Spearman Correlation Matrix"
)

# Based on the correlation coefficients (exclude >= 0.7)
# we only include elevation, slope.
elevation <- rast("G:/Input/Predictors_microclimate/elevation.tif")
slope <- rast("G:/Input/Predictors_microclimate/slope.tif")
micro_bio5 <- rast("G:/SDMs/SDMs_current/Predictors/Micro_BIO5_EU_CHELSAbased_2000-2020.tif")

# mask out the non-forests area.
slope_forest <- mask(slope, micro_bio5)
eleva_forest <- mask(elevation, micro_bio5)

writeRaster(slope_forest, "G:/SDMs/SDMs_current/Predictors/slope_euForests.tif", overwrite = TRUE)
writeRaster(eleva_forest, "G:/SDMs/SDMs_current/Predictors/eleva_euForests.tif", overwrite = TRUE)

# Crop the slope and elevation of France.
fr_slope <- crop(slope, region)
fr_mask <- mask(fr_slope, region)
writeRaster(fr_mask, "I:/SDMs_France/Predictors/Slope.tif", overwrite = TRUE)

fr_eleva <- crop(elevation, region)
fr_eleva <- mask(fr_eleva, region)
writeRaster(fr_eleva, "I:/SDMs_France/Predictors/Elevation.tif", overwrite = TRUE)

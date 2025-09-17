library(terra)

offset_bio5_current <- rast("D:/Output/SDM_bioclim/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO5_2000-2020_recalculated.tif")
chelsa_macro_bio5_current <- rast("I:/DATA/CHELSA/BIO5_CHELSA_2000_2019_epsg3035_25m_1digit.tif")
cover <- rast("D:/Input/Predictors_microclimate/cover.tif")

# Create sample data
s <- c(offset_bio5_current, chelsa_macro_bio5_current)
set.seed(123)
SampleOffsetbio5 <- spatSample(s, 10^5, "regular", na.rm = TRUE, xy = TRUE)
head(SampleOffsetbio5)
colnames(SampleOffsetbio5)[3] <- "offsetBIO5_current"
colnames(SampleOffsetbio5)[4] <- "ChelsaMacroBIO5"

# Extract canopy cover
# 2) Build a coords matrix/data.frame in the same order as SampleOffsetbio5
coords <- SampleOffsetbio5[, c("x", "y")]

# 3) Extract and append (extract returns a data.frame with ID + value)
cov_vals <- terra::extract(cover, coords)
# If 'cover' is a single-layer raster, its value is the second column:
SampleOffsetbio5$canopy_cover <- cov_vals[, 2]
head(SampleOffsetbio5)

# offsetBIO5 vs canopy
plot(SampleOffsetbio5$offsetBIO5_current,
     SampleOffsetbio5$canopy_cover,
     xlab = "Offset BIO5 Current",
     ylab = "Canopy Cover",
     pch = 16, col = rgb(0,0,1,0.3))

# ChelsaMacroBIO5 vs canopy
plot(SampleOffsetbio5$ChelsaMacroBIO5,
     SampleOffsetbio5$canopy_cover,
     xlab = "CHELSA Macro BIO5",
     ylab = "Canopy Cover",
     pch = 16, col = rgb(1,0,0,0.3))

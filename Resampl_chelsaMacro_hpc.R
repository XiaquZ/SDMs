library(terra)

# Load template for resampling
template <- rast("I:/DATA/mean_annualOffset.tif")
macro_bio5 <- rast("")
macro_bio6 <- rast("")

# Resample data to 25 meter
x <- rast(template)
bio5_resample <- resample(macro_bio5, x, method = "bilinear")
bio6_resample <- round(bio5_resample, digits = 1)
print(bio5_resample)

bio6_resample <- resample(macro_bio5, x, method = "bilinear")
bio6_resample <- round(bio6_resample, digits = 1)
print(bio6_resample)

writeRaster(bio5_resample,
    filename = "E:/Input/CHELSAdata/BIO5BIO6/macroBIO5_resampled_25m.tif"
)

writeRaster(bio6_resample,
    filename = "E:/Input/CHELSAdata/BIO5BIO6/macroBIO5_resampled_25m.tif"
)
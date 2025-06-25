library(terra)

# Load template for resampling
template <- rast("H:/Input/ForestTemp_new/01_Offsets/mean_annualOffset.tif")
macro_bio5 <- rast("H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio5_EU_2000-2019.tif")
macro_bio6 <- rast("H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio6_EU_2000-2019.tif")
macro_bio12 <- rast("H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio12_EU_2000-2019.tif")
macro_bio15 <- rast("H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio15_EU_2000-2019.tif")
forest <- rast("H:/Output/forestBIO1_convertedTO0_25m.tif")
print(template)
print(macro_bio5)

# Resample data to 25 meter
x <- rast(template)

# Bio5
bio5_reproj <- project(macro_bio5, crs(x))
bio5_resample <- resample(bio5_reproj, x, method = "bilinear")
bio5_mask <- mask(bio5_resample, forest)
bio5_round <- round(bio5_mask, digits = 1)
print(bio5_round)
writeRaster(bio5_round,
    filename = "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio5_EU_25m_2000-2019.tif",
    overwrite = TRUE
)

# Bio6
bio6_reproj <- project(macro_bio6, crs(x))
plot(bio6_reproj)
bio6_resample <- resample(bio6_reproj, x, method = "bilinear")
plot(bio6_resample)
bio6_mask <- mask(bio6_resample, forest)
bio6_round <- round(bio6_mask, digits = 1)
print(bio6_round)
writeRaster(bio6_round,
            filename = "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio6_EU_25m_2000-2019.tif",
            overwrite = TRUE
)

# bio12
bio12_reproj <- project(macro_bio12, crs(x))
plot(bio12_reproj)
bio12_resample <- resample(bio12_reproj, x, method = "bilinear")
plot(bio12_resample)
bio12_mask <- mask(bio12_resample, forest)
bio12_round <- round(bio12_mask, digits = 1)
print(bio12_round)
writeRaster(bio12_round,
            filename = "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio12_EU_25m_2000-2019.tif",
            overwrite = TRUE
)

# bio15
bio15_reproj <- project(macro_bio15, crs(x))
plot(bio15_reproj)
bio15_resample <- resample(bio15_reproj, x, method = "bilinear")
plot(bio15_resample)
bio15_mask <- mask(bio15_resample, forest)
bio15_round <- round(bio15_mask, digits = 1)
print(bio15_round)
writeRaster(bio15_round,
            filename = "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio15_EU_25m_2000-2019.tif",
            overwrite = TRUE
)

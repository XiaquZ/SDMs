library(terra)

# Load template for resampling
template <- rast("/lustre1/scratch/348/vsc34871/input/mean_annualOffset.tif")
terra_bio5 <- rast("/lustre1/scratch/348/vsc34871/input/terra_BIO5_forResamp.tif")
terra_bio6 <- rast("/lustre1/scratch/348/vsc34871/input/terra_BIO6_forResamp.tif")
print(terra_bio5)
print(terra_bio6)

# Resample data to 25 meter
x <- rast(template)

bio5_resample <- resample(terra_bio5, x, method = "bilinear")
bio5_resample <- round(bio5_resample, digits = 1)
print(bio5_resample)
writeRaster(bio5_resample,
    filename = "/lustre1/scratch/348/vsc34871/output/2000-2020TerraBIO5_resampled_25m.tif",
    overwrite = TRUE
)

bio6_resample <- resample(terra_bio6, x, method = "bilinear")
bio6_resample <- round(bio6_resample, digits = 1)
print(bio6_resample)
writeRaster(bio6_resample,
    filename = "/lustre1/scratch/348/vsc34871/output/2000-2020TerraBIO6_resampled_25m.tif",
    overwrite = TRUE
)


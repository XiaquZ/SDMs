library(terra)

# Load template for resampling
template <- rast("/lustre1/scratch/348/vsc34871/input/mean_annualOffset.tif")
macro_bio12 <- rast("/lustre1/scratch/348/vsc34871/input/MacroEUbio12_2071-2100_ssp370_705m.tif")
macro_bio15 <- rast("/lustre1/scratch/348/vsc34871/input/MacroEUbio15_2071-2100_ssp370_705m.tif")
print(macro_bio12)
print(macro_bio15)

# Resample data to 25 meter
x <- rast(template)

bio12_resample <- resample(macro_bio12, x, method = "bilinear")
bio12_resample <- round(bio12_resample, digits = 1)
print(bio12_resample)
writeRaster(bio12_resample,
    filename = "/lustre1/scratch/348/vsc34871/output/2071-2100macroBIO12_resampled_25m.tif",
    overwrite = TRUE
)

bio15_resample <- resample(macro_bio15, x, method = "bilinear")
bio15_resample <- round(bio15_resample, digits = 1)
print(bio15_resample)
writeRaster(bio15_resample,
    filename = "/lustre1/scratch/348/vsc34871/output/2071-2100macroBIO15_resampled_25m.tif",
    overwrite = TRUE
)
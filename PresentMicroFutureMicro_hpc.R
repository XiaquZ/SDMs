library(terra)

# Load ForestClim BIO5 BIO6 2000-2020.
bio5_pre <- rast("/lustre1/scratch/348/vsc34871/input/ForestClim_05.tif")
bio6_pre <- rast("/lustre1/scratch/348/vsc34871/input/ForestClim_06.tif")
print(bio5_pre)
print(bio6_pre)

# Load future microclimate 2071-2100
bio5_fut <- rast("/lustre1/scratch/348/vsc34871/input/Predictors/PredictedMicroclimate_BIO5_2071-2100_ssp370.tif")
bio6_fut <- rast("/lustre1/scratch/348/vsc34871/input/Predictors/PredictedMicroclimate_BIO6_2071-2100_ssp370.tif")
print(bio5_fut)
print(bio6_fut)

# Calculate the differece in microclimate
bio5_diff <- bio5_fut - bio5_pre
bio5_diff
bio5_diff <- round(bio5_diff, digits = 1)
writeRaster(bio5_diff,
    filename = "/lustre1/scratch/348/vsc34871/output/microBIO5_fut-pre.tif",
    overwrite = TRUE
)

bio6_diff <- bio6_fut - bio6_pre
bio6_diff
bio6_diff <- round(bio6_diff, digits = 1)
writeRaster(bio6_diff,
    filename = "/lustre1/scratch/348/vsc34871/output/microBIO6_fut-pre.tif",
    overwrite = TRUE
)
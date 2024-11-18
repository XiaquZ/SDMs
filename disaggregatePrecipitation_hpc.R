library(terra)

# Load the precipitation present time
bio12_pre <- rast("E:/SDMs/Stef_SDMs/precipitation_pre/TerraClim_12.tif")
bio15_pre <- rast("E:/SDMs/Stef_SDMs/precipitation_pre/TerraClim_15.tif")
bio12_pre
bio15_pre

# Disaggregate precipitation data to 25 m.
bio12_pre_dis <- terra::disagg(bio12_pre, fact=40)
bio15_pre_dis <- terra::disagg(bio15_pre, fact=40)
print(bio12_pre_dis)
print(bio15_pre_dis)

# Round the output
bio12_pre_out <- round(bio12_pre_dis, digits = 1)
bio15_pre_out <- round(bio15_pre_dis, digits = 1)
names(bio12_pre_out) <- "BIO12_TerraClimate_EUforests_2000-2020_25m"
names(bio15_pre_out) <- "BIO15_TerraClimate_EUforests_2000-2020_25m"
print(bio12_pre_out)
print(bio15_pre_out)

# Save raster
writeRaster(bio12_pre_out,
    filename = "/lustre1/scratch/348/vsc34871/output/bio12_2000-2020_terraclim_25m.tif",
    overwrite = TRUE
)

writeRaster(bio15_pre_out,
    filename = "/lustre1/scratch/348/vsc34871/output/bio12_2000-2020_terraclim_25m.tif",
    overwrite = TRUE
)
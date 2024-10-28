library(terra)

# Load ForestClim BIO5 BIO6 2000-2020.
microbio5 <- rast("/lustre1/scratch/348/vsc34871/input/ForestClim_05.tif")
microbio6 <- rast("/lustre1/scratch/348/vsc34871/input/ForestClim_06.tif")
microbio5

# Load resampled macroclimate BIO5 and 6, 1981-2010.
macrobio5 <- rast("/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO5_EUforests_25m.tif")
macrobio6 <- rast("/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO6_EUforests_25m.tif")
macrobio5
macrobio6
plot(macrobio5)

# Calculate the offset.
offsetbio5 <- microbio5 - macrobio5
offsetbio5
offsetbio5 <- round(offsetbio5, digits = 1)
writeRaster(offsetbio5,
    filename = "/lustre1/scratch/348/vsc34871/output/terraOffsetBIO5_2000-2020.tif",
    overwrite = TRUE
)

offsetbio6 <- microbio6 - macrobio6
offsetbio6
offsetbio6 <- round(offsetbio6, digits = 1)
writeRaster(offsetbio6,
    filename = "/lustre1/scratch/348/vsc34871/output/terraOffsetBIO6_2000-2020.tif",
    overwrite = TRUE
)
library(terra)

# Load ForestBIO1 offset.
matoffset <- rast("/lustre1/scratch/348/vsc34871/input/mean_annualOffset.tif")
matoffset

# Load resampled macroclimate BIO5 and 6, 1981-2010.
macrobio5 <- rast("/lustre1/scratch/348/vsc34871/output/2000-2020TerraBIO5_resampled_25m.tif")
macrobio6 <- rast("/lustre1/scratch/348/vsc34871/output/2000-2020TerraBIO6_resampled_25m.tif")
macrobio5
macrobio6

# Mask the macroclimate to keep only the forests.
macrobio5 <- mask(macrobio5, matoffset)
macrobio5
plot(macrobio5)
writeRaster(macrobio5,
    filename = "/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO5_EUforests_25m.tif",
    overwrite = TRUE
)

macrobio6 <- mask(macrobio6, matoffset)
macrobio6
plot(macrobio6)
writeRaster(macrobio6,
    filename = "/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO6_EUforests_25m.tif",
    overwrite = TRUE
)
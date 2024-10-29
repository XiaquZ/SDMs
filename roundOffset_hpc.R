library(terra)

# Load data.
offsetbio5_fut <- rast("/lustre1/scratch/348/vsc34871/output/offsetBIO5_2071-2100.tif")
offsetbio6_fut <- rast("/lustre1/scratch/348/vsc34871/output/offsetBIO6_2071-2100.tif")

# Round the data.
offsetbio5_fut2 <- round(offsetbio5_fut, digits = 1)
offsetbio6_fut2 <- round(offsetbio6_fut, digits = 1)

writeRaster(offsetbio5_fut2,
    filename = "/lustre1/scratch/348/vsc34871/output/offsetBIO5_2071-2100_1digit.tif",
    overwrite = TRUE
)

writeRaster(offsetbio6_fut2,
    filename = "/lustre1/scratch/348/vsc34871/output/offsetBIO6_2071-2100_1digit.tif",
    overwrite = TRUE
)
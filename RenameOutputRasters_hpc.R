library(terra)

# For present BIO5 offset
offsetbio5 <- rast(
    "/lustre1/scratch/348/vsc34871/output/terraOffsetBIO5_2000-2020.tif"
)
names(offsetbio5)
names(offsetbio5) <- "ForestClimTerraClimate_OffsetBIO5_2000-2020"
names(offsetbio5)

writeRaster(
    offsetbio5,
    "/lustre1/scratch/348/vsc34871/output/Rename_ForestClimTerraClimate_OffsetBIO5_2000-2020.tif",
    overwrite = TRUE
)

# For present BIO6 offset
offsetbio6 <- rast(
    "/lustre1/scratch/348/vsc34871/output/terraOffsetBIO6_2000-2020.tif"
)
names(offsetbio6)
names(offsetbio6) <- "ForestClimTerraClimate_OffsetBIO6_2000-2020"
names(offsetbio6)

writeRaster(
    offsetbio6,
    "/lustre1/scratch/348/vsc34871/output/Rename_ForestClimTerraClimate_OffsetBIO6_2000-2020.tif",
    overwrite = TRUE
)

##### For future offset
# BIO5
fut_offsetbio5 <- rast(
    "/lustre1/scratch/348/vsc34871/output/offsetBIO5_2071-2100_1digit.tif"
)
names(fut_offsetbio5)
names(fut_offsetbio5) <- "Predicted_OffsetBIO5_2071-2100"
names(fut_offsetbio5)
writeRaster(
    fut_offsetbio5,
    "/lustre1/scratch/348/vsc34871/output/Rename_Predicted_OffsetBIO5_2071-2100.tif",
    overwrite = TRUE
)

# BIO6
fut_offsetbio6 <- rast(
    "/lustre1/scratch/348/vsc34871/output/offsetBIO6_2071-2100_1digit.tif"
)
names(fut_offsetbio6)
names(fut_offsetbio6) <- "Predicted_OffsetBIO6_2071-2100"
names(fut_offsetbio6)
writeRaster(
    fut_offsetbio6,
    "/lustre1/scratch/348/vsc34871/output/Rename_Predicted_OffsetBIO6_2071-2100.tif",
    overwrite = TRUE
)
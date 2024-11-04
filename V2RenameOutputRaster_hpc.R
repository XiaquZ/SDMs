library(terra)

# Predicted future microclimate
fut_microbio5 <- rast(
    "/lustre1/scratch/348/vsc34871/output/PredictedMicroBIO5_2071-2100_ssp370.tif"
    )
print(fut_microbio5)
names(fut_microbio5) <- "PredictedMicroclimate_BIO5_2071-2100_ssp370"
print(fut_microbio5)
writeRaster(
    fut_microbio5,
"/lustre1/scratch/348/vsc34871/output/Rename_PredictedMicroclimate_BIO5_2071-2100_ssp370.tif",
    overwrite = TRUE
    )

# BIO6
fut_microbio6 <- rast(
    "/lustre1/scratch/348/vsc34871/output/PredictedMicroBIO6_2071-2100_ssp370.tif"
    )
names(fut_microbio6)
print(fut_microbio6)
names(fut_microbio6) <- "PredictedMicroclimate_BIO6_2071-2100_ssp370"
print(fut_microbio6)
writeRaster(
    fut_microbio6,
"/lustre1/scratch/348/vsc34871/output/Rename_PredictedMicroclimate_BIO6_2071-2100_ssp370.tif",
    overwrite = TRUE
    )

#### Delta macroclimate ####
#BIO5
deltamacro5 <- rast("/lustre1/scratch/348/vsc34871/output/deltaMacroclimate_bio5.tif")
print(deltamacro5)
names(deltamacro5) <- "deltaMacroclimate_BIO5_CHELSA2071-2100-TerraClim2000-2020"
print(deltamacro5)
writeRaster(
    deltamacro5,
    "/lustre1/scratch/348/vsc34871/output/Rename_DeltaMacroclimateBIO5_2000-2020WITH2071-2100.tif"
)

# BIO6
deltamacro6 <- rast("/lustre1/scratch/348/vsc34871/output/deltaMacroclimate_bio6.tif")
print(deltamacro6)
names(deltamacro6) <- "deltaMacroclimate_BIO6_CHELSA2071-2100-TerraClim2000-2020"
print(deltamacro6)
writeRaster(
    deltamacro6,
    "/lustre1/scratch/348/vsc34871/output/Rename_DeltaMacroclimateBIO6_2000-2020WITH2071-2100.tif"
)
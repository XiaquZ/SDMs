library(terra)

# Calculate the delta macroclimate between current and future.
# BIO5
macrobio5_fut <- rast(
    "/lustre1/scratch/348/vsc34871/input/2071-2100chelsaBIO5_EUforests_25m.tif"
)
macrobio5_pre <- rast(
    "/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO5_EUforests_25m.tif"
)

# BIO6
macrobio6_fut <- rast(
    "/lustre1/scratch/348/vsc34871/input/2071-2100chelsaBIO6_EUforests_25m.tif"
)
macrobio6_pre <- rast(
    "/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO6_EUforests_25m.tif"
)

deltamacro5 <- macrobio5_fut - macrobio5_pre
deltamacro6 <- macrobio6_fut - macrobio6_pre

# # Save rasters.
# writeRaster(deltamacro5,
#     filename = "/lustre1/scratch/348/vsc34871/output/deltaMacroclimate_bio5.tif",
#     overwrite = TRUE
# )

# writeRaster(deltamacro6,
#     filename = "/lustre1/scratch/348/vsc34871/output/deltaMacroclimate_bio6.tif",
#     overwrite = TRUE
# )

#### Future offset BIO5 BIO6 ####
# Load offset of current time.
offsetbio5_pre <- rast(
    "/lustre1/scratch/348/vsc34871/output/terraOffsetBIO5_2000-2020.tif"
)
offsetbio6_pre <- rast(
    "/lustre1/scratch/348/vsc34871/output/terraOffsetBIO6_2000-2020.tif"
)

# Load the delta macroclimate
deltamacro5 <- rast(
    "/lustre1/scratch/348/vsc34871/output/deltaMacroclimate_bio5.tif"
)
deltamacro6 <- rast(
    "/lustre1/scratch/348/vsc34871/output/deltaMacroclimate_bio6.tif"
)

# Load the data from linear models.
lmbio5 <- readRDS(file = "I:/DATA/output/BIO5BIO6_lm/lm_offsetVSterraBIO5.rds")
lmbio6 <- readRDS(file = "I:/DATA/output/BIO5BIO6_lm/lm_offsetVSterraBIO6.rds")
summary(lmbio5)
summary(lmbio6)

# Extract slope values.
slope5 <- 0.1751
slope6 <- -0.2481

# Calculate future offset.
offsetbio5_fut <- slope5 * deltamacro5 + offsetbio5_pre
offsetbio6_fut <- slope6 * deltamacro6 + offsetbio6_pre

# writeRaster(offsetbio5_fut,
#     filename = "/lustre1/scratch/348/vsc34871/output/offsetBIO5_2071-2100.tif",
#     overwrite = TRUE
# )

# writeRaster(offsetbio6_fut,
#     filename = "/lustre1/scratch/348/vsc34871/output/offsetBIO6_2071-2100.tif",
#     overwrite = TRUE
# )

# Calculate the future microclimate bio5 and 6.
# First, load the future macroclimate bio5 and 6.
# macro future BIO5
macrobio5_fut <- rast(
    "/lustre1/scratch/348/vsc34871/input/2071-2100chelsaBIO5_EUforests_25m.tif"
)

# macro future BIO6
macrobio6_fut <- rast(
    "/lustre1/scratch/348/vsc34871/input/2071-2100chelsaBIO6_EUforests_25m.tif"
)

# Offset BIO5 future.
offsetbio5_fut <- rast(
    "/lustre1/scratch/348/vsc34871/output/offsetBIO5_2071-2100_1digit.tif"
)
offsetbio6_fut <- rast(
    "/lustre1/scratch/348/vsc34871/output/offsetBIO6_2071-2100_1digit.tif"
)

# Future microclimate.
microbio5_fut <- macrobio5_fut + offsetbio5_fut
microbio6_fut <- macrobio6_fut + offsetbio6_fut
print(microbio5_fut)
print(microbio6_fut)

# Round the data.
microbio5_fut <- round(microbio5_fut, digits = 1)
microbio6_fut <- round(microbio6_fut, digits = 1)

# Save outputs.
writeRaster(microbio5_fut,
    filename = "/lustre1/scratch/348/vsc34871/output/PredictedMicroBIO5_2071-2100_ssp370.tif",
    overwrite = TRUE
)

writeRaster(microbio6_fut,
    filename = "/lustre1/scratch/348/vsc34871/output/PredictedMicroBIO6_2071-2100_ssp370.tif",
    overwrite = TRUE
)
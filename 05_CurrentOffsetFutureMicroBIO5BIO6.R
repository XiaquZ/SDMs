library(terra)

# Load the data from linear models.
lmbio5 <- readRDS(file = "I:/DATA/output/BIO5BIO6_lm/lm_BIO5_OffsetANDchelsaMacro.rds")
lmbio6 <- readRDS(file = "I:/DATA/output/BIO5BIO6_lm/lm_BIO6_OffsetANDchelsaMacro.rds")

# Extract slope values.
slope5 <- lmbio5$coefficients[[2]]
slope6 <- lmbio6$coefficients[[2]]

# Calculate the delta macroclimate between current and future.
# BIO5
macrobio5_fut <- rast(
    "H:/Input/CHELSAdata/BIO5BIO6_ssp370/2071-2100/2071-2100chelsaBIO5_EUforests_25m.tif"
)
macrobio5_pre <- rast(
    "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio5_EU_25m_2000-2019.tif"
)

# BIO6
macrobio6_fut <- rast(
    "H:/Input/CHELSAdata/BIO5BIO6_ssp370/2071-2100/2071-2100chelsaBIO6_EUforests_25m.tif"
)
macrobio6_pre <- rast(
    "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio6_EU_25m_2000-2019.tif"
)

deltamacro5 <- macrobio5_fut - macrobio5_pre
deltamacro6 <- macrobio6_fut - macrobio6_pre

#### Future offset BIO5 BIO6 ####
# Load offset of current time.
offsetbio5_pre <- rast(
  "H:/Output/SDM_bioclim/BIO5BIO6/present/BIO5BIO6_offset_20002020/ForestClimTerraClimate_OffsetBIO5_2000-2020_recalculated.tif"
  )
offsetbio6_pre <- rast(
  "H:/Output/SDM_bioclim/BIO5BIO6/present/BIO5BIO6_offset_20002020/ForestClimTerraClimate_OffsetBIO6_2000-2020_recalculated.tif"
  )

# Calculate future offset.
offsetbio5_fut <- slope5 * deltamacro5 + offsetbio5_pre
offsetbio6_fut <- slope6 * deltamacro6 + offsetbio6_pre

#### Calculate the future microclimate bio5 and 6.
# First, load the future macroclimate bio5 and 6.
microbio5_fut <- macrobio5_fut + offsetbio5_fut
microbio6_fut <- macrobio6_fut + offsetbio6_fut
print(microbio5_fut)
print(microbio6_fut)

# Round the data.
microbio5_fut <- round(microbio5_fut, digits = 1)
microbio6_fut <- round(microbio6_fut, digits = 1)


# Save data
writeRaster(microbio5_fut,
            "D:/PhD/Data/Output/BIO5BIO6_MicroclimateSSP370/BIO5_microclimate_euForests_2071_2100_ssp370.tif",
            overwrite = TRUE)
writeRaster(microbio6_fut,
            "D:/PhD/Data/Output/BIO5BIO6_MicroclimateSSP370/BIO6_microclimate_euForests_2071_2100_ssp370.tif",
            overwrite = TRUE)

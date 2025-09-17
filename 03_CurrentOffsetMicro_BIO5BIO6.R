library(terra)

# Load ForestClim BIO5 BIO6 2000-2020.
microbio5 <- rast("F:/Input/ForestBioClim/ForestClim_05.tif")
microbio6 <- rast("F:/Input/ForestBioClim/ForestClim_06.tif")
microbio5
plot(microbio5)

# Load resampled macroclimate BIO5 and 6, 1981-2010.
macrobio5 <- rast("F:/Output/TerraClimate/TerraBIO5_2000-2020_recalculated.tif")
macrobio6 <- rast("F:/Output/TerraClimate/TerraBIO6_2000-2020_recalculated.tif")
macrobio5
macrobio6
plot(macrobio5)

# Calculate the offset.
offsetbio5 <- microbio5 - macrobio5
offsetbio5
offsetbio5 <- round(offsetbio5, digits = 1)
writeRaster(offsetbio5,
    filename = "F:/Output/SDM_bioclim/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO5_2000-2020_recalculated.tif",
    overwrite = TRUE
)
pre_offsetbio5 <- rast(
  "F:/Output/SDM_bioclim/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO5_2000-2020.tif"
  )


offsetbio6 <- microbio6 - macrobio6
offsetbio6
plot(offsetbio6)
offsetbio6 <- round(offsetbio6, digits = 1)
writeRaster(offsetbio6,
    filename = "F:/Output/SDM_bioclim/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO6_2000-2020_recalculated.tif",
    overwrite = TRUE
)

#### Get the microclimate BIO5 and BIO6 based on CHELSA 2000-2019. #####
macro_pre_bio5 <- rast("I:/DATA/CHELSA/BIO5_CHELSA_2000_2019_epsg3035_25m_1digit.tif")
macro_pre_bio6 <- rast("I:/DATA/CHELSA/BIO6_CHELSA_2000_2019_epsg3035_25m_1digit.tif")

offset_pre_bio5 <- rast(
  "F:/Output/SDM_bioclim/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO5_2000-2020_recalculated.tif")
offset_pre_bio6 <- rast(
  "D:/Output/SDM_bioclim/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO6_2000-2020_recalculated.tif"
  )

# calculate the microclimate BIO5 and BIO6.
micro_pre_bio5 <- macro_pre_bio5 + offset_pre_bio5
print(micro_pre_bio5)
plot(micro_pre_bio5)

micro_pre_bio6 <- macro_pre_bio6 + offset_pre_bio6
print(micro_pre_bio6)
plot(micro_pre_bio6)

# Save microclimate BIO5 and BIO6
writeRaster(
  micro_pre_bio5,
  "F:/Output/SDM_bioclim/BIO5BIO6/present/Micro_BIO5_EU_25m_2000-2020_recalculated.tif",
  overwrite = TRUE
)

writeRaster(
  micro_pre_bio6,
  "D:/Output/SDM_bioclim/BIO5BIO6/present/Micro_BIO6_EU_25m_2000-2020_recalculated.tif",
  overwrite = TRUE
)

micro_bio5_old <- rast("F:/Output/SDM_bioclim/BIO5BIO6/present/Micro_BIO5_EU_25m_CHELSAbased_2000-2020.tif")
r <- micro_pre_bio5 - micro_bio5_old
# Bio5 offset in old version without using dismo:
terrabio5 <- rast("F:/Input/TerraClimate/Original/OffsetBIO5_2000-2020.tif")
micro_bio6_old <- rast("D:/Output/SDM_bioclim/BIO5BIO6/present/Micro_BIO6_EU_25m_CHELSAbased_2000-2020.tif")

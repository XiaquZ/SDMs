library(terra)

# Load ForestClim BIO5 BIO6 2000-2020.
microbio5 <- rast("H:/Input/ForestBioClim/ForestClim_05.tif")
microbio6 <- rast("H:/Input/ForestBioClim/ForestClim_06.tif")
microbio5
plot(microbio5)

# Load resampled macroclimate BIO5 and 6, 1981-2010.
macrobio5 <- rast("H:/Output/TerraClimate/Terra_bio5_EU_25m_2000-2020.tif")
macrobio6 <- rast("H:/Output/TerraClimate/Terra_bio6_EU_25m_2000-2020.tif")
macrobio5
macrobio6
plot(macrobio5)

# Calculate the offset.
offsetbio5 <- microbio5 - macrobio5
offsetbio5
offsetbio5 <- round(offsetbio5, digits = 1)
writeRaster(offsetbio5,
    filename = "H:/Output/BIO5BIO6/present/terraOffsetBIO5_2000-2020.tif",
    overwrite = TRUE
)

offsetbio6 <- microbio6 - macrobio6
offsetbio6
plot(offsetbio6)
offsetbio6 <- round(offsetbio6, digits = 1)
writeRaster(offsetbio6,
    filename = "H:/Output/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO6_2000-2020.tif",
    overwrite = TRUE
)

#### Get the microclimate BIO5 and BIO6 based on CHELSA 2000-2019. #####
macro_pre_bio5 <- rast("H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio5_EU_25m_2000-2019.tif")
macro_pre_bio6 <- rast("H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio6_EU_25m_2000-2019.tif")

offset_pre_bio5 <- rast("H:/Output/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO5_2000-2020.tif")
offset_pre_bio6 <- rast("H:/Output/BIO5BIO6/present/ForestClimTerraClimate_OffsetBIO6_2000-2020.tif")

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
  "H:/Output/BIO5BIO6/present/Micro_BIO5_EU_25m_2000-2020.tif",
  overwrite = TRUE
)

writeRaster(
  micro_pre_bio6,
  "H:/Output/BIO5BIO6/present/Micro_BIO6_EU_25m_2000-2020.tif",
  overwrite = TRUE
)

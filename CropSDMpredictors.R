#### Crop bioclim to run SDMs on smaller scale. #####
#### Testing the scripts.####
library(terra)
# Load belgium shapefile. #
be <- vect("H:/EuropeShapefile/Shapefiles/Belgium.shp")
plot(be)

# Bioclim data
micro_bio5 <- rast(
  "H:/Output/SDM_bioclim/BIO5BIO6/present/Micro_BIO5_EU_25m_CHELSAbased_2000-2020.tif"
  )
micro_bio6 <- rast(
  "H:/Output/SDM_bioclim/BIO5BIO6/present/Micro_BIO6_EU_25m_CHELSAbased_2000-2020.tif"
)
chelsa_bio12 <- rast(
  "H:/Output/SDM_bioclim/BIO12BIO15/CHELSA_bio12_EU_25m_2000-2019.tif"
)
chelsa_bio15 <- rast(
  "H:/Output/SDM_bioclim/BIO12BIO15/CHELSA_bio15_EU_25m_2000-2019.tif"
)
cec <- rast("H:/Input/SDMs_Soil variables/cec.tif")
clay <- rast("H:/Input/SDMs_Soil variables/clay.tif")
plot(cec)
plot(clay)

# Crop all variables for belgium
be_bio5 <- crop(micro_bio5, be)
be_bio5 <- mask(be_bio5, be)
writeRaster(
  be_bio5,
  "H:/Output/SDM_test/belgium/Predictors/be_bio5.tif",
  overwrite = TRUE
)

be_bio6 <- crop(micro_bio6, be)
be_bio6 <- mask(be_bio6, be)
writeRaster(
  be_bio6,
  "H:/Output/SDM_test/belgium/Predictors/be_bio6.tif",
  overwrite = TRUE
)

be_bio12 <- crop(chelsa_bio12, be)
be_bio12 <- mask(be_bio12, be)
writeRaster(
  be_bio12,
  "H:/Output/SDM_test/belgium/Predictors/be_bio12.tif",
  overwrite = TRUE
)

be_bio15 <- crop(chelsa_bio15, be)
be_bio15 <- mask(be_bio15, be)
writeRaster(
  be_bio15,
  "H:/Output/SDM_test/belgium/Predictors/be_bio15.tif",
  overwrite = TRUE
)

cec_be <- crop(cec, be)
cec_be <- mask(cec_be, be)
writeRaster(
  cec_be,
  "H:/Output/SDM_test/belgium/Predictors/cec_be.tif",
  overwrite = TRUE
)

clay_be <- crop(clay, be)
clay_be <- mask(clay_be, be)
plot(clay_be)
writeRaster(
  clay_be,
  "H:/Output/SDM_test/belgium/Predictors/clay_be.tif",
  overwrite = TRUE
)

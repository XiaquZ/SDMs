library(terra)
# Check the previous calculation of CHELSA bioclimatic variables.
# We used "biovars" function with dismo package to get these variables.
chelsa_bio5 <- rast("F:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio5_EU_25m_2000-2019.tif")
chelsa_bio6 <- rast("F:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio6_EU_25m_2000-2019.tif")

chelsa_bio12 <- rast("F:/Output/SDM_bioclim/BIO12BIO15/present/CHELSA_bio12_EU_25m_2000-2019.tif")
chelsa_bio15 <- rast("F:/Output/SDM_bioclim/BIO12BIO15/present/CHELSA_bio15_EU_25m_2000-2019.tif")

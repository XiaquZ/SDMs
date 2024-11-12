library(ENMeval)
library(predicts)
library(terra)

# Load the predictors
# BIO05, BIO06, BIO12,BIO15, cec, clay
bio5 <- rast(
    "E:/Output/BIO5BIO6/future/PredictedMicroclimate_BIO5_2071-2100_ssp370.tif"
    )
bio6 <- rast(
    "E:/Output/BIO5BIO6/future/PredictedMicroclimate_BIO6_2071-2100_ssp370.tif"
)
bio12 <- rast(
    "E:/Input/CHELSAdata/BIO12BIO15/2071-2100chelsaBIO12_EUforests_25m.tif"
)
bio15 <- rast(
    "E:/Input/CHELSAdata/BIO12BIO15/2071-2100chelsaBIO15_EUforests_25m.tif"
)
cec <- rast(
    "E:/Input/SDMs_Soil variables/cec.tif"
)
clay <- rast(
    "E:/Input/SDMs_Soil variables/clay.tif"
)

# load the shapefile to crop a small region
bel <- vect("E:/EuropeShapefile/Shapefiles/Belgium.shp")
print(bel)

# Crop the predictors.
# BIO5
bio5_bel <- crop(bio5, bel)
plot(bio5_bel)
bio5_bel <- mask(bio5_bel, bel)
plot(bio5_bel)
writeRaster(bio5_bel,
 "E:/Output/SDM_test/bio5_belgium.tif",
 overwrite = TRUE)

# BIO6
bio6_bel <- crop(bio6, bel)
plot(bio6_bel)
bio6_bel <- mask(bio6_bel, bel)
plot(bio6_bel)
writeRaster(bio6_bel,
 "E:/Output/SDM_test/bio6_belgium.tif",
 overwrite = TRUE)

# BIO12
bio12_bel <- crop(bio12, bel)
plot(bio12_bel)
bio12_bel <- mask(bio12_bel, bel)
plot(bio12_bel)
writeRaster(bio12_bel,
 "E:/Output/SDM_test/bio12_belgium.tif",
 overwrite = TRUE)

# BIO15
bio15_bel <- crop(bio15, bel)
plot(bio15_bel)
bio15_bel <- mask(bio15_bel, bel)
plot(bio15_bel)
writeRaster(bio15_bel,
 "E:/Output/SDM_test/bio15_belgium.tif",
 overwrite = TRUE)

# Two soil variables.
# cation exchange capacity
cec_bel <- crop(cec, bel)
plot(cec_bel)
cec_bel <- mask(cec_bel, bel)
plot(cec_bel)
writeRaster(cec_bel,
 "E:/Output/SDM_test/cec_belgium.tif",
 overwrite = TRUE)

# soil clay content
clay_bel <- crop(clay, bel)
plot(clay_bel)
clay_bel <- mask(clay_bel, bel)
plot(clay_bel)
writeRaster(clay_bel,
 "E:/Output/SDM_test/clay_belgium.tif",
 overwrite = TRUE)

# Change the name of predictors.
names(bio12_bel)
names(bio12_bel) <- "mean annual precipitation 2071-2100"
names(bio15_bel) <- "precipitation seasonality 2071-2100"
names(bio15_bel)
ForestClim_05 <- bio5_bel
ForestClim_06 <- bio6_bel
ForestClim_12 <- bio12_bel
ForestClim_15 <- bio15_bel
cec <- cec_bel
clay <- clay_bel

# Load one of the SDMs from stef
# actaea spicata
mdl <- load("E:/SDMs/Stef_SDMs/Models/Actaea spicata.RData")
e.mx_rp.f
eval.results.partitions(e.mx_rp.f) %>% head()
eval.models(e.mx_rp.f) %>% str(max.level = 1)
eval.occs(e.mx_rp.f) %>% head()
eval.results(e.mx_rp.f) |> head()
?eval.predictions

predictors <- c(
    ForestClim_05, ForestClim_06,
     ForestClim_12, ForestClim_15,
      cec, clay)
actaea <- predict(e.mx_rp.f, predictors, type = "logistic")

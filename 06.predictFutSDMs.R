library(ENMeval)
library(predicts)
library(terra)
library(dplyr)
library(enmSdmX)
# # Load the predictors
# # BIO05, BIO06, BIO12,BIO15, cec, clay
# bio5 <- rast(
#     "E:/Output/BIO5BIO6/future/PredictedMicroclimate_BIO5_2071-2100_ssp370.tif"
#     )
# bio6 <- rast(
#     "E:/Output/BIO5BIO6/future/PredictedMicroclimate_BIO6_2071-2100_ssp370.tif"
# )
# bio12 <- rast(
#     "E:/Input/CHELSAdata/BIO12BIO15/2071-2100chelsaBIO12_EUforests_25m.tif"
# )
# bio15 <- rast(
#     "E:/Input/CHELSAdata/BIO12BIO15/2071-2100chelsaBIO15_EUforests_25m.tif"
# )
# cec <- rast(
#     "E:/Input/SDMs_Soil variables/cec.tif"
# )
# clay <- rast(
#     "E:/Input/SDMs_Soil variables/clay.tif"
# )

# load the shapefile to crop a small region
bel <- vect("E:/EuropeShapefile/Shapefiles/Belgium.shp")
print(bel)

# # Crop the predictors.
# # BIO5
# bio5_bel <- crop(bio5, bel)
# plot(bio5_bel)
# bio5_bel <- mask(bio5_bel, bel)
# plot(bio5_bel)
# writeRaster(bio5_bel,
#  "E:/Output/SDM_test/bio5_belgium.tif",
#  overwrite = TRUE)

# # BIO6
# bio6_bel <- crop(bio6, bel)
# plot(bio6_bel)
# bio6_bel <- mask(bio6_bel, bel)
# plot(bio6_bel)
# writeRaster(bio6_bel,
#  "E:/Output/SDM_test/bio6_belgium.tif",
#  overwrite = TRUE)

# # BIO12
# bio12_bel <- crop(bio12, bel)
# plot(bio12_bel)
# bio12_bel <- mask(bio12_bel, bel)
# plot(bio12_bel)
# writeRaster(bio12_bel,
#  "E:/Output/SDM_test/bio12_belgium.tif",
#  overwrite = TRUE)

# # BIO15
# bio15_bel <- crop(bio15, bel)
# plot(bio15_bel)
# bio15_bel <- mask(bio15_bel, bel)
# plot(bio15_bel)
# writeRaster(bio15_bel,
#  "E:/Output/SDM_test/bio15_belgium.tif",
#  overwrite = TRUE)

# # Two soil variables.
# # cation exchange capacity
# cec_bel <- crop(cec, bel)
# plot(cec_bel)
# cec_bel <- mask(cec_bel, bel)
# plot(cec_bel)
# writeRaster(cec_bel,
#     "E:/Output/SDM_test/cec_belgium.tif",
#     overwrite = TRUE
# )

# # soil clay content
# clay_bel <- crop(clay, bel)
# plot(clay_bel)
# clay_bel <- mask(clay_bel, bel)
# plot(clay_bel)
# writeRaster(clay_bel,
#  "E:/Output/SDM_test/clay_belgium.tif",
#  overwrite = TRUE)

# Change the name of predictors.
ForestClim_05 <- rast("E:/Output/SDM_test/bio5_belgium.tif")
ForestClim_06 <- rast("E:/Output/SDM_test/bio6_belgium.tif")
ForestClim_12 <- rast("E:/Output/SDM_test/bio12_belgium.tif")
ForestClim_15 <- rast("E:/Output/SDM_test/bio15_belgium.tif")
cec <- rast("E:/Output/SDM_test/cec_belgium.tif")
clay <- rast("E:/Output/SDM_test/clay_belgium.tif")

names(ForestClim_05) <- "ForestClim_05"
names(ForestClim_06) <- "ForestClim_06"
names(ForestClim_12) <- "ForestClim_12"
names(ForestClim_15) <- "ForestClim_15"
names(cec) <- "cec"
names(clay) <- "clay"

# Predict
predictors <- c(
    ForestClim_05, ForestClim_06,
    ForestClim_12, ForestClim_15,
    cec, clay
)
print(predictors)

# Load one of the SDMs from stef
# actaea spicata
mdl <- load("E:/SDMs/Stef_SDMs/Models/Actaea spicata.RData")
mdl <- e.mx_rp.f
# Results inspection
eval.results.partitions(mdl) %>% head()
eval.models(mdl) %>% str(max.level = 1)
eval.occs(mdl) %>% head()
eval.results(mdl) |> head()

# Select the best SDM based on delta AIC
# This dplyr operation executes the sequential criteria explained above.
res <- eval.results(mdl)
opt_mdl <- res[res$delta.AICc == min(res$delta.AICc), ]
opt_mdl
min_index <- which(res$delta.AICc == min(res$delta.AICc))
mdl_select <- mdl@models[[min_index]]
str(mdl_select)

# actaea <- terra::predict(predictors, mdl_select, type = "logistic")
# # Try another package
# actaea <- predicts::predict(predictors, mdl_select, type = "logistic")
# ?predicts::predict
# ## The above two functions cannot run maxnet model.

# Try predictMaxNet function.
actaea02 <- predictMaxNet(mdl_select, predictors, type = "logistic")
plot(actaea02)

# Compare with the current distribution map
actaea_pre <- rast("E:/SDMs/Stef_SDMs/Maps_current/Actaea spicata.tif")
actaea_pre <- crop(actaea_pre, bel)
actaea_pre <- mask(actaea_pre, bel)
actaea02 <- actaea02 * 100
# Plot the two maps.
par(mfrow = c(1, 2))
plot(actaea_pre, main = "Suitability map for Actaea Spicata 2000-2020")
plot(actaea02, main = "Suitability map for Actaea Spicata 2071-2100")
par(mfrow = c(1, 1))
# Save outputs
writeRaster(
    actaea_pre,
    "E:/Output/SDM_test/Actaea Spicata_2000-2020.tif",
    overwrite = TRUE
)
writeRaster(
    actaea02,
    "E:/Output/SDM_test/Actaea Spicata_2071-2100.tif",
    overwrite = TRUE
)
?predictMaxNet

# Test on another species
mdl02 <- load("E:/SDMs/Stef_SDMs/Models/Adoxa moschatellina.RData")
mdl02 <- e.mx_rp.f

# Select the best SDM based on delta AIC
# This dplyr operation executes the sequential criteria explained above.
res <- eval.results(mdl02)
opt_mdl <- res[res$delta.AICc == 0, ]
opt_mdl
min_index <- which(res$delta.AICc == 0)
mdl_select <- mdl02@models[[min_index]]

species02 <- predictMaxNet(mdl_select, predictors, type = "logistic")
plot(species02)
species02 <- species02 * 100

# Compare with the current distribution map
adoxa_pre <- rast("E:/SDMs/Stef_SDMs/Maps_current/Adoxa moschatellina.tif")
adoxa_pre <- crop(adoxa_pre, bel)
adoxa_pre <- mask(adoxa_pre, bel)
plot(adoxa_pre)
adoxa_pre
species02

# Save outputs
writeRaster(
    species02,
    "E:/Output/SDM_test/Adoxa moschatellina_2000-2020.tif",
    overwrite = TRUE
)
writeRaster(
    adoxa_pre,
    "E:/Output/SDM_test/Adoxa moschatellina_2071-2100.tif",
    overwrite = TRUE
)
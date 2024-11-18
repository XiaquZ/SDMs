library(terra)
library(ENMeval)
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
switz <- vect("E:/EuropeShapefile/Shapefiles/Switzerland.shp")
plot(switz)

# # Crop the predictors.
# # BIO5
# bio5_switz <- crop(bio5, switz)
# plot(bio5_switz)
# bio5_switz <- mask(bio5_switz, switz)
# plot(bio5_switz)
# writeRaster(bio5_switz,
#  "E:/Output/SDM_test/switzerland/bio5_switz.tif",
#  overwrite = TRUE)

# # BIO6
# bio6_switz <- crop(bio6, switz)
# plot(bio6_switz)
# bio6_switz <- mask(bio6_switz, switz)
# plot(bio6_switz)
# writeRaster(bio6_switz,
#  "E:/Output/SDM_test/switzerland/bio6_switz.tif",
#  overwrite = TRUE)

# # BIO12
# bio12_switz <- crop(bio12, switz)
# plot(bio12_switz)
# bio12_switz <- mask(bio12_switz, switz)
# plot(bio12_switz)
# writeRaster(bio12_switz,
#  "E:/Output/SDM_test/switzerland/bio12_switz.tif",
#  overwrite = TRUE)

# # BIO15
# bio15_switz <- crop(bio15, switz)
# plot(bio15_switz)
# bio15_switz <- mask(bio15_switz, switz)
# plot(bio15_switz)
# writeRaster(bio15_switz,
#  "E:/Output/SDM_test/switzerland/bio15_switz.tif",
#  overwrite = TRUE)

# # Two soil variables.
# # cation exchange capacity
# cec_switz <- crop(cec, switz)
# plot(cec_switz)
# cec_switz <- mask(cec_switz, switz)
# plot(cec_switz)
# writeRaster(cec_switz,
#     "E:/Output/SDM_test/switzerland/cec_switz.tif",
#     overwrite = TRUE
# )

# # soil clay content
# clay_switz <- crop(clay, switz)
# plot(clay_switz)
# clay_switz <- mask(clay_switz, switz)
# plot(clay_switz)
# writeRaster(clay_switz,
#  "E:/Output/SDM_test/switzerland/clay_switz.tif",
#  overwrite = TRUE)

# Change the name of predictors.
ForestClim_05 <- rast("E:/Output/SDM_test/switzerland/bio5_switz.tif")
ForestClim_06 <- rast("E:/Output/SDM_test/switzerland/bio6_switz.tif")
ForestClim_12 <- rast("E:/Output/SDM_test/switzerland/bio12_switz.tif")
ForestClim_15 <- rast("E:/Output/SDM_test/switzerland/bio15_switz.tif")
cec <- rast("E:/Output/SDM_test/switzerland/cec_switz.tif")
clay <- rast("E:/Output/SDM_test/switzerland/clay_switz.tif")

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
# # Results inspection
# eval.results.partitions(mdl) %>% head()
# eval.models(mdl) %>% str(max.level = 1)
# eval.occs(mdl) %>% head()
# eval.results(mdl) |> head()

# Select the best SDM based on delta AIC
# This dplyr operation executes the sequential criteria explained above.
res <- eval.results(mdl)
opt_mdl <- res[res$delta.AICc == min(res$delta.AICc), ]
opt_mdl
min_index <- which(res$delta.AICc == min(res$delta.AICc))
mdl_select <- mdl@models[[min_index]]
str(mdl_select)

# Try predictMaxNet function.
start_t <- Sys.time()
actaea <- predictMaxNet(mdl_select, predictors, type = "logistic")
end_t <- Sys.time()
print(end_t - start_t)
## Time difference of 4.285568 mins
plot(actaea)
?bioticVelocity

# Compare with the current distribution map
actaea_pre <- rast("E:/SDMs/Stef_SDMs/Maps_current/Actaea spicata.tif")
actaea_pre <- crop(actaea_pre, switz)
actaea_pre <- mask(actaea_pre, switz)
plot(actaea_pre)
actaea_pre

# Future prediction multiply by 100 
actaea <- actaea * 100
actaea

# Try to calculate biotic velocity
s <- c(actaea_pre, actaea)
s
start_t <- Sys.time()
bv <- bioticVelocity(
	x = s,
	times = c(2010, 2085),
	cores = 1
)
end_t <- Sys.time()
print(end_t - start_t)

# Plot the two maps.
par(mfrow = c(1, 2))
plot(actaea_pre, main = "Suitability map for Actaea Spicata 2000-2020")
plot(actaea, main = "Suitability map for Actaea Spicata 2071-2100")
par(mfrow = c(1, 1))
# Save outputs
writeRaster(
    actaea_pre,
    "E:/Output/SDM_test/switzerland/Actaea Spicata_2000-2020_switz.tif",
    overwrite = TRUE
)
writeRaster(
    actaea,
    "E:/Output/SDM_test/switzerland/Actaea Spicata_2071-2100_switz.tif",
    overwrite = TRUE
)

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
    "E:/Output/SDM_test/Adoxa moschatellina_2071-2100.tif",
    overwrite = TRUE
)
writeRaster(
    adoxa_pre,
    "E:/Output/SDM_test/Adoxa moschatellina_2000-2020.tif",
    overwrite = TRUE
)

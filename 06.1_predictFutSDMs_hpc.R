library(ENMeval)
library(dplyr)
library(enmSdmX)

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

# Load Stef's SDMs for prediction.
mdl02 <- load("E:/SDMs/Stef_SDMs/Models/Adoxa moschatellina.RData")
mdl02 <- e.mx_rp.f

# Select the best SDM based on delta AIC
res <- eval.results(mdl02)
opt_mdl <- res[res$delta.AICc == 0, ]
opt_mdl

#choose the best model with lowest AIC.
min_index <- which(res$delta.AICc == 0)
## What if the minimum delta.AICc is not 0?
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

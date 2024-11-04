library(terra)

# Load data
macrobio5 <- rast(
    "/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO5_EUforests_25m.tif"
)
macrobio6 <- rast(
    "/lustre1/scratch/348/vsc34871/output/2000-2020terraBIO6_EUforests_25m.tif"
)
microbio5 <- rast("/lustre1/scratch/348/vsc34871/input/ForestClim_05.tif")
microbio6 <- rast("/lustre1/scratch/348/vsc34871/input/ForestClim_06.tif")

#### Make amplification and buffering maps. ####
# For BIO5, check which areas had microclimate > macroclimate.
bio5class <- function(micro5, macro5) {
    ifel(
        micro5 > macro5, 200,
        ifel(micro5 < macro5, 100, 150)
    )
}

bio6class <- function(micro6, macro6) {
    ifel(
        micro6 > macro6, 400,
        ifel(micro6 < macro6, 300, 250)
    )
}

# Output
eubio5 <- bio5class(microbio5, macrobio5)
eubio6 <- bio6class(microbio6, macrobio6)

writeRaster(eubio5,
    filename = "/lustre1/scratch/348/vsc34871/output/bio5micromacro.tif",
    overwrite = TRUE
)

writeRaster(eubio6,
    filename = "/lustre1/scratch/348/vsc34871/output/bio6micromacro.tif",
    overwrite = TRUE
)

library(terra)

# Load data
macrobio5 <- rast(
    "E:/Input/TerraClimate/2000-2020terraBIO5_EUforests_25m.tif"
)
macrobio6 <- rast(
    "E:/Input/TerraClimate/2000-2020terraBIO6_EUforests_25m.tif"
)
microbio5 <- rast("E:/Input/ForestBioClim/ForestClim_05.tif")
microbio6 <- rast("E:/Input/ForestBioClim/ForestClim_06.tif")

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

# Crop a small part to test the function.
nl <- vect("E:/EuropeShapefile/Shapefiles/NL_shapefile.shp")
nlbio5mic <- crop(microbio5, nl)
nlbio5mic <- mask(nlbio5mic, nl)
plot(nlbio5mic)
nlbio5mac <- crop(macrobio5, nl)
nlbio5mac <- mask(nlbio5mac, nl)
plot(nlbio5mac)
test <- bio5class(nlbio5mic, nlbio5mac)
test
plot(test)

# BIO6
bio6mic <- crop(microbio6, nl)
bio6mic <- mask(bio6mic, nl)
bio6mac <- crop(macrobio6, nl)
bio6mac <- mask(bio6mac, nl)
plot(bio6mic)
plot(bio6mac)
test2 <- bio6class(bio6mic, bio6mac)
plot(test2)

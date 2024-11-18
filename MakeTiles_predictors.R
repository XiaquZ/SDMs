library(terra)
library(sf)
bio5 <- rast("E:/Output/BIO5BIO6/future/PredictedMicroclimate_BIO5_2071-2100_ssp370.tif")
bio6 <- rast("E:/Output/BIO5BIO6/future/PredictedMicroclimate_BIO6_2071-2100_ssp370.tif")
bio12 <- rast("E:/Input/CHELSAdata/BIO12BIO15/2071-2100chelsaBIO12_EUforests_25m.tif")
bio15 <- rast("E:/Input/CHELSAdata/BIO12BIO15/2071-2100chelsaBIO15_EUforests_25m.tif")
cec <- rast("E:/Input/SDMs_Soil variables/cec.tif")
clay <- rast("E:/Input/SDMs_Soil variables/clay.tif")

rastlist <- list.files(path = "E:/Output/BIO5BIO6/future", pattern='.tif$', all.files= T, full.names= T)
stk <- terra::rast(rastlist)
stk[[1]]

#Read in data in sf format.
c_shape <- read_sf("E:/EuropeShapefile/Shapefiles/Europe.shp") #change pathway if change to another country

# create an initial grid for centroid determination
c_grid <- st_make_grid(c_shape, cellsize = c(4e5, 4e5), square = TRUE) |>
  st_as_sf()
# inspect
plot(st_geometry(c_shape))
plot(st_geometry(c_grid), border = "red", add = TRUE)

c_grid_spat <- vect(c_grid)

## Making tiles:
pred_n <- 6
for(i in 1:pred_n){
filename <- paste0("/lustre1/scratch/348/vsc34871/output/Tiles/FVoCC_50kmSR", "_.tif") #Change path
tile <- makeTiles(stk[[1]], c_grid_spat, filename, overwrite = TRUE)
}

library(terra)
t.lst <- list.files("F:/SDMs/SDMs_current/Results/Species_tiles/Actaea spicata/", pattern=".tif", full.names=TRUE)
MIs <- function(t.lst, fout="") {
  r <- vrt(t.lst)
  r <- round(r, digits = 1)
  # assign CRS (EPSG:3035)
  crs(r) <- "EPSG:3035"
  if (fout != "") {
    writeRaster(r, fout, overwrite=TRUE)
    fout
  } else {
    wrap(r)
  }
} 

MIs(
  t.lst,
  fout = "F:/SDMs/SDMs_current/Results/Species_final/Actaea spicata.tif"
)

pred_species <- rast("F:/SDMs/SDMs_current/Results/Species_final/Actaea spicata.tif")
plot(pred_species)

# crop a small part of results.
be <- vect("F:/EuropeShapefile/Shapefiles/Belgium.shp")
be_crop <- crop(pred_species, be)
be_mask <- mask(be_crop, be)

# Crop Stef's result.
ecol_species <- rast("F:/SDMs/Stef_SDMs/Maps_current/logistic/Actaea spicata.tif")
be_ecol_c <- crop(ecol_species, be)
be_ecol_m <- mask(be_ecol_c, be)

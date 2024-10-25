library(terra)

# Load chelsa macroclimate BIO5 and BIO6 data of current time.
macro_bio5 <- rast(
    "E:/Input/CHELSAdata/BIO5BIO6/1981-2010/archives/CHELSA_bio5_1981-2010_V.2.1.tif")
macro_bio6 <- rast(
    "E:/Input/CHELSAdata/BIO5BIO6/1981-2010/archives/CHELSA_bio6_1981-2010_V.2.1.tif")
macro_bio5
macro_bio6
plot(macro_bio5)
plot(macro_bio6)

# Crop to the study areas.
eu_shp <- vect("I:/EUshap/Europe.shp")
plot(eu_shp)
print(eu_shp)
x <- crs(macro_bio5)
eu_shp <- project(eu_shp, x)

macro_bio5 <- crop(macro_bio5, eu_shp, mask = TRUE)
plot(macro_bio5)

macro_bio6 <- crop(macro_bio6, eu_shp, mask = TRUE)
plot(macro_bio6)

# Reproject
template <- rast("I:/DATA/mean_annualOffset.tif")
x <- crs(template)
macro_bio5 <- project(macro_bio5, x)
plot(macro_bio5)

macro_bio6 <- project(macro_bio6, x)
plot(macro_bio6)

writeRaster(macro_bio5,
    filename = "E:/Input/CHELSAdata/BIO5BIO6/macroBIO5_forResampling.tif",
    overwrite = TRUE
)
writeRaster(macro_bio6,
    filename = "E:/Input/CHELSAdata/BIO5BIO6/macroBIO6_forResampling.tif",
    overwrite = TRUE
)

# Resample data to 25 meter on hpc. See Reampl_chelsaMacro_hpc.R
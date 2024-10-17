library(terra)

# Read the paths
paths <- read.delim2("E:/Input/CHELSAdata/futBIO12BIO15.txt", header = F)

# Download CHELSA BIO12 2071-2100
pathsbio12 <- paths[grep("bio12", paths$V1), ]
pathsbio12[1]

# Forloop
rastlsbio12 <- list()
for (i in 1:5) {
    rastlsbio12[[i]] <- rast(pathsbio12[i])
}
plot(rastlsbio12[[2]])
rastlsbio12 <- rast(rastlsbio12)
rastlsbio12
# Crop to the study areas.
eu_shp <- vect("I:/EUshap/Europe.shp")
plot(eu_shp)
print(eu_shp)
x <- crs(rastlsbio12)
eu_shp <- project(eu_shp, x)
rastlsbio12 <- crop(rastlsbio12, eu_shp, mask = TRUE)
plot(rastlsbio12)
rastlsbio12
meanbio12 <- mean(rastlsbio12)
plot(meanbio12)
meanbio12

# Reproject
template <- rast("I:/DATA/mean_annualOffset.tif")
x <- crs(template)
meanbio12 <- project(meanbio12, x)
plot(meanbio12)

writeRaster(meanbio12,
    filename = "E:/Input/CHELSAdata/BIO12BIO15/MacroEUbio12_2071-2100_ssp370_705m.tif",
    overwrite = TRUE
)

# Download CHELSA BIO15 2071-2100
pathsbio15 <- paths[grep("bio15", paths$V1), ]
pathsbio15[1]

# Forloop
rastlsbio15 <- list()
for (i in 1:5) {
    rastlsbio15[[i]] <- rast(pathsbio15[i])
}
plot(rastlsbio15[[2]])
rastlsbio15 <- rast(rastlsbio15)
rastlsbio15

# Crop to the study areas.
eu_shp <- vect("I:/EUshap/Europe.shp")
plot(eu_shp)
print(eu_shp)
x <- crs(rastlsbio15)
eu_shp <- project(eu_shp, x)
rastlsbio15 <- crop(rastlsbio15, eu_shp, mask = TRUE, overwrite = T)
plot(rastlsbio15)
rastlsbio15
meanbio15 <- mean(rastlsbio15)
plot(meanbio15)
meanbio15

# Reproject
template <- rast("I:/DATA/mean_annualOffset.tif")
x <- crs(template)
meanbio15 <- project(meanbio15, x)
plot(meanbio15)
meanbio15

writeRaster(meanbio15,
    filename = "E:/Input/CHELSAdata/BIO12BIO15/MacroEUbio15_2071-2100_ssp370_705m.tif",
    overwrite = TRUE
)

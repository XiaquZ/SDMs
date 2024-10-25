library(terra)

# Read the paths
paths <- read.delim2("E:/Input/CHELSAdata/futurebio5bio6chelsa.txt", header = F)

# Download CHELSA BIO5 2070-2100
pathsbio5 <- paths[grep("bio5", paths$V1), ]
pathsbio5[2]

# Forloop
rastlsbio5 <- list()
for (i in 1:5) {
    rastlsbio5[[i]] <- rast(pathsbio5[i])
}
plot(rastlsbio5[[3]])
rastlsbio5 <- rast(rastlsbio5)

# Crop to the study areas.
eu_shp <- vect("I:/EUshap/Europe.shp")
plot(eu_shp)
print(eu_shp)
x <- crs(rastlsbio5)
eu_shp <- project(eu_shp, x)
rastlsbio5 <- crop(rastlsbio5, eu_shp, mask = TRUE)
plot(rastlsbio5)
rastlsbio5
meanbio5 <- mean(rastlsbio5)
plot(meanbio5)

# Reproject
template <- rast("I:/DATA/mean_annualOffset.tif")
x <- crs(template)
meanbio5 <- project(meanbio5, x)
plot(meanbio5)

writeRaster(meanbio5,
    filename = "E:/Input/CHELSAdata/BIO5BIO6/MacroEUbio5_2071-2100_ssp370_705m.tif",
    overwrite = TRUE
)

# Subset macroclimate bio6.
pathsbio6 <- paths[grep("bio6", paths$V1), ]
pathsbio6[1]

# Forloop
rastlsbio6 <- list()
for (i in 1:5) {
    rastlsbio6[[i]] <- rast(pathsbio6[i])
}
plot(rastlsbio6[[2]])
rastlsbio6 <- rast(rastlsbio6)

# Crop to the study areas.
rastlsbio6 <- crop(rastlsbio6, eu_shp, mask = TRUE)
plot(rastlsbio6)
rastlsbio6
meanbio6 <- mean(rastlsbio6)
plot(meanbio6)

# Reproject
meanbio6 <- project(meanbio6, x)
plot(meanbio6)

writeRaster(meanbio6,
    filename = "E:/Input/CHELSAdata/BIO5BIO6/2071-2100/MacroEUbio6_2071-2100_ssp370_705m.tif",
    overwrite = TRUE
)
library(terra)

# Read the paths
paths <- read.delim2("E:/Input/CHELSAdata/futBIO12BIO15.txt", header = F)

# Download CHELSA BIO5 2071-2100
pathsbio12 <- paths[grep("bio12", paths$V1), ]
pathsbio12[1]

# Forloop
rastlsbio12 <- list()
for (i in 1:5) {
    rastlsbio12[[i]] <- rast(pathsbio12[i])
}
plot(rastlsbio12[[2]])
rastlsbio12 <- rast(rastlsbio12)

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

# Reproject
template <- rast("I:/DATA/mean_annualOffset.tif")
x <- crs(template)
meanbio12 <- project(meanbio12, x)
plot(meanbio12)

writeRaster(meanbio12,
    filename = "E:/Input/CHELSAdata/BIO5BIO6/MacroEUbio5_2071-2100_ssp370_705m.tif",
    overwrite = TRUE
)
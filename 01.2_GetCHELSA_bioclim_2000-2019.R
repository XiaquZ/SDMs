library(terra)
library(dismo)

# Set your input folder path
#### For tasmin during 2000-2019.####
input_dir <- "D:/PhD/Data/CHELSA_monthly/tasmin2000-2019"

# List all the tif files
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
s_files <- rast(tif_files)

# Create grouping vector: repeat each month index 20 times
# (i.e., group layers by month)
grouping <- rep(1:12, each = 20)

# Get the mean temperature for each month
monthly_tasmin <- tapp(s_files, grouping, fun = mean)

# Assign month names to the layers
month_names <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Assuming there are exactly 12 layers (one for each month)
names(monthly_tasmin) <- month_names

# Change to degree unit.
tasmin_final <- monthly_tasmin * 0.1 - 273.15
tasmin_final <- round(tasmin_final, digits = 1)

# Crop to study areas.
monthly_tasmin <- rast("D:/PhD/Data/Output/CHELSA/CHELSA_tasmin_2000-2019_12monthsStck.tif")
eu_shp <- vect("I:/EUshap/Europe.shp")
eu_wgs <- project(eu_shp, crs(monthly_tasmax))
eu_tasmin <- crop(monthly_tasmin, eu_wgs)
eu_tasmin_final <- mask(eu_tasmax, eu_wgs)
plot(eu_tasmin_final)

writeRaster(
    eu_tasmin_final,
    "D:/PhD/Data/Output/CHELSA/CHELSA_tasmin_Europe_2000-2019_12monthsStck.tif",
    overwrite = TRUE
)




#### For tasmax during 2000-2019.####
input_dir <- "D:/PhD/Data/CHELSA_monthly/tasmax2000-2019"

# List all .tif files
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
s_files <- rast(tif_files)

# Create grouping vector: repeat each month index 20 times
# (i.e., group layers by month)
grouping <- rep(1:12, each = 20)

# Get the mean temperature for each month
monthly_means_max <- tapp(s_files, grouping, fun = mean)

# Assign month names to the layers
month_names <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Assuming there are exactly 12 layers (one for each month)
names(monthly_means_max) <- month_names

# Change to degree unit.
mean_final <- monthly_means_max * 0.1 - 273.15
mean_final <- round(mean_final, digits = 1)

# Crop to study areas.
monthly_tasmax <- rast("D:/PhD/Data/Output/CHELSA/CHELSA_tasmax_2000-2019_12monthsStack.tif")
eu_shp <- vect("I:/EUshap/Europe.shp")
eu_wgs <- project(eu_shp, crs(monthly_tasmax))
eu_tasmax <- crop(monthly_tasmax, eu_wgs)
eu_tasmax_final <- mask(eu_tasmax, eu_wgs)
plot(eu_tasmax_final)

writeRaster(
  eu_tasmax_final,
  "D:/PhD/Data/Output/CHELSA/CHELSA_tasmax_Europe_2000-2019_12monthsStack.tif",
  overwrite = TRUE
)

#### For precipitation 2000-2018 ####
input_dir <- "D:/PhD/Data/CHELSA_monthly/pr_2000-2019"

# List all .tif files
tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
s_files <- rast(tif_files)

# Create grouping vector: repeat each month index 19 times
# (i.e., group layers by month)
grouping <- rep(1:12, each = 19)

# Get the precipitation for each month.
monthly_means_pr <- tapp(s_files, grouping, fun = mean)
print(monthly_means_pr)

# Assign month names to the layers
month_names <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Assuming there are exactly 12 layers (one for each month)
names(monthly_means_pr) <- month_names

# Crop to study areas.
eu_shp <- vect("I:/EUshap/Europe.shp")
eu_wgs <- project(eu_shp, crs(monthly_means_pr))
eu_pr <- crop(monthly_means_pr, eu_wgs)
eu_pr_final <- mask(eu_pr, eu_wgs)

# Change to liter(kg) per square meter.
mean_pr_final <- eu_pr_final * 0.01 
mean_pr_final <- round(mean_pr_final, digits = 1)


writeRaster(
  mean_pr_final,
  "D:/PhD/Data/Output/CHELSA/CHELSA_pr_Europe_2000-2018_12monthsStack.tif",
  overwrite = TRUE
)

#################################################
#### Use dismo to get bioclimatic variables. ####
#################################################
library(raster)
tasmax <- stack("D:/PhD/Data/Output/CHELSA/CHELSA_tasmax_Europe_2000-2019_12monthsStack.tif")
tasmin <- stack("D:/PhD/Data/Output/CHELSA/CHELSA_tasmin_Europe_2000-2019_12monthsStck.tif")
prec <- stack("D:/PhD/Data/Output/CHELSA/CHELSA_pr_Europe_2000-2018_12monthsStack.tif")

# Get the bioclimatics.
# All the input tasmax, tasmin, prec were scaled based on CHELSA report.
bio_vars <- biovars(prec, tasmin, tasmax)
writeRaster(bio_vars, 
            filename = "D:/PhD/Data/Output/CHELSA/CHELSA_bioclim_Europe_2000_2019.tif", 
            format = "GTiff", 
            overwrite = TRUE)

bio01 <- bio_vars[[1]]
plot(bio01)
forest <- rast("I:/DATA/forestBIO1_convertedTO0_25m.tif")
bio01 <- rast(bio01)
bio01_repro <- project(bio01, crs(forest))
bio01_repro
plot(bio01_repro)
bio01_resamp <- resample(bio01_repro, forest, method = "bilinear")
bio01_mask <- mask(bio01_resamp, forest)

## Extract bio5 and bio6 from output bioclim ####

# Load data.
bioclim <- rast("H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bioclim_Europe_2000_2019.tif")
plot(bioclim)

# bio5
bio5 <- bioclim[[5]]
plot(bio5)
writeRaster(bio5, 
            "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio5_EU_2000-2019.tif",
            overwrite = TRUE)

# bio6
bio6 <- bioclim[[6]]
plot(bio6)
writeRaster(bio6, 
            "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio6_EU_2000-2019.tif",
            overwrite = TRUE)

# bio12
bio12 <- bioclim[[12]]
plot(bio12)
writeRaster(bio12, 
            "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio12_EU_2000-2019.tif",
            overwrite = TRUE)

# bio15
bio15 <- bioclim[[15]]
plot(bio15)
writeRaster(bio15, 
            "H:/Input/CHELSAdata/BIOclim2000-2019/CHELSA_bio15_EU_2000-2019.tif",
            overwrite = TRUE)

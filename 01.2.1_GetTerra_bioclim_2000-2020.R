#################################################
#### Use dismo to get bioclimatic variables. ####
#################################################
library(raster)
library(dismo)

tasmax <- stack("I:/DATA/output/terraClimate/terra_tasmax_monthly_climatology_wgs84_2000-2020.tif")
tasmin <- stack("I:/DATA/output/terraClimate/terra_tasmin_monthly_climatology_wgs84_2000-2020.tif")
prec <- stack("I:/DATA/output/terraClimate/terra_prec_monthly_climatology_wgs84_2000-2020.tif")

# Get the bioclimatics.
# All the input tasmax, tasmin, prec were scaled based on CHELSA report.
bio_vars <- biovars(prec, tasmin, tasmax)
writeRaster(bio_vars, 
            filename = "H:/Output/TerraClimate/Terra_bioclim_Europe_2000_2020_wgs84.tif", 
            format = "GTiff", 
            overwrite = TRUE)

forest <- rast("I:/DATA/forestBIO1_convertedTO0_25m.tif")


## Extract bio5 and bio6 from output bioclim ####

# Load data.
bioclim <- rast("H:/Output/TerraClimate/Terra_bioclim_Europe_2000_2020_wgs84.tif")
plot(bioclim)

# bio5
bio5 <- bioclim[[5]]
plot(bio5)
bio5 <- project(bio5, crs(forest))
bio5_resamp <- resample(bio5, forest, method = "bilinear")
bio5_mask <- mask(bio5_resamp, forest)
bio5_round <- round(bio5_mask, digits = 1)

writeRaster(bio5_round, 
            "H:/Output/TerraClimate/Terra_bio5_EU_25m_2000-2019.tif",
            overwrite = TRUE)

# bio6
bio6 <- bioclim[[6]]
plot(bio6)
bio6_reproj <- project(bio6, crs(forest))
bio6_resamp <- resample(bio6_reproj, forest, method = "bilinear")
bio6_mask <- mask(bio6_resamp, forest)
bio6_round <- round(bio6_mask, digits = 1)
print(bio6_round)

writeRaster(bio6_round, 
            "H:/Output/TerraClimate/Terra_bio6_EU_25m_2000-2019.tif",
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
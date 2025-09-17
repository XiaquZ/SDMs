#################################################
#### Use dismo to get bioclimatic variables. ####
#################################################
library(raster)
library(dismo)

tasmax <- stack(
  "F:/Input/TerraClimate/Monthly_EU/terra_tasmax_monthly_climatology_wgs84_2000-2020_12layers.tif"
  )
tasmin <- stack(
  "F:/Input/TerraClimate/Monthly_EU/terra_tasmin_monthly_climatology_wgs84_2000-2020_12layers.tif"
  )
prec <- stack(
  "F:/Input/TerraClimate/Monthly_EU/terra_prec_monthly_climatology_wgs84_2000-2020_12layers.tif"
  )

forest <- rast("I:/DATA/forestBIO1_convertedTO0_25m.tif")

# Get the bioclimatics.
# All the input tasmax, tasmin, prec were scaled based on CHELSA report.
bio_vars <- biovars(prec, tasmin, tasmax)

BIO5 <- rast(bio_vars[[5]])
BIO5_repro <- project(BIO5, crs(forest))
BIO5_resamp <- resample(BIO5_repro, forest, method = "bilinear")
BIO5_mask <- mask(BIO5_resamp, forest) # Same as the previous results.
BIO5_round <- round(BIO5_mask, digits = 1)

writeRaster(BIO5_round,
            "F:/Output/TerraClimate/TerraBIO5_2000-2020_recalculated.tif",
            overwrite = TRUE)

BIO6 <- rast(bio_vars[[6]])
BIO6_repro <- project(BIO6, crs(forest))
BIO6_resamp <- resample(BIO6_repro, forest, method = "bilinear")
BIO6_mask <- mask(BIO6_resamp, forest) # Same as the previous results.
BIO6_round <- round(BIO6_mask, digits = 1)
writeRaster(BIO6_round,
            "F:/Output/TerraClimate/TerraBIO6_2000-2020_recalculated.tif",
            overwrite = TRUE)
# Optional: sanity check — compute BIO5 directly from tmax12 (should match biovars BIO5)
# Same for BIO6.
tmax12 <- rast(
  "F:/Input/TerraClimate/Monthly_EU/terra_tasmax_monthly_climatology_wgs84_2000-2020_12layers.tif"
  )
tmin12 <- rast(
  "F:/Input/TerraClimate/Monthly_EU/terra_tasmin_monthly_climatology_wgs84_2000-2020_12layers.tif"
)

BIO5_direct <- app(tmax12, max, na.rm = TRUE)
BIO6_direct <- app(tmin12, min, na.rm = TRUE)
print(global(BIO5_direct - BIO5, fun = function(x) c(min=min(x,na.rm=TRUE),
                                                     max=max(x,na.rm=TRUE),
                                                     mean=mean(x,na.rm=TRUE))))

writeRaster(bio_vars, 
            filename = "F:/Output/TerraClimate/Terra_bioclim_Europe_2000_2020_wgs84.tif", 
            format = "GTiff", 
            overwrite = TRUE)

## Extract bio5 and bio6 from output bioclim ####
# These are previous computed results. We check below if the new ones
# the same as previous outputs.

# Load data.
bioclim <- rast("F:/Output/TerraClimate/Terra_bioclim_Europe_2000_2020_wgs84.tif")
plot(bioclim)

# bio5
bio5 <- bioclim[[5]]
show(bio5)
bio5 <- project(bio5, crs(forest))
bio5_resamp <- resample(bio5, forest, method = "bilinear")
bio5_mask <- mask(bio5_resamp, forest)
bio5_round <- round(bio5_mask, digits = 1)

writeRaster(bio5_round, 
            "H:/Output/TerraClimate/Terra_bio5_EU_25m_2000-2019.tif",
            overwrite = TRUE)

# bio6
bio6 <- bioclim[[6]]
show(bio6)
bio6_reproj <- project(bio6, crs(forest))
bio6_resamp <- resample(bio6_reproj, forest, method = "bilinear")
bio6_mask <- mask(bio6_resamp, forest)
bio6_round <- round(bio6_mask, digits = 1)
print(bio6_round)

writeRaster(bio6_round, 
            "H:/Output/TerraClimate/Terra_bio6_EU_25m_2000-2019.tif",
            overwrite = TRUE)


library(terra)

tmax <- rast("F:/Input/TerraClimate/Original/terraclimate_tmax_2000-2020.nc")
plot(tmax)
tmin <- rast("F:/Input/TerraClimate/Original/terraclimate_tmin_2000-2020.nc")
show(tmin)
show(tmax)
prec <- rast("F:/Input/TerraClimate/Original/terraclimate_ppt_2000-2020.nc")
show(prec)

# Load the eu shapefile.
eu_shp <- vect("I:/EUshap/Europe.shp")
plot(eu_shp)
print(eu_shp)
x <- crs(tmax)
eu_shp <- project(eu_shp, x)

# Crop the maximumT and minimumT.
tmax_crop <- crop(tmax, eu_shp)
tmax_mask <- mask(tmax_crop, eu_shp)
plot(tmax_mask)

tmin_crop <- crop(tmin, eu_shp)
tmin_mask <- mask(tmin_crop, eu_shp)
show(tmin_mask)
plot(tmin_mask)

prec_crop <- crop(prec, eu_shp)
prec_mask <- mask(prec_crop, eu_shp)
print(prec_mask)
plot(prec_mask)

writeRaster(tmax_mask,
    filename = "F:/Input/TerraClimate/Monthly_EU/terraClimate_tmax_EU_wgs84.tif",
    overwrite = TRUE
)

writeRaster(tmin_mask,
            filename = "F:/Input/TerraClimate/Monthly_EU/terraClimate_tmin_EU_wgs84.tif",
            overwrite = TRUE
)

writeRaster(
    prec_mask,
    filename = "F:/Input/TerraClimate/Monthly_EU//terraClimate_prec_EU_wgs84.tif",
    overwrite = TRUE
)

#################################
#### calculate monthly data. ####
#################################
# Load the monthly data.
prec <- rast("F:/Input/TerraClimate/Monthly_EU//terraClimate_prec_EU_wgs84.tif")
tasmax <- rast("F:/Input/TerraClimate/Monthly_EU/terraClimate_tmax_EU_wgs84.tif")
tasmin <- rast("F:/Input/TerraClimate/Monthly_EU/terraClimate_tmin_EU_wgs84.tif")

##  Build grouping vectors from the time stamp --------------------------
mons <- as.integer(format(time(prec), "%m")) # 1 … 12,   length = 252

##  Long-term monthly climatology (mean for all Januaries, all Februaries …) --
prec_monthly <- tapp(prec, index = mons, fun = mean, na.rm = TRUE)
names(prec_monthly) <- sprintf("prec_%02d", 1:12) # Jan, Feb, …

##  (Optional) Save to disk ----------------------------------------------------
writeRaster(
    prec_monthly,
    "F:/Input/TerraClimate/Monthly_EU/terra_prec_monthly_climatology_wgs84_2000-2020_12layers.tif",
    overwrite = TRUE
)

##  Inspect results -----------------------------------------------------------
prec_monthly

## For tasmax and tasmin, we can do the same.
mons_tas <- as.integer(format(time(tasmax), "%m")) # 1 … 12,   length = 252

##  Long-term monthly climatology (mean for all Januaries, all Februaries …) --
tasmax_monthly <- tapp(tasmax, index = mons_tas, fun = mean, na.rm = TRUE)
names(tasmax_monthly) <- sprintf("tmax_%02d", 1:12) # Jan, Feb, …
tasmin_monthly <- tapp(tasmin, index = mons_tas, fun = mean, na.rm = TRUE)
names(tasmin_monthly) <- sprintf("tmin_%02d", 1:12) # Jan, Feb, …
##  (Optional) Save to disk ----------------------------------------------------
writeRaster(
    tasmax_monthly,
    "F:/Input/TerraClimate/Monthly_EU/terra_tasmax_monthly_climatology_wgs84_2000-2020_12layers.tif",
    overwrite = TRUE
)
writeRaster(
    tasmin_monthly,
    "F:/Input/TerraClimate/Monthly_EU/terra_tasmin_monthly_climatology_wgs84_2000-2020_12layers.tif",
    overwrite = TRUE
)



#### Reproject to GRS80.####
template <- rast("I:/DATA/mean_annualOffset.tif")
x <- crs(template)
tmax_bio5 <- project(tmax_bio5, x)
plot(tmax_bio5)

tmin_bio6 <- project(tmin_bio6, x)
plot(tmin_bio6)

writeRaster(tmax_bio5,
    filename = "E:/Input/TerraClimate/terra_BIO5_forResamp.tif",
    overwrite = TRUE
)

writeRaster(tmin_bio6,
    filename = "E:/Input/TerraClimate/terra_BIO6_forResamp.tif",
    overwrite = TRUE
)

library(terra)
library(raster)
library(dismo)

# Set your input folder path
#### For tasmin during 2000-2019.####
root <- "D:/PhD/Data/CHELSA_monthly"

files_tasmax <- list.files(file.path(root, "tasmax2000-2019"),
                           pattern = "\\.tif$", full.names = TRUE)
files_tasmin <- list.files(file.path(root, "tasmin2000-2019"),
                           pattern = "\\.tif$", full.names = TRUE)
files_prec   <- list.files(file.path(root, "pr_2000-2018"),
                           pattern = "\\.tif$", full.names = TRUE)

eu_shp <- vect("I:/EUshap/Europe.shp")

# For tasmin:

  b <- basename(files_tasmin)
  m <- regexec(".*_(\\d{2})_(\\d{4})_", b)
  mm_yy <- regmatches(b, m)
  if (any(lengths(mm_yy) < 3)) stop("Month/year not found in some filenames.")
  month <- vapply(mm_yy, function(x) as.integer(x[2]), integer(1))
  year  <- vapply(mm_yy, function(x) as.integer(x[3]), integer(1))
  
  ord <- order(year, month)
  r <- rast(files_tasmin[ord])
  time(r) <- as.Date(sprintf("%04d-%02d-01", year, month))  # attach time axis
  eu_wgs <- project(eu_shp, crs(r))
  r_crop <- crop(r, eu_wgs)
  r_final <- mask(r_crop, eu_wgs)
  r_tmin <- r_final * 0.1 - 273.15
  show(r_tmin)
  writeRaster(r_tmin,
              "D:/PhD/Data/Output/CHELSA/CHELSA_EU_Monthly_tmin_2000_2019_scaled.tif",
              overwrite = TRUE)

# For tasmax:
  b_tmax <- basename(files_tasmax)
  m_tmax <- regexec(".*_(\\d{2})_(\\d{4})_", b_tmax)
  mm_yy_tmax <- regmatches(b_tmax, m_tmax)
  if (any(lengths(mm_yy_tmax) < 3)) stop("Month/year not found in some filenames.")
  month_tmax <- vapply(mm_yy_tmax, function(x) as.integer(x[2]), integer(1))
  year_tmax  <- vapply(mm_yy_tmax, function(x) as.integer(x[3]), integer(1))
  ord_tmax <- order(year_tmax, month_tmax)
  r_tmax <- rast(files_tasmax[ord_tmax])
  time(r_tmax) <- as.Date(
    sprintf("%04d-%02d-01", year_tmax[ord_tmax], month_tmax[ord_tmax])
    )
  eu_wgs <- project(eu_shp, crs(r_tmax))
  r_tmax_crop <- crop(r_tmax, eu_wgs)
  r_tmax_final <- mask(r_tmax_crop, eu_wgs)
  r_tmax_final <- r_tmax_final * 0.1 -273.15
  show(r_tmax_final) 
  writeRaster(r_tmax_final,
              "D:/PhD/Data/Output/CHELSA/CHELSA_EU_Monthly_tmax_2000_2019_scaled.tif",
              overwrite = TRUE)

  # For precipitation:
  b_prec <- basename(files_prec)
  m_prec <- regexec(".*_(\\d{2})_(\\d{4})_", b_prec)
  mm_yy_prec <- regmatches(b_prec, m_prec)
  if (any(lengths(mm_yy_prec) < 3)) stop("Month/year not found in some filenames.")
  month_prec <- vapply(mm_yy_prec, function(x) as.integer(x[2]), integer(1))
  year_prec  <- vapply(mm_yy_prec, function(x) as.integer(x[3]), integer(1))
  ord_prec <- order(year_prec, month_prec)
  r_prec <- rast(files_prec[ord_prec])
  time(r_prec) <- as.Date(
    sprintf("%04d-%02d-01", year_prec[ord_prec], month_prec[ord_prec])
  )
  eu_wgs <- project(eu_shp, crs(r_prec))
  r_prec_crop <- crop(r_prec, eu_wgs)
  r_prec_final <- mask(r_prec_crop, eu_wgs)
  r_prec_final <- r_prec_final * 0.01 # Change to liter(kg) per square meter.
  show(r_prec_final) # mm/month already
  writeRaster(r_prec_final,
              "D:/PhD/Data/Output/CHELSA/CHELSA_EU_Monthly_prec_2000_2018_scaled.tif",
              overwrite = TRUE)

  # ------------------ BIO5 for 2000–2019 (tmax only) ------------------
  keep_0019 <- time(r_tmax_final) >= as.Date("2000-01-01") & time(r_tmax_final) <= as.Date("2019-12-01")
  tmax_0019 <- r_tmax_final[[which(keep_0019)]]
  tasmin_0019 <- r_tmin[[which(keep_0019)]]
  
  mo_0019   <- as.integer(format(time(tmax_0019), "%m"))
  
  tmax12_0019 <- tapp(tmax_0019, index = mo_0019, fun = mean, na.rm = TRUE)
  tmin12_0019 <- tapp(tasmin_0019, index = mo_0019, fun = mean, na.rm = TRUE)
  
  BIO5_2000_2019 <- app(tmax12_0019, fun = max, na.rm = TRUE)
  BIO6_2000_2019 <- app(tmin12_0019, min, na.rm = TRUE)  # Min Temp of Coldest Month
  names(BIO5_2000_2019) <- "BIO5_CHELSA_2000_2019"
  names(BIO6_2000_2019) <- "BIO6_CHELSA_2000_2019"
  writeRaster(BIO5_2000_2019, 
              "D:/PhD/Data/Output/CHELSA/BIO5_CHELSA_2000_2018_wgs84.tif",  
              overwrite=TRUE)
  writeRaster(BIO6_2000_2019, 
              "D:/PhD/Data/Output/CHELSA/BIO6_CHELSA_2000_2018_wgs84.tif",  
              overwrite=TRUE)
 

  # ------------------ BIO5, BIO6, BIO12, BIO15 for 2000–2018 via biovars ------------------
  # Align to common timestamps
  t_common <- Reduce(intersect, list(time(r_tmax_final), time(r_tmin), time(r_prec_final)))
  tasmax_c <- r_tmax_final[[ which(time(r_tmax_final) %in% t_common) ]]
  tasmin_c <- r_tmin[[ which(time(r_tmin) %in% t_common) ]]
  prec_c   <- r_prec_final  [[ which(time(r_prec_final)   %in% t_common) ]]
  
  # Monthly climatologies (simple means per calendar month)
  mo_c <- as.integer(format(time(tasmax_c), "%m"))
  tmax12_c <- tapp(tasmax_c, index = mo_c, fun = mean, na.rm = TRUE)
  tmin12_c <- tapp(tasmin_c, index = mo_c, fun = mean, na.rm = TRUE)
  prec12_c <- tapp(prec_c,   index = mo_c, fun = mean, na.rm = TRUE)
  
  #################################################
  #### Use dismo to get bioclimatic variables. ####
  #################################################
  # Run biovars on climatologies
  bio_c <- biovars(prec = stack(prec12_c), tmin = stack(tmin12_c), tmax = stack(tmax12_c))
  
  BIO5_2000_2018  <- rast(bio_c[[5 ]]); names(BIO5_2000_2018)  <- "BIO5_CHELSA_2000_2018"
  BIO6_2000_2018  <- rast(bio_c[[6 ]]); names(BIO6_2000_2018)  <- "BIO6_CHELSA_2000_2018"
  BIO12_2000_2018 <- rast(bio_c[[12]]); names(BIO12_2000_2018) <- "BIO12_CHELSA_2000_2018"
  BIO15_2000_2018 <- rast(bio_c[[15]]); names(BIO15_2000_2018) <- "BIO15_CHELSA_2000_2018"
  
  # Save Rasters.
  writeRaster(BIO12_2000_2018, 
              "D:/PhD/Data/Output/CHELSA/BIO12_CHELSA_2000_2018_wgs84.tif", 
              overwrite = TRUE)
  writeRaster(BIO15_2000_2018, 
              "D:/PhD/Data/Output/CHELSA/BIO15_CHELSA_2000_2018_wgs84.tif", 
              overwrite = TRUE)
  
  #### Resample data ####
  forest <- rast("I:/DATA/forestBIO1_convertedTO0_25m.tif")
  
  # BIO5
  BIO5_0019_repro <- project(BIO5_2000_2019, crs(forest))
  show(BIO5_0019_repro)
  BIO5_0019_resamp <- resample(BIO5_0019_repro, forest, method = "bilinear")
  BIO5_0019_mask <- mask(BIO5_0019_resamp, forest)
  BIO5_0019_round <- round(BIO5_0019_mask, digits = 1)
  writeRaster(BIO5_0019_round,
              "D:/PhD/Data/Output/CHELSA/BIO5_CHELSA_2000_2019_epsg3035_25m_1digit.tif",
              overwrite = TRUE)
  
  # BIO6
  BIO6_0019_repro <- project(BIO6_2000_2019, crs(forest))
  show(BIO6_0019_repro)
  BIO6_0019_resamp <- resample(BIO6_0019_repro, forest, method = "bilinear")
  BIO6_0019_mask <- mask(BIO6_0019_resamp, forest)
  BIO6_0019_round <- round(BIO6_0019_mask, digits = 1)
  writeRaster(BIO6_0019_round,
              "D:/PhD/Data/Output/CHELSA/BIO6_CHELSA_2000_2019_epsg3035_25m_1digit.tif",
              overwrite = TRUE)
  
  # BIO5 2000-2018
  BIO5_0018_repro <- project(BIO5_2000_2018, crs(forest))
  show(BIO5_0018_repro)
  BIO5_0018_resamp <- resample(BIO5_0018_repro, forest, method = "bilinear")
  BIO5_0018_mask <- mask(BIO5_0018_resamp, forest)
  BIO5_0018_round <- round(BIO5_0018_mask, digits = 1)
  writeRaster(BIO5_0018_round,
              "D:/PhD/Data/Output/CHELSA/BIO5_CHELSA_2000_2018_epsg3035_25m_1digit.tif",
              overwrite = TRUE)
  # BIO6 2000-2018
  BIO6_0018_repro <- project(BIO6_2000_2018, crs(forest))
  show(BIO6_0018_repro)
  BIO6_0018_resamp <- resample(BIO6_0018_repro, forest, method = "bilinear")
  BIO6_0018_mask <- mask(BIO6_0018_resamp, forest)
  BIO6_0018_round <- round(BIO6_0018_mask, digits = 1)
  writeRaster(BIO6_0018_round,
              "D:/PhD/Data/Output/CHELSA/BIO6_CHELSA_2000_2018_epsg3035_25m_1digit.tif",
              overwrite = TRUE)
  
  
#### For CHELSA ssp370 2071-2100. ####



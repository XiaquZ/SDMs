# Soilgrid data download tutorial from: 
# https://rstudio-pubs-static.s3.amazonaws.com/1267440_9592ed6b8cfc40a7a0490bf7ab865e85.html#
library(XML)
library(sf)
library(gdalUtilities)
library(terra)
library(dplyr)
library(leaflet)
library(RColorBrewer)

url = "https://files.isric.org/soilgrids/latest/data/" # Path to the webDAV data.

# basic strings
voi = "phh2o" # soil pH 
depth = "0-30cm"
quantile = "median"
# concatenate the strings
(variable = paste(url, voi, sep=""))

(layer = paste(variable,depth,quantile, sep="_")) # layer of interest 


#### Stef's soil grids data 0-30 cm. ####
soil_05 <- rast("H:/SoilGrids_Stef/euForests/phh2o_0-5cm_mean_eu_25m_rounded.tif")
soil_515 <- rast("H:/SoilGrids_Stef/euForests/phh2o_5-15cm_mean_eu_25m_rounded.tif")
soil_1530 <- rast("H:/SoilGrids_Stef/euForests/phh2o_15-30cm_mean_eu_25m_rounded.tif")

# Stack
pHx10 <- c(soil_05, soil_515, soil_1530)
names(pHx10) <- c("pH_0_5", "pH_5_15", "pH_15_30")

# --- 2) Depth-weighted averaging on the H+ scale ---
# weights are layer thicknesses in cm: 0–5, 5–15, 15–30
w <- c(5, 10, 15)
pH_0_30 <- -log10( (5*10^(-r1) + 10*10^(-r2) + 15*10^(-r3)) / 30 )
pH0_30 <- app(pHx10, fun = function(v){
  if (all(is.na(v))) return(NA_real_)
  pH <- v / 10
  H  <- 10^(-pH)
  keep <- !is.na(H)
  w <- c(5, 10, 15)   # thickness weights defined inside
  Hbar <- sum(H[keep] * w[keep]) / sum(w[keep])
  -log10(Hbar)
}, cores = 5)


# --- 3) Return as pH*10 (integer), write to disk ---
pH0_30_x10 <- round(pH0_30 * 10)
names(pH0_30_x10) <- "phh2o_0_30cm_mean"

# Choose an output datatype that fits 0–140 range (uint8). Set NA flag if you use one.
out_file <- "H:/SoilGrids_Stef/euForests/phh2o_0-30cm_WeightedMean_25m.tif"
writeRaster(
  pH0_30_x10, out_file,
  overwrite = TRUE,
  datatype = "INT1U",  # unsigned 8-bit integer
  NAflag = 255         # pick your NA value; 255 is common for uint8
)

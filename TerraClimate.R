library(sf)
library(terra)
library(ncdf4)

# Open the NetCDF file using ncdf4
var <- "tmax"
baseurlagg <- paste0(
    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",
    var,
    "_1958_CurrentYear_GLOBE.nc"
)

nc <- nc_open(baseurlagg)

# Download the NetCDF file
# download.file(baseurlagg, destfile = "terraclimate_tmax.nc", mode = "wb")

# Extract latitude, longitude, and tmax variable
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
tmax <- ncvar_get(nc, "tmax", start = c(1, 1, 1), count = c(1, 1, -1)) # e.g., first time slice

# Close the NetCDF connection
nc_close(nc)

# Create a terra raster from the data
nc_raster <- rast(tmax, extent = ext(-9.0, 35.0, 30.0, 75.0), crs = "EPSG:4326")
plot(tmax)

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
flat <- match(abs(lat - x[2]) < 1 / 48, 1)
latindex <- which(flat %in% 1)
flon <- match(abs(lon - x[1]) < 1 / 48, 1)
lonindex <- which(flon %in% 1)
start <- c(lonindex, latindex, 1)
count <- c(1, 1, -1)


# read in the full period of record using aggregated files

data <- as.numeric(ncvar_get(nc, varid = var, start = start, count))

nc_data <- nc_open('I:/DATA/TerraClimate/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc')
# Save the print(nc) dump to a text file
{
    sink('I:/DATA/TerraClimate/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.txt')
 print(nc_data)
    sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
tmax <- ncvar_get(nc_data, " time", "units")$value

# Close the NetCDF connection
nc_close(nc)

# Create a terra raster from the data
nc_raster <- rast(tmax, extent = ext(-9.0, 35.0, 30.0, 75.0), crs = "EPSG:4326")
plot(t)

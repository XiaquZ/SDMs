library(RNetCDF)

# Enter lat and lon ranges
lat.range <- c(30.0, 75.0) # ! Ranges instead of point values. Order does not matter
lon.range <- c(-9.0, 35.0)

# Open the NetCDF file using ncdf4
var <- "tmax"
baseurlagg <- paste0(
    "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",
    var,
    "_1958_CurrentYear_GLOBE.nc"
)

nc <- open.nc(baseurlagg)
lon <- var.get.nc(nc, "lon")
lat <- var.get.nc(nc, "lat")
lat.range <- sort(lat.range)
# !sort user input values from low to high
lon.range <- sort(lon.range)
lat.index <- which(lat >= lat.range[1] & lat <= lat.range[2])
# ! index values within specified range
lon.index <- which(lon >= lon.range[1] & lon <= lon.range[2])
lat.n <- length(lat.index) # !value for count
lon.n <- length(lon.index)
start <- c(lon.index[1], lat.index[1], 1)
count <- c(lon.n, lat.n, NA)
# ! parameter change: 'NA' instead of '-1' to signify entire dimension

# read in the full period of record using aggregated files

data <- var.get.nc(nc, variable = var, start = start, count, unpack = TRUE)
head(data)

library(reshape2)
data_df <- melt(data)
write.csv(data_df, "extracted_data.csv", row.names = FALSE)
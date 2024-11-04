library(terra)

tmax <- rast("E:/Input/TerraClimate/Original/terraclimate_tmax_2000-2020.nc")
plot(tmax)
tmin <- rast("E:/Input/TerraClimate/Original/terraclimate_tmin_2000-2020.nc")
show(tmin)
show(tmax)

# Load the eu shapefile.
eu_shp <- vect("I:/EUshap/Europe.shp")
plot(eu_shp)
print(eu_shp)
x <- crs(tmin)
eu_shp <- project(eu_shp, x)

# Crop the maximumT and minimumT.
tmax_c <- crop(tmax, eu_shp, mask = TRUE, overwrite = TRUE)
plot(tmax_c)
tmin_c <- crop(tmin, eu_shp, mask = TRUE, overwrite = TRUE)
show(tmin_c)
plot(tmin_c)

# writeRaster(tmax_c,
#     filename = "E:/Input/TerraClimate/terraClimate_tmax_EU_wgs84.tif",
#     overwrite = TRUE
# )

# writeRaster(tmin_c,
#     filename = "E:/Input/TerraClimate/terraClimate_tmin_EU_wgs84.tif",
#     overwrite = TRUE
# )

#### calculate BIO5 based on the maximum of each pixel across layers. ####
tmax_bio5 <- max(tmax_c)
tmin_bio6 <- min(tmin_c)
which.max(tmax_bio5)
which.min(tmin_bio6)
plot(tmax_bio5)
plot(tmin_bio6)

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

# # Create a data frame to store the output
# output <- data.frame(matrix(ncol = 3, nrow = 0))
# cols <- c("year", "maxT", "which_max")
# colnames(output) <- cols

# years <- 2000:2020
# for (i in years) {
#     # Subset the data by year.
#     tmax_year <- tmax_c[[
#         time(tmax_c) >= paste0(i, "-01-01") &
#             time(tmax_c) < paste0(i + 1, "-01-01")
#     ]]

#     # Show and plot the data
#     show(tmax_year)
#     plot(tmax_year)

#     # Compute maximum temperature for each month
#     mx <- minmax(tmax_year, compute = TRUE)[2, ]

#     # Identify the month with the highest temperature
#     mxid <- which.max(mx)

#     # Print the highest temperature and corresponding month
#     output[i - 1999, 1] <- i
#     output[i - 1999, 2] <- max(mx)
#     output[i - 1999, 3] <- mxid
# }
# which.max(output$maxT) # row 21 has the maxT. 2020 July.

# #### tmin ####
# tmin_c <- crop(tmin, eu_shp, mask = TRUE)
# plot(tmin_c)
# # Create a data frame to store the output
# output_min <- data.frame(matrix(ncol = 3, nrow = 0))
# cols <- c("year", "minT", "which_min")
# colnames(output_min) <- cols

# years <- 2000:2020
# for (i in years) {
#     # Subset the data by year.
#     tmin_year <- tmin_c[[
#         time(tmin_c) >= paste0(i, "-01-01") &
#             time(tmin_c) < paste0(i + 1, "-01-01")
#     ]]

#     # Show and plot the data
#     show(tmin_year)
#     plot(tmin_year)

#     # Compute maximum temperature for each month
#     min <- minmax(tmin_year, compute = TRUE)[1, ]

#     # Identify the month with the highest temperature
#     minid <- which.min(min)

#     # Print the highest temperature and corresponding month
#     output_min[i - 1999, 1] <- i
#     output_min[i - 1999, 2] <- min(min)
#     output_min[i - 1999, 3] <- minid
# }
# i=2000
# which.min(output_min$minT) # row 4 has the minT. 2003 January.

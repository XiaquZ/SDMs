remotes::install_github("8Ginette8/gbif.range")
library(gbif.range)
# Download
obs.pt <- get_gbif(sp_name = "Panthera tigris")

# Plot species records
countries <- rnaturalearth::ne_countries(type = "countries", returnclass = "sv")
terra::plot(countries, col = "#bcbddc")
points(obs.pt[, c("decimalLongitude","decimalLatitude")], pch = 20, col = "#99340470", cex = 1.5)
get_status("Panthera tigris", all = FALSE)
# Download ecoregion and read
eco.terra <- read_bioreg(bioreg_name = "eco_terra", save_dir = NULL)

# Range
range.tiger <- get_range(occ_coord = obs.pt,
                        bioreg = eco.terra,
                        bioreg_name = "ECO_NAME",
                        degrees_outlier = 5,
                        clustered_points_outlier = 3)

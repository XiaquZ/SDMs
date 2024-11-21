library(terra)
library(sf)

c_shape <- read_sf("/lustre1/scratch/348/vsc34871/input/Shapefile/Europe.shp")
plot(c_shape)

# create an initial grid for centroid determination
c_grid <- st_make_grid(c_shape, cellsize = c(4e5, 4e5)) |>
  st_as_sf()
# inspect
plot(st_geometry(c_shape))
plot(st_geometry(c_grid), border = "red", add = TRUE)

c_grid_spat <- vect(c_grid)

predlist <- list.files(
  path = "/lustre1/scratch/348/vsc34871/input/Predictors/",
  pattern = ".tif$",
  all.files = T, full.names = T
)
stk <- terra::rast(predlist)
print(stk)

# Define a mapping of old names (patterns) to new names
name_mapping <- list(
  "CHELSAV2.1_BIO12_2071-2100_ssp370_25m" = "ForestClim_12",
  "CHELSAV2.1_BIO15_2071-2100_ssp370_25m" = "ForestClim_15",
  "PredictedMicroclimate_BIO5_2071-2100_ssp370"  = "ForestClim_05",
  "PredictedMicroclimate_BIO6_2071-2100_ssp370"  = "ForestClim_06",
  "clay"  = "clay",
  "cec" = "cec"
)
# Loop through the layers and rename based on the mapping
for (pattern in names(name_mapping)) {
  # Find layers that match the current pattern
  matching_indices <- grep(pattern, names(stk))
  
  # Rename the matched layers
  names(stk)[matching_indices] <- name_mapping[[pattern]]
}

# Print renamed stack to verify
print(names(stk))

## Making tiles:

pred_n <- 6

# Define a mapping of keywords to output folders
output_folders <- list(
  ForestClim_12 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_12/",
  ForestClim_15 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_15/",
  ForestClim_05 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_05/",
  ForestClim_06 = "/lustre1/scratch/348/vsc34871/output/tiles/ForestClim_06/",
  cec = "/lustre1/scratch/348/vsc34871/output/tiles/cec/",
  clay = "/lustre1/scratch/348/vsc34871/output/tiles/clay/"
)

# Iterate through the layers in the stack
for (i in 1:pred_n) {
  pred_name <- names(stk[[i]])
  print(pred_name)
  
  # Check which keyword matches and create tiles accordingly
  for (key in names(output_folders)) {
    if (grepl(key, pred_name, ignore.case = TRUE)) {
      output_folder <- output_folders[[key]]
      makeTiles(
        stk[[i]], c_grid_spat,
        filename = paste0(output_folder, key, "_.tif"),
        overwrite = TRUE
      )
      break
    }
  }
}
library(terra)
library(sf)


# Read in shapefile in sf format.
c_shape <- read_sf(
  "/lustre1/scratch/348/vsc34871/SDM_current/Shapefiles/Europe.shp"
  )
c_shape <- st_buffer(c_shape, 1) # buffer by 1 meter to avoid potential issues
plot(c_shape)

# create an initial grid for centroid determination
c_grid <- st_make_grid(c_shape, cellsize = c(9e5, 9e5)) |>
  st_as_sf()
# inspect
plot(st_geometry(c_shape))
plot(st_geometry(c_grid), border = "red", add = TRUE)

c_grid_spat <- vect(c_grid)

predlist <- list.files(
  path = "/lustre1/scratch/348/vsc34871/SDM_fut/predictors/",
  pattern = ".tif$",
  all.files = T, full.names = T
)
stk <- terra::rast(predlist)
print(stk)
names(stk)

 # Define a mapping of old names (patterns) to new names
 name_mapping <- list(
   #"cec" = "cec",
   "bio12_2071_2100_ssp370"  = "CHELSA_bio12_EU_2000.2019",
   "bio15_2071_2100_ssp370"  = "CHELSA_bio15_EU_2000.2019",
   #"clay"  = "clay",
   #"elevation" = "Elevation",
   "bio5_2071_2100_ssp370" = "Micro_BIO5_EU_CHELSAbased_2000.2020",
   "bio6_2071_2100_ssp370" = "Micro_BIO6_EU_CHELSAbased_2000.2020"
   #"slope" = "Slope",
   #"TWI" = "TWI",
   #"phh2o_0_30cm_mean" = "phh2o_0_30_WeightedMean"
 )
# Note that these are data from future period 2071-2100 under ssp370 scenario,
# but to fit the SDMs models, we have to change the names to match with the model input data.

 # Loop through the layers and rename based on the mapping
 for (pattern in names(name_mapping)) {
   # Find layers that match the current pattern
   matching_indices <- grep(pattern, names(stk))

   # Rename the matched layers
   names(stk)[matching_indices] <- name_mapping[[pattern]]
 }

# Print renamed stack to verify
print(names(stk))
print(stk)

## Making tiles:
# Numbers of predictors.
pred_n <- 4

# Define a mapping of keywords to output folders
output_folders <- list(
  #cec = "/lustre1/scratch/348/vsc34871/SDM_current/pred_bigtiles/cec/",
  CHELSA_bio12_EU_2000.2019 = "/lustre1/scratch/348/vsc34871/SDM_fut/pred_tiles/CHELSA_bio12_EU_2000.2019/",
  CHELSA_bio15_EU_2000.2019 = "/lustre1/scratch/348/vsc34871/SDM_fut/pred_tiles/CHELSA_bio15_EU_2000.2019/",
  #clay = "/lustre1/scratch/348/vsc34871/SDM_current/pred_bigtiles/clay/",
  #Elevation = "/lustre1/scratch/348/vsc34871/SDM_current/pred_bigtiles/Elevation/",
  Micro_BIO5_EU_CHELSAbased_2000.2020 = "/lustre1/scratch/348/vsc34871/SDM_fut/pred_tiles/Micro_BIO5_EU_CHELSAbased_2000.2020/",
  Micro_BIO6_EU_CHELSAbased_2000.2020 = "/lustre1/scratch/348/vsc34871/SDM_fut/pred_tiles/Micro_BIO6_EU_CHELSAbased_2000.2020/"
  #Slope = "/lustre1/scratch/348/vsc34871/SDM_current/pred_bigtiles/Slope/",
  #TWI = "/lustre1/scratch/348/vsc34871/SDM_current/pred_bigtiles/TWI/",
  #phh2o_0_30_WeightedMean = "/lustre1/scratch/348/vsc34871/SDM_current/pred_bigtiles/phh2o_0_30_WeightedMean/"
)

# Iterate through the layers in the stack
for (i in 1:pred_n) {
  pred_name <- names(stk[[i]])
  print(pred_name)
  
  # Check which keyword matches and create tiles accordingly
  for (key in names(output_folders)) {
    if (grepl(key, pred_name, ignore.case = TRUE)) {
      output_folder <- output_folders[[key]]
      # write tile to a temp GeoTIFF
    
      # Write tiles to a temporary directory first.
      # 0) ensure you have a project‐local temp folder
      tmp_dir <- file.path("/lustre1/scratch/348/vsc34871/tile_temp/")
      if (!dir.exists(tmp_dir)) dir.create(tmp_dir, showWarnings = FALSE)
      # 1) write to a temp file inside that folder
      tmpfile <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
      message("Writing temp tile to: ", tmpfile)
      
      # Create tiles in the temp folder.
      out_paths <- makeTiles(
        stk[[i]], c_grid_spat,
        filename  = tmpfile,
        overwrite = TRUE
      )
      
      # 2) for each returned file, test & keep or discard
      for (j in seq_along(out_paths)) {
        tmp_path <- out_paths[j]
        r        <- terra::rast(tmp_path)
        mx       <- terra::global(r, fun="max", na.rm=TRUE)[1, ]
        
        if (!all(is.na(mx))) {
          # build the _final_ name yourself:
          #   base = your original variable name
          #   j    = the tile index from 1..n
          base     <- pred_name
          new_name <- paste0(base, "_", j, ".tif")
          dest     <- file.path(output_folder, new_name)
          file.copy(tmp_path, dest, overwrite = TRUE)
          message("  → kept: ", new_name)
        } else {
          ## empty → remove it
          file.remove(tmp_path)
          message("  → dropped empty tile ", j)
        }
      }
      
      # 4) clean up temp file & exit this key‑loop
      unlink(tmpfile)
      break
      
    
    }
  }
}

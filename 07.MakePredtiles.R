library(terra)
library(sf)


# Read in shapefile in sf format.
c_shape <- read_sf("I:/EUshap/Europe.shp")
plot(c_shape)

# create an initial grid for centroid determination
c_grid <- st_make_grid(c_shape, cellsize = c(9e5, 9e5)) |>
  st_as_sf()
# inspect
plot(st_geometry(c_shape))
plot(st_geometry(c_grid), border = "red", add = TRUE)

c_grid_spat <- vect(c_grid)

predlist <- list.files(
  path = "I:/SDMs_France/Predictors",
  pattern = ".tif$",
  all.files = T, full.names = T
)
stk <- terra::rast(predlist)
print(stk)
names(stk)

 # Define a mapping of old names (patterns) to new names
 name_mapping <- list(
   "cec" = "cec",
   "CHELSA_bioclim_Europe_2000_2019_12"  = "CHELSA_bio12_EU_2000.2019",
   "CHELSA_bioclim_Europe_2000_2019_15"  = "CHELSA_bio15_EU_2000.2019",
   "clay"  = "clay",
   "elevation" = "Elevation",
   "CHELSA_bioclim_Europe_2000_2019_5" = "Micro_BIO5_EU_CHELSAbased_2000.2020",
   "CHELSA_bioclim_Europe_2000_2019_6" = "Micro_BIO6_EU_CHELSAbased_2000.2020",
   "slope" = "Slope",
   "TWI" = "TWI",
   "phh2o_0_30cm_mean" = "phh2o_0_30_WeightedMean"
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
print(stk)

## Making tiles:
# Numbers of predictors.
pred_n <- 10

# Define a mapping of keywords to output folders
output_folders <- list(
  cec = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/cec/",
  CHELSA_bio12_EU_2000.2019 = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/CHELSA_bio12_EU_2000.2019/",
  CHELSA_bio15_EU_2000.2019 = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/CHELSA_bio15_EU_2000.2019/",
  clay = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/clay/",
  Elevation = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/Elevation/",
  Micro_BIO5_EU_CHELSAbased_2000.2020 = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/Micro_BIO5_EU_CHELSAbased_2000.2020/",
  Micro_BIO6_EU_CHELSAbased_2000.2020 = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/Micro_BIO6_EU_CHELSAbased_2000.2020/",
  Slope = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/Slope/",
  TWI = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/TWI/",
  phh2o_0_30_WeightedMean = "/lustre1/scratch/348/vsc34871/SDM_current/pred_tiles/phh2o_0_30_WeightedMean/"
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
      
      # # HPC: choose a tempdir you know is writable on the cluster:
      # scratch <- Sys.getenv("SCRATCH_DIR", tempdir())
      # tmpfile <- tempfile(tmpdir = scratch, fileext = ".tif")
      # message("Temp tile written to: ", tmpfile)
      
      # For lab PC.
      # 0) ensure you have a project‐local temp folder
      tmp_dir <- file.path("I:/tile_temp")
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

test <- rast("I:/SDMs_France/pred_tiles/Slope/Slope_1.tif
             ")
test2 <- rast("I:/SDMs_France/pred_tiles/Elevation/Elevation_1.tif")

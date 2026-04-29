library(terra)

#### Microrefugia Index based on richness ratio on all the forest specialist species (140 species) ####
# ---------------------------------------------------------
# Global terra / GDAL settings for HPC
# ---------------------------------------------------------
terra::terraOptions(
  memfrac = 0.6,
  progress = 1,
  threads = 1
)

Sys.setenv(
  GDAL_NUM_THREADS = "1",
  OMP_NUM_THREADS  = "1"
)

# # ---------------------------------------------------------
# # Input / output files
# # ---------------------------------------------------------
# infile  <- "I:/DATA/output/Beta_SDM/EU_richness_ratio.tif"
# outfile <- "I:/DATA/output/Beta_SDM/EU_richness_MI.tif"

# # If on HPC, replace with your scratch/output paths, e.g.
# # infile  <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU/EU_richness_ratio.tif"
# # outfile <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU/EU_richness_MI.tif"

# # ---------------------------------------------------------
# # Read raster
# # ---------------------------------------------------------
# r <- terra::rast(infile)

# print(r)

# # Optional: check min/max without loading all values into memory
# print(terra::global(r, c("min", "max"), na.rm = TRUE))

# # ---------------------------------------------------------
# # Rescale richness ratio to MI
# # Rule:
# #   ratio >= 0   -> 1
# #   ratio <= -1  -> 0
# #   otherwise    -> ratio + 1
# # ---------------------------------------------------------
# mi <- terra::ifel(
#   is.na(r), NA,
#   terra::ifel(
#     r >= 0, 1,
#     terra::ifel(
#       r <= -1, 0,
#       r + 1
#     )
#   )
# )

# print(mi)

# # ---------------------------------------------------------
# # Write output raster
# # ---------------------------------------------------------
# terra::writeRaster(
#   mi,
#   outfile,
#   overwrite = TRUE,
#   datatype = "FLT4S",
#   gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
# )

# # ---------------------------------------------------------
# # Quick check of output
# # ---------------------------------------------------------
# mi_check <- terra::rast(outfile)
# print(mi_check)
# print(terra::global(mi_check, c("min", "max"), na.rm = TRUE))

#### Microrefugia Index based on richness ratio on red list species. ####
# ---------------------------------------------------------
# Global terra / GDAL settings for HPC
# ---------------------------------------------------------
terra::terraOptions(
  memfrac = 0.6,
  progress = 1,
  threads = 1
)

Sys.setenv(
  GDAL_NUM_THREADS = "1",
  OMP_NUM_THREADS  = "1"
)

# ---------------------------------------------------------
# Input / output files
# ---------------------------------------------------------
infile  <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU_RedList/EU_richness_ratio.tif"
outfile <- "/lustre1/scratch/348/vsc34871/output/MicrorefugiaIndex_RedList/EU_richness_ratio_RedList_MI.tif"

# If on HPC, replace with your scratch/output paths, e.g.
# infile  <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU/EU_richness_ratio.tif"
# outfile <- "/lustre1/scratch/348/vsc34871/output/Merge_beta_EU/EU_richness_MI.tif"

# ---------------------------------------------------------
# Read raster
# ---------------------------------------------------------
r <- terra::rast(infile)

print(r)

# Optional: check min/max without loading all values into memory
print(terra::global(r, c("min", "max"), na.rm = TRUE))

# ---------------------------------------------------------
# Rescale richness ratio to MI
# Rule:
#   ratio >= 0   -> 1
#   ratio <= -1  -> 0
#   otherwise    -> ratio + 1
# ---------------------------------------------------------
mi <- terra::ifel(
  is.na(r), NA,
  terra::ifel(
    r >= 0, 1,
    terra::ifel(
      r <= -1, 0,
      r + 1
    )
  )
)

print(mi)

# ---------------------------------------------------------
# Write output raster
# ---------------------------------------------------------
terra::writeRaster(
  mi,
  outfile,
  overwrite = TRUE,
  datatype = "FLT4S",
  gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
)

# ---------------------------------------------------------
# Quick check of output
# ---------------------------------------------------------
mi_check <- terra::rast(outfile)
print(mi_check)
print(terra::global(mi_check, c("min", "max"), na.rm = TRUE))
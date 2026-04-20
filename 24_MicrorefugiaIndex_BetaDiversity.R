library(terra)

# ---------------------------------------------------------
# Optional HPC-safe settings
# ---------------------------------------------------------
terraOptions(
  memfrac = 0.8,
  progress = 1,
  threads = 1
)

Sys.setenv(
  GDAL_NUM_THREADS = "1",
  OMP_NUM_THREADS  = "1"
)

# ---------------------------------------------------------
# Input / output paths
# ---------------------------------------------------------
infile  <- "/lustre1/scratch/348/vsc34871/output/Merge_Beta_EU/EU_beta_Baselga2010_sor_sim_nes_named.tif"
outfile <- "/lustre1/scratch/348/vsc34871/output/MicrorefugiaIndex/MI_EU_beta_1_minus_beta.tif"

# ---------------------------------------------------------
# Read beta raster stack
# ---------------------------------------------------------
beta <- rast(infile)

print(beta)
print(global(beta, c("min", "max"), na.rm = TRUE))

# ---------------------------------------------------------
# Convert beta to MI = 1 - beta
# ---------------------------------------------------------
mi_beta <- 1 - beta

# Rename layers
names(mi_beta) <- paste0("MI_", names(beta))

print(mi_beta)
print(global(mi_beta, c("min", "max"), na.rm = TRUE))

# ---------------------------------------------------------
# Write output
# ---------------------------------------------------------
writeRaster(
  mi_beta,
  outfile,
  overwrite = TRUE,
  datatype = "FLT4S",
  gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
)
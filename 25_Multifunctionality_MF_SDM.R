library(terra)

# ------------------------------------------------------------
# 1. Input files
# ------------------------------------------------------------

mi_beta_file <- "/lustre1/scratch/348/vsc34871/output/MicrorefugiaIndex_140species/MI_EU_beta_1_minus_beta.tif"
mi_rich_file <- "/lustre1/scratch/348/vsc34871/output/MicrorefugiaIndex_140species/EU_richness_ratio_MI.tif"

out_dir <- "/lustre1/scratch/348/vsc34871/output/MF_SDMs/"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 2. Read raster data
# ------------------------------------------------------------

mi_beta <- rast(mi_beta_file)
mi_richness_ratio <- rast(mi_rich_file)

print(mi_beta)
print(mi_richness_ratio)

# ------------------------------------------------------------
# 3. Check geometry consistency
# ------------------------------------------------------------

same_geom <- compareGeom(
  mi_beta,
  mi_richness_ratio,
  stopOnError = FALSE
)

print(same_geom)

if (!same_geom) {
  stop("The beta MI raster and richness-ratio MI raster do not have the same geometry.")
}

# ------------------------------------------------------------
# 4. Combine all MI layers into one raster stack
# ------------------------------------------------------------

mi_stack <- c(mi_beta, mi_richness_ratio)

names(mi_stack) <- c(
  "MI_beta_sor",
  "MI_beta_sim",
  "MI_beta_nes",
  "MI_richness_ratio"
)

print(mi_stack)

# ------------------------------------------------------------
# 5. Define write options
# ------------------------------------------------------------

wopt_float <- list(
  datatype = "FLT4S",
  gdal = c(
    "COMPRESS=LZW",
    "TILED=YES",
    "BIGTIFF=YES"
  )
)

wopt_int <- list(
  datatype = "INT1U",
  gdal = c(
    "COMPRESS=LZW",
    "TILED=YES",
    "BIGTIFF=YES"
  )
)

# ------------------------------------------------------------
# 6. Averaging approach
# ------------------------------------------------------------

mf_average <- mean(
  mi_stack,
  na.rm = FALSE,
  filename = file.path(out_dir, "MF_average_4MIs.tif"),
  overwrite = TRUE,
  wopt = wopt_float
)

names(mf_average) <- "MF_average"

print(mf_average)

# ------------------------------------------------------------
# 7. Single-threshold approach: threshold = 0.8
# ------------------------------------------------------------

mf_threshold_08 <- (mi_stack > 0.8)
mf_threshold_08 <- sum(
  mf_threshold_08,
  na.rm = FALSE,
  filename = file.path(out_dir, "MF_threshold_count_08_4MIs.tif"),
  overwrite = TRUE,
  wopt = wopt_int
)

names(mf_threshold_08) <- "MF_threshold_count_08"

print(mf_threshold_08)
print(global(mf_threshold_08, c("min", "max"), na.rm = TRUE))

# ------------------------------------------------------------
# 8. Single-threshold approach: threshold = 0.6
# ------------------------------------------------------------

mf_threshold_06 <- (mi_stack > 0.6)
mf_threshold_06 <- sum(
  mf_threshold_06,
  na.rm = FALSE,
  filename = file.path(out_dir, "MF_threshold_count_06_4MIs.tif"),
  overwrite = TRUE,
  wopt = wopt_int
)

names(mf_threshold_06) <- "MF_threshold_count_06"

print(mf_threshold_06)
print(global(mf_threshold_06, c("min", "max"), na.rm = TRUE))
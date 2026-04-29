library(terra)

# beta_raster is your 3-layer SpatRaster
# layer order: 1 = beta_sor, 2 = beta_sim, 3 = beta_nes
beta_raster <- rast("N:/SDMs/Merge_beta_RedList/EU_beta_Baselga2010_sor_sim_nes.tif")

names(beta_raster) <- c("beta_sor", "beta_sim", "beta_nes")

writeRaster(
  beta_raster[[1]],
  "N:/SDMs/Merge_beta_RedList/EU_redlist_beta_sorensen.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

writeRaster(
  beta_raster[[2]],
  "N:/SDMs/Merge_beta_RedList/EU_redlist_beta_simpson_turnover.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

writeRaster(
  beta_raster[[3]],
  "N:/SDMs/Merge_beta_RedList/EU_redlist_beta_nestedness_loss.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

#############################
# Species loss, gain, shared:
#############################
eu_abc <- rast("N:/SDMs/Merge_Beta_EU/EU_abc_named.tif")
out_dir <- "N:/SDMs/Merge_Beta_EU"

writeRaster(
  eu_abc[["a_shared"]],
  filename = file.path(out_dir, "a_shared.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT2U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

writeRaster(
  eu_abc[["b_losses"]],
  filename = file.path(out_dir, "b_losses.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT2U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

writeRaster(
  eu_abc[["c_gains"]],
  filename = file.path(out_dir, "c_gains.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT2U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

#############################
#### Microrefugia index. ####
#############################
## Beta diversity
beta_mi <- rast("N:/SDMs/")

names(beta_raster) <- c("beta_sor", "beta_sim", "beta_nes")

writeRaster(
  beta_raster[[1]],
  "N:/SDMs/Merge_beta_RedList/EU_redlist_beta_sorensen.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

writeRaster(
  beta_raster[[2]],
  "N:/SDMs/Merge_beta_RedList/EU_redlist_beta_simpson_turnover.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

writeRaster(
  beta_raster[[3]],
  "N:/SDMs/Merge_beta_RedList/EU_redlist_beta_nestedness_loss.tif",
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)



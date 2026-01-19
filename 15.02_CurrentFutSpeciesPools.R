library(terra)

#### Change in size of species pools based on all species. ####
# 1) Align all current binary maps to common extent/resolution/CRS.
# --- compute union extent without stacking (cheap)
exts <- lapply(filelist_current.binarized, function(f) ext(rast(f)))
e_union <- Reduce(terra::union, exts)

# --- pick a reference raster for geometry (CRS/res/origin)
ref <- rast(filelist_current.binarized[1])

# template with union extent
tmpl <- ref
ext(tmpl) <- e_union

aligned_files <- vapply(seq_along(filelist_current.binarized), function(i) {
    x <- rast(filelist_current.binarized[i])
    # If it already matches the template fully, skip
    if (compareGeom(x, tmpl, stopOnError = FALSE)) {
      return(NA_character_)   # marker for "skipped"
    }
    
    # Otherwise, align
    if (!same.crs(x, tmpl)) {
      x <- project(x, tmpl, method = "near")
    } else {
      x <- resample(x, tmpl, method = "near")
    }
    
    # Safety check
    if (!compareGeom(x, tmpl, stopOnError = FALSE)) {
      stop(sprintf("Still mismatched after alignment: %s", p))
    }
    
    out <- file.path(
        "G:/SDMs/SDMs_current/Results/AlignedBinaryMaps/",
        paste0("aligned_", basename(filelist_current.binarized[i]))
    )
    writeRaster(x, out, overwrite = TRUE)
    out
}, character(1))

# To align the first 4 species with the rest of the common extent in the current period, 
# in this case, the rasters that have different extent appears in the first
# four species.
align01.lst <- list.files(
  path = "G:/SDMs/SDMs_current/Results/AlignedBinaryMaps/",
  pattern = "\\.tif$",
  full.names = TRUE
)
# Align the first five binary maps with new extent.
aligned_files02 <- vapply(seq_along(align01.lst), function(i) {
  x <- rast(align01.lst[i])
  # If it already matches the template fully, skip
  if (compareGeom(x, tmpl, stopOnError = FALSE)) {
    return(NA_character_)   # marker for "skipped"
  }
  
  # Otherwise, align
  if (!same.crs(x, tmpl)) {
    x <- project(x, tmpl, method = "near")
  } else {
    x <- resample(x, tmpl, method = "near")
  }
  
  # Safety check
  if (!compareGeom(x, tmpl, stopOnError = FALSE)) {
    stop(sprintf("Still mismatched after alignment: %s", p))
  }
  
  out <- file.path(
    "G:/SDMs/SDMs_current/Results/AlignedBinaryMaps/",
    paste0("aligned02_", basename(align01.lst[i]))
  )
  writeRaster(x, out, overwrite = TRUE)
  out
}, character(1))

## Test if all rasters can be stacked now. ##
align_binarized <- list.files(
  path = "G:/SDMs/SDMs_current/Results/Binary_SpeciesCurrentAtualDistri/",
  pattern = "\\.tif$",
  full.names = TRUE
)
align_binarized <- rast(align_binarized)

# 2) Stack and sum cell-wise, ignoring NA
sum_r <- app(align_binarized, sum, na.rm = TRUE)

writeRaster(
  sum_r,
  filename = "G:/SDMs/SDMs_current/Results/SpeciesPool_current_140species.tif",
  overwrite = TRUE,
  datatype = "INT1U",  # unsigned 8-bit: exact for 0..255
  wopt = list(
    gdal = c(
      "COMPRESS=DEFLATE",   # lossless compression (often very good)
      "PREDICTOR=2",        # helps for integer rasters
      "ZLEVEL=9",           # max compression (slower write, smaller file)
      "TILED=YES",          # better I/O for huge rasters
      "BLOCKXSIZE=512",
      "BLOCKYSIZE=512",
      "BIGTIFF=IF_SAFER"    # allows >4GB automatically if needed
    )
  )
)

#### Future species pool size.####
#### Test if all rasters can be stacked now. ####
align_binarized_fut <- list.files(
  path = "G:/SDMs/SDMs_future/Results/BinaryMaps_original/",
  pattern = "\\.tif$",
  full.names = TRUE
)
align_binarized_fut <- rast(align_binarized_fut)

# 2) Stack and sum cell-wise, ignoring NA
sum_r_fut <- app(align_binarized_fut, sum, na.rm = TRUE)

writeRaster(
  sum_r,
  filename = "SpeciesPool_2071-2100_140SDMs.tif",
  overwrite = TRUE,
  datatype = "INT1U",  # unsigned 8-bit: exact for 0..255
  wopt = list(
    gdal = c(
      "COMPRESS=DEFLATE",   # lossless compression (often very good)
      "PREDICTOR=2",        # helps for integer rasters
      "ZLEVEL=9",           # max compression (slower write, smaller file)
      "TILED=YES",          # better I/O for huge rasters
      "BLOCKXSIZE=512",
      "BLOCKYSIZE=512",
      "BIGTIFF=IF_SAFER"    # allows >4GB automatically if needed
    )
  )
)

#### For RedList species only. ####
rl_species <- read.csv(
  header = TRUE,
  "I:/RedListSpecies/RL_forest_species.csv",
   sep = ";",
   row.names = 1)
head(rl_species)

## Select CR, EN, VU, R and NT species. ##
rl_selected <- rl_species[
  rl_species$median_redlist_text %in% c("CR", "EN", "VU", "R", "NT"),
  ]
head(rl_selected)

## Calculate the species pool for the selected RedList species only. ##
rl_species_names <- rl_selected$Species
rl_species_names <- gsub(" ", "_", rl_species_names)

ras_dir <- "G:/SDMs/SDMs_current/Results/Binary_SpeciesCurrentAtualDistri/"
ras_files <- list.files(
  ras_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

matched_rasters <- ras_files[
  vapply(
    ras_files,
    function(f) {
      any(grepl(rl_species_names, basename(f), fixed = TRUE))
    },
    logical(1)
  )
]

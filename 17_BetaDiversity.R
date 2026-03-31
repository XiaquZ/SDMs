library(terra)
# ---- inputs ----
cur_dir <- "I:/DATA/SDM_current/results/Binary_SpeciesCurrentAtualDistri_be/"
fut_dir <- "I:/DATA/SDM_future/Binary_SpeciesFutReachable_be_test/"
out_dir <- "I:/DATA/beta_temp"
cores   <- 3

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
#dir.create(tmpdir,  recursive = TRUE, showWarnings = FALSE)

terraOptions(memfrac = 0.8, progress = 10)
Sys.setenv(GDAL_NUM_THREADS = "1")

# ---- files ----
cur_files <- list.files(cur_dir, pattern = "\\.tif$", full.names = TRUE)
fut_files <- list.files(fut_dir, pattern = "\\.tif$", full.names = TRUE)

# ---- load stacks ----
C <- rast(cur_files)
F <- rast(fut_files)

# ---- richness function ----
rich_fun <- function(v) {
  # If entire pixel is NA (non-forest)
  if (all(is.na(v))) return(NA_real_)
  
  # Otherwise count presences
  sum(v == 1, na.rm = TRUE)
}

# ---- current richness ----
rich_cur <- app(
  C, rich_fun,
  cores = cores,
  filename = file.path(out_dir, "richness_current.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT2U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)

# ---- future richness ----
rich_fut <- app(
  F, rich_fun,
  cores = cores,
  filename = file.path(out_dir, "richness_future.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT2U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")
  )
)



## Speedup the process.##
dir.create("D:/PhD/Data/terra_tmp", showWarnings = FALSE)
dir.create("D:/PhD/Data/beta_tmp", showWarnings = FALSE)

src_to_species <- function(x) {
  x <- basename(x)
  x <- sub("\\.tif$", "", x, ignore.case = TRUE)
  x <- sub("^binary[_ ]+", "", x, ignore.case = TRUE)
  x <- sub("_CurrentActual$", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+", "_", x)
  x
}


names(cur) <- src_to_species(sources(cur))
names(fut) <- src_to_species(sources(fut))

if (!setequal(names(cur), names(fut))) stop("Species sets differ.")
fut <- fut[[names(cur)]]
stopifnot(identical(names(cur), names(fut)))

# Convert float to byte
cur_file <- "D:/PhD/Data/beta_tmp/curActualBinary_140_INT1U.tif"
fut_file <- "D:/PhD/Data/beta_tmp/futTestBinary_140_INT1U.tif"

writeRaster(cur, cur_file, overwrite = TRUE,
            wopt = list(datatype = "INT1U"),
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))

writeRaster(fut, fut_file, overwrite = TRUE,
            wopt = list(datatype = "INT1U"),
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "BIGTIFF=YES"))


# Check if species are in the same order in both raster stacks.
names(cur)[[1]]
names(fut)[[1]]
if (!identical(names(cur), names(fut))) {
  stop("Layer names of current and future stacks do not match exactly.")
}

stopifnot(nlyr(cur) == nlyr(fut))
if (!compareGeom(cur, fut, stopOnError = FALSE)) {
  stop("Current and future stacks are not aligned (extent/resolution/CRS). Align them first.")
}

# Optional but recommended: enforce same layer names/order
# names(cur) <- species_names
# names(fut) <- species_names
stopifnot(all(names(cur) == names(fut)))

terraOptions(memfrac = 0.6, progress = 1)  # adjust memfrac if needed

a <- sum(cur == 1 & fut == 1)              # persisted
b <- sum(cur == 1 & fut == 0)              # losses
c <- sum(cur == 0 & fut == 1)              # gains

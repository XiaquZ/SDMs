library(terra)
library(sf)

terraOptions(memfrac = 0.8, progress = 1, threads = 1)

eu_shape <- read_sf("/lustre1/scratch/348/vsc34871/EUshap/Europe.shp")
eu_shape <- st_buffer(eu_shape, 1)

cellsize <- 9e5   # 900 km
grid <- st_make_grid(eu_shape, cellsize = c(cellsize, cellsize)) |>
  st_as_sf()

grid_vect <- vect(grid)

# -----------------------------
# Load raster files
# -----------------------------
cur_files <- list.files(
  "/lustre1/scratch/348/vsc34871/Binary_curActual/",
  pattern = "\\.tif$",
  full.names = TRUE
)

fut_files <- list.files(
  "/lustre1/scratch/348/vsc34871/Binary_futActual/",
  pattern = "\\.tif$", 
  full.names = TRUE
)

to_species_key <- function(x) {
  x <- basename(x)
  x <- sub("\\.tif$", "", x, ignore.case = TRUE)
  x <- sub("^binary[_ ]+", "", x, ignore.case = TRUE)
  x <- sub("(_CurrentActual|_CurrentAtual|_Current)$", "", x, ignore.case = TRUE)
  x <- sub("(_FuturePotentialReachable|_FuturePotential|_FutureReachable|_Future)$", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

cur_key <- to_species_key(cur_files)
fut_key <- to_species_key(fut_files)

if (!setequal(cur_key, fut_key)) {
  stop(
    "Species sets differ.\n",
    "Missing in future: ", paste(setdiff(cur_key, fut_key), collapse = ", "), "\n",
    "Missing in current: ", paste(setdiff(fut_key, cur_key), collapse = ", ")
  )
}

# reorder future to current species order
fut_files <- fut_files[match(cur_key, fut_key)]

cur_stack <- rast(cur_files)
fut_stack <- rast(fut_files)

if (!compareGeom(cur_stack, fut_stack, stopOnError = FALSE)) {
  stop("Current and future stacks do not have identical geometry.")
}

# -----------------------------
# Output folders
# -----------------------------
out_dir_cur <- "/lustre1/scratch/348/vsc34871/output/Binary_CurrentActual_tiles/"
out_dir_fut <- "/lustre1/scratch/348/vsc34871/output/Binary_FutureReachable_tiles/"

dir.create(out_dir_cur, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_fut, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Make tiles
# -----------------------------
tile_files_cur <- makeTiles(
  cur_stack,
  grid_vect,
  filename = file.path(out_dir_cur, "tile_current.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )
)

tile_files_fut <- makeTiles(
  fut_stack,
  grid_vect,
  filename = file.path(out_dir_fut, "tile_future.tif"),
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",
    gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
  )
)

# -----------------------------
# Remove only jointly empty tiles
# -----------------------------
get_id <- function(x) sub("^.*?(\\d+)\\.tif$", "\\1", basename(x))

cur_ids <- get_id(tile_files_cur)
fut_ids <- get_id(tile_files_fut)

if (!setequal(cur_ids, fut_ids)) {
  stop("Current and future tile IDs do not match after tiling.")
}

tile_files_fut <- tile_files_fut[match(cur_ids, fut_ids)]

is_all_na_tile <- function(f) {
  r <- rast(f)
  mx <- global(r, "max", na.rm = TRUE)[1, ]
  all(is.na(mx))
}

for (i in seq_along(tile_files_cur)) {
  fcur <- tile_files_cur[i]
  ffut <- tile_files_fut[i]

  cur_empty <- is_all_na_tile(fcur)
  fut_empty <- is_all_na_tile(ffut)

  if (cur_empty && fut_empty) {
    file.remove(fcur)
    file.remove(ffut)
    message("Removed jointly empty tile pair: ",
            basename(fcur), " and ", basename(ffut))
  }
}

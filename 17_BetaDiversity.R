library(terra)
## Raw data and plotting
cur <- list.files(
  path = "I:\\SDM_betaDiver\\Binary_be_CurrentActu\\",
  pattern = "\\.tif$",
  full.names = TRUE
)
cur <- rast(cur)

fut <- list.files(
  path = "I:\\SDM_betaDiver\\Binary_be_FutureReacha\\",
  pattern = "\\.tif$",
  full.names = TRUE
)
fut <- rast(fut)

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

## Temporal beta diversity maps
den_jac <- a + b + c
beta_jac <- (b + c) / den_jac
beta_jac <- classify(beta_jac, rcl = matrix(c(-Inf,0,NA), ncol=3, byrow=TRUE))  # optional: NA where den=0

beta_sor <- (b + c) / (2*a + b + c)
beta_sor <- classify(beta_sor, rcl = matrix(c(-Inf,0,NA), ncol=3, byrow=TRUE))  # optional

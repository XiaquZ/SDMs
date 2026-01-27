library(terra)
## Raw data and plotting
cur <- list.files(
  path = "G:/SDMs/SDMs_current/Results/Binary_SpeciesCurrentAtualDistri/",
  pattern = "\\.tif$",
  full.names = TRUE
)
cur <- rast(cur)

fut <- list.files(
  path = "G:/SDMs/SDMs_future/Results/BinaryMaps_original/",
  pattern = "\\.tif$",
  full.names = TRUE
)
fut <- rast(fut)

# Check if species are in the same order in both raster stacks.
names(cur)[[130]]
names(fut)[[130]]
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

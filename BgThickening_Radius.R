# Load required packages
library(sf)              # for spatial operations
library(terra)           # for raster data
library(gstat)           # for variograms and autocorrelation
library(spatstat.geom)   # for spatial point operations
library(dplyr)           # for data manipulation

# -----------------------
# STEP 1: Load data
# -----------------------
# Species presence data (make it an sf object)
species_sf <- st_as_sf(species_df,
                       coords = c("Longitude", "Latitude"),
                       crs = st_crs(covariates_pred))  # use CRS from covariates_pred

# Environmental covariates (SpatRaster)
env_stack <- covariates_pred  # already loaded

# -----------------------
# STEP 2: Compute thickening radius
# -----------------------
# Calculate spatial autocorrelation range for each variable
ranges <- list()
for (i in nlyr(env_stack)) {
  var_raster <- env_stack[[i]]
  # Randomly sample 5000 points (or fewer if raster is small)
  set.seed(16)
  sample_pts <- spatSample(var_raster, size = 5000,
                           method = "random", as.points = TRUE,
                           na.rm = TRUE, exhaustive = TRUE)
  if (is.null(sample_pts)) next  # skip if no points

  coords <- crds(sample_pts)
  values <- values(sample_pts)[,1]
  var_data <- data.frame(x = coords[,1], y = coords[,2], value = values)

  # Create gstat object
  g <- gstat(id = "value", formula = value ~ 1, locations = ~x+y, data = var_data)
  vgm_model <- variogram(g)

  # Fit a variogram model
  fit <- tryCatch({
    fit.variogram(vgm_model, vgm("Sph"))
  }, error = function(e) NA)

  # Extract range or fallback
  if (!is.null(fit) && "range" %in% names(fit)) {
    # Take range value from the second row (the fitted model parameters)
    range_value <- fit$range[2]
  } else {
    range_value <- max(vgm_model$dist, na.rm = TRUE)  # fallback
  }

  ranges[[names(env_stack)[i]]] <- range_value
}

# Mean autocorrelation range (thickening radius)
thickening_radius <- mean(unlist(ranges))
print(paste("Thickening radius:", thickening_radius, "meters"))
# [1] "Thickening radius: 1218104.06222577 meters (including topography variables)"
# [1] "Thickening radius: 1681805.24764157 meters"

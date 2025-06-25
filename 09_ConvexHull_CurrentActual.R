################################################################################
# BULK CONVEX-HULL EXPORTER · terra + base R                                  #
# --------------------------------------------------------------------------- #
#  Assumptions                                                                #
#    • Each CSV is named "<Species name>.csv" and lives in one folder         #
#    • CSV columns are:   Species | Longitude | Latitude                      #
#    • Longitude / Latitude are in WGS-84 (EPSG:4326)                         #
#    • Output CRS is Europe ETRS89 / LAEA (EPSG:3035)                         #
################################################################################

library(terra)      # v1.7–x or later
library(tools)      # for file_path_sans_ext()
library(dplyr)      # for data manipulation

## ─── 1. CONFIGURE YOUR FOLDERS ────────────────────────────────────────────────
in_dir  <- "I:/DATA/Occurrences_cleaned"           # where the CSVs live
out_dir <- "I:/DATA/output/ConvexHull"             # where shapefiles go
eu_shp  <- "I:/EUshap/Europe.shp"                  # (optional) EU coastlines


# Create output folder if it doesn't exist
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ─── 2. (OPTIONAL) READ A BASEMAP FOR QC PLOTS ───────────────────────────────
eu <- try(vect(eu_shp), silent = TRUE)  # will be used only if it loads OK

###############################
##### Test on one species.#####
###############################
occ <- read.csv("I:/DATA/Occurrences_cleaned/Actaea spicata.csv", header = TRUE)
head(occ)
# 1. Convert occurrence data frame to SpatVector
occ_vect <- vect(occ, geom = c("Longitude", "Latitude"), crs = "EPSG:3035")

# 2. Plot the shapefile
plot(
  eu,
  col = "lightgrey", border = "darkgrey",
  main = "Actaea spicata Occurrences in Europe"
)

# 3. Add the occurrence points
points(occ_vect, col = "red", pch = 20, cex = 0.7)

# minimum convex polygon method
conh <- hull(occ_vect, "concave_ratio", param = 0.7)
# 4. Add convex hull (calibration area)
lines(conh, col = "blue", lwd = 2)



## ─── 3. LIST ALL CSV FILES ───────────────────────────────────────────────────
csvs <- list.files(in_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(csvs) == 0) stop("No CSV files found in ", in_dir)

## ─── 4. MAIN LOOP ────────────────────────────────────────────────────────────
for (csv in csvs) {
  cat("• Processing", basename(csv), "...\n")
  
  ## 4a. Read occurrences ------------------------------------------------------
  occ <- read.csv(csv, header = TRUE, stringsAsFactors = FALSE)
  
  ## Quick sanity checks
  ok_cols <- c("Longitude", "Latitude")
  if (!all(ok_cols %in% names(occ))) {
    warning("  > Skipped (missing Lon/Lat columns): ", basename(csv))
    next
  }
  occ <- na.omit(occ[, ok_cols])               # drop rows with missing coords
  occ <- unique(occ)                           # remove duplicates
  
  if (nrow(occ) < 3) {                         # need ≥3 points for a polygon
    warning("  > Skipped (fewer than 3 unique points): ", basename(csv))
    next
  }
  
  ## 4b. Convert to SpatVector, project to EPSG:3035 ---------------------------
  pts <- vect(occ, geom = c("Longitude", "Latitude"), crs = "EPSG:3035")
  
  
  ## 4c. Build the hull --------------------------------------------------------
  # Choose one of the two lines below:
  hull_poly <- convHull(pts)                     # classic convex hull         #
  # hull_poly <- hull(pts, type = "concave", alpha = 1)   # tighter concave hull
  
  ## 4d. Write shapefile -------------------------------------------------------
  sp_name <- gsub("\\s+", "_", file_path_sans_ext(basename(csv)))  # clean name
  out_file <- file.path(out_dir, paste0(sp_name, "_ConvexHull.shp"))
  
  writeVector(hull_poly, out_file, overwrite = TRUE)
  cat("  ✔ saved to", out_file, "\n")
  
}

cat("\nDone! All hulls exported to: ", out_dir, "\n")

#### Check the output by plotting one of the saved convex hulls.####
# 1. Paths
shp_dir <- "I:/DATA/output/ConvexHull"
csv_dir <- "I:/DATA/Occurrences_cleaned"
eu  <- vect("I:/EUshap/Europe.shp")  

# 3. List shapefiles
shapefiles <- list.files(shp_dir, pattern = "_ConvexHull\\.shp$", full.names = TRUE)

# 4. Loop through shapefiles
for (shp_path in shapefiles) {
  # Extract species name (used to find matching CSV)
  shp_file <- basename(shp_path)
  species_name <- gsub("_", " ", file_path_sans_ext(gsub("_ConvexHull", "", shp_file)))
  csv_file <- file.path(csv_dir, paste0(species_name, ".csv"))

  # Check if corresponding CSV exists
  if (!file.exists(csv_file)) {
    warning("CSV not found for ", species_name)
    next
  }

  cat("\nPlotting:", species_name, "\n")

  # Load data
  occ <- read.csv(csv_file, header = TRUE)
  if (!all(c("Longitude", "Latitude") %in% names(occ))) {
    warning("Missing coordinates in: ", species_name)
    next
  }

  occ_vect <- vect(occ, geom = c("Longitude", "Latitude"), crs = "EPSG:3035")  
  hull <- vect(shp_path)

  # Plot
  plot(eu, col = "lightgrey", border = "darkgrey",
       main = paste("Occurrences and Hull for:", species_name))
  points(occ_vect, col = "red", pch = 20, cex = 0.7)
  lines(hull, col = "blue", lwd = 2)

  # Pause for user input before showing next plot
  cat("Press [Enter] to continue to the next species...")
  readline()
}

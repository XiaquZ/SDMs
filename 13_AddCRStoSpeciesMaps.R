# Packages
library(terra)

# 1) Set your folder with the .tif files
dir_path <- "D:/SDMs/SDMs_future/Results/Species_final"   # <-- change if needed

# 2) List .tif files (non-recursive; set recursive=TRUE if you want subfolders too)
tifs <- list.files(dir_path, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)

if (length(tifs) == 0) {
  stop("No .tif files found in the specified directory.")
}

# 3) Check and assign CRS if missing (no reprojection)
report <- lapply(tifs, function(f) {
  r <- rast(f)
  old_crs <- crs(r)               # empty string "" means missing CRS in terra
  
  if (!nzchar(old_crs)) {
    # Assign the known CRS (no reprojection)
    crs(r) <- "EPSG:3035"
    
    #If you prefer saving a new file instead of overwriting, use:
    out <- sub("\\.tif$", "_future.tif", f, ignore.case = TRUE)
    writeRaster(r, out, overwrite = TRUE)
  }
})


rtest <- rast("D:/SDMs/SDMs_future/Results/Species_final/Asperula taurina_3035_future.tif")

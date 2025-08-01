#### Loop through each speceis tile folder to get final map. ####
library(foreach)
library(doParallel)
library(terra)

# Define paths
input_dir <- "F:/Output/fut_SDMs/species_tiles/"
output_dir <- "F:/Output/fut_SDMs/species_final/"

# extract species names from the file names.
folder_path <- "D:/SDMs/SDMs_current/Occurrences_done"

# List all .csv files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = FALSE)

# Remove the '.csv' extension to extract species names
species_names <- tools::file_path_sans_ext(csv_files)

# List species tiles
species_tiles <- list.files(input_dir, full.names = TRUE)
species_names <- unique(basename(species_tiles))

# Set up parallel backend
# For PC:
cl <- makeCluster(detectCores() - 6)
registerDoParallel(cl)

# Parallel processing
foreach(
  species = species_names,
  .packages = "terra",
  .errorhandling = "pass"  # Continue on error, but log it
) %dopar% {
  tryCatch({
    # Read in the paths of all SDM tiles.
    # Filter tiles for the current species
    t.list <- list.files(
      paste0(input_dir, species), 
      pattern = ".tif", full.names = T
    )
    
    # Merge and process raster
    r <- vrt(t.list)
    r <- round(r, digits = 1)
    
    # Define output file path
    fout <- file.path(paste0(output_dir,species, "_predictedSDMs_2071-2100_ssp370_25m.tif"))
    
    # Write the raster
    writeRaster(r, fout, overwrite = TRUE)
    
    # Log success
    message("Successfully processed: ", species)
  }, error = function(e) {
    # Log error
    message("Error processing ", species, ": ", e$message)
  })
}

# Stop the cluster
stopCluster(cl)

# Final message
message("All species processed.")

# test
species <- "Brachypodium_sylvaticum" 

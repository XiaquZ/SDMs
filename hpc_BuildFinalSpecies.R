#### Loop through each speceis tile folder to get final map. ####
library(foreach)
library(doParallel)
library(terra)

# Define paths
input_dir <- "/lustre1/scratch/348/vsc34871/input/species_tiles/"
output_dir <- "/lustre1/scratch/348/vsc34871/output/species_final/"

# List species tiles
species_tiles <- list.files(input_dir, full.names = TRUE)
species_names <- unique(basename(species_tiles))

# Set up parallel backend
# For HPC:
cl <- makeCluster(6)

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


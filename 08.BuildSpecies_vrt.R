library(terra)

# Read in the paths of all SDM tiles.
out <- list.files("E:/Output/SDM_test/belgium/out/",
    pattern = ".tif", full.names = T
)

# Get species names.
mdl_paths <- list.files("E:/SDMs/Stef_SDMs/Models/", full.names = T)
species_names <- gsub(".RData", "", basename(mdl_paths))

# Output path for the final speceis distribution map.
fout <- "/lustre1/scratch/348/vsc34871/output/FVoCC_20kmSR_25m_2010-2085_SSP370.tif"

# Loop through all the species.
for (species in species_names) {
    t.lst <- out[grepl(species, basename(out))]
    r <- vrt(t.lst)
    fout <- paste0("E:/Output/SDM_test/belgium/out_species/",
     species, "_futSDM.tif")
    writeRaster(r, fout, overwrite = TRUE)
}
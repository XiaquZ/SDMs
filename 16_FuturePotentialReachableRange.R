library(terra)

# 1. Paths
shp_dir <- "I:/DATA/output/ConvexHull"
out_dir <- "I:/DATA/output/ConvexHull_75kmBuffer"

# 2. List shapefiles
shp_lst <- list.files(
    shp_dir,
    pattern = "_ConvexHull\\.shp$",
    full.names = TRUE
)
shp <- shp_lst[1]
for (shp in shp_lst) {
    species <- sub("_ConvexHull\\.shp$", "", basename(shp))
    cat("• Processing", species, "...\n")
    conh_poly <- vect(shp)
    conh_buffer <- buffer(conh_poly, 75000)
    plot(conh_poly, main = species)
    plot(conh_buffer,
        add = TRUE,
        main = paste0(species, " with 75km buffer")
    )
    out_file <- file.path(
        out_dir, paste0(species, "_ConvexHull_75kmBuffer.shp")
        )
    writeVector(
        conh_buffer,
         out_file,
          overwrite = TRUE)
}


#############################################
#### Species-specific dispersal distance ####
#############################################
dispersal_classes <- read.csv("I:/DATA/Lososova_et_al_2023_Dispersal_version2.xlsx")





# Inputs per species: # occ2010    : SpatRaster, binary (0/1) current occupied (or thresholded) for 2010 
# suit2085   : SpatRaster, continuous suitability (0-1) for 2085 
# forest     : SpatRaster, binary (1 = forest, 0 = non-forest), aligned to same grid 
# rate_m_yr  : numeric (m/year) 
# thr        : suitability threshold to binarize (choose your rule)
T_years <- 75
D <- rate_m_yr * T_years
# Enforce "non-forest cannot be occupied"occ2010_f <- (occ2010 == 1) & (forest == 1)
# 2085 habitat that can be occupiedhab2085_f <- (suit2085 >= thr) & (forest == 1)# Dispersal reachability from 2010 occupied (Euclidean distance)dist_to_source <- distance(occ2010_f)
reachable <- dist_to_source <= D
# Final 2085 occupancy under dispersal limitationocc2085_disp <- (hab2085_f & reachable) * 1 
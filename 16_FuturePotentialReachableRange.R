library(terra)
library(readxl)

# 1. Paths
shp_dir <- "I:/DATA/output/ConvexHull"
out_dir <- "I:/DATA/output/ConvexHull_Buffer"
dispersal_df <- read_excel(
    "I:\\DATA\\forestSpecialist_Maturity_DispersalRate.xlsx"
)
plot(dispersal_df$'Dispersal rate(m/year)')
# 2. List shapefiles
shp_lst <- list.files(
    shp_dir,
    pattern = "_ConvexHull\\.shp$",
    full.names = TRUE
)
min(dispersal_df$'Dispersal rate(m/year)', na.rm = TRUE)
anyNA(dispersal_df$'Dispersal rate(m/year)')
shp <- shp_lst[46]
for (shp in shp_lst) {
    species <- sub("_ConvexHull\\.shp$", "", basename(shp))
    species <- gsub("_", " ", species)
    cat("• Processing", species, "...\n")
    conh_poly <- vect(shp)
    t_year <- 75
    # extract dispersal rate for this species
    dispersal_rate <- dispersal_df$'Dispersal rate(m/year)'[
        dispersal_df$species.name == species
    ]
    dispersal_rate <- round(dispersal_rate, digits = 2)

    if (length(dispersal_rate) == 0 || is.na(dispersal_rate)) {
        warning(paste("No dispersal rate found for", species, "- skipping"))
        next
    }
    distance_buff <- dispersal_rate * t_year
    conh_buffer <- buffer(conh_poly, distance_buff)
    plot(conh_poly, main = species)
    plot(conh_buffer,
        add = TRUE,
        main = paste0(species, " with ", distance_buff, "m buffer")
    )
    out_file <- file.path(
        out_dir, paste0(species, "_ConvexHull", "Buffer.shp")
    )
    writeVector(
        conh_buffer,
        out_file,
        overwrite = TRUE
    )
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
# 2085 habitat that can be occupied
# hab2085_f <- (suit2085 >= thr) & (forest == 1)# Dispersal reachability from 2010 occupied (Euclidean distance)dist_to_source <- distance(occ2010_f)
reachable <- dist_to_source <= D
# Final 2085 occupancy under dispersal limitation
# occ2085_disp <- (hab2085_f & reachable) * 1

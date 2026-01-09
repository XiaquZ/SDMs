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

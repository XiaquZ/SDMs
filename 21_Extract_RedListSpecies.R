# =========================================================
# Copy selected species .tif files into a new folder
# based on species names listed in an Excel sheet
# =========================================================

# Install packages if needed
# install.packages("readxl")

library(readxl)

# -----------------------------
# 1. Define paths
# -----------------------------
tif_dir <- "N:/SDMs/SDMs_future/Results/Binary_FuturePotentialRechableDist/"
excel_file <- "I:/DATA/TableS2_Red_list_species.xlsx"
output_dir <- "N:/SDMs/SDMs_future/Results/Binary_FutureReachable_RedList/"

# Create output folder if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------
# 2. Read species names from Excel
#    Assumes species names are in the first column
# -----------------------------
red_list_df <- read_excel(excel_file)

species_list <- red_list_df[[1]]
species_list <- as.character(species_list)
species_list <- trimws(species_list)
species_list <- species_list[!is.na(species_list) & species_list != ""]

# Optional: remove duplicates
species_list <- unique(species_list)


# -----------------------------
# 3. List all .tif files in folder
# -----------------------------
tif_files <- list.files(
  tif_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# -----------------------------
# 4. Extract species names from filenames
#    Example:
#    "Aegonychon_purpurocaeruleum_CurrentActual.tif"
#    -> "Aegonychon purpurocaeruleum"
# -----------------------------
get_species_name_from_tif <- function(x) {
  nm <- basename(x)
  nm <- sub("\\.tif$", "", nm, ignore.case = TRUE)
  
  # remove suffix like "_FuturePotentialReachable"
  nm <- sub("_FuturePotentialReachable$", "", nm, ignore.case = TRUE)
  
  # replace underscores with spaces
  nm <- gsub("_", " ", nm)
  
  # trim spaces
  nm <- trimws(nm)
  
  nm
}

tif_species_names <- sapply(tif_files, get_species_name_from_tif)

# -----------------------------
# 5. Match species from Excel to tif files
# -----------------------------
matched_idx <- tif_species_names %in% species_list
matched_files <- tif_files[matched_idx]

# -----------------------------
# 6. Copy matched files to new folder
# -----------------------------
file.copy(
  from = matched_files,
  to = file.path(output_dir, basename(matched_files)),
  overwrite = FALSE
)

# -----------------------------
# 7. Report results
# -----------------------------
cat("Total species in Excel:", length(species_list), "\n")
cat("Total .tif files found:", length(tif_files), "\n")
cat("Matched .tif files copied:", length(matched_files), "\n")

# Species in Excel that were NOT found as tif
not_found <- setdiff(species_list, tif_species_names)

if (length(not_found) > 0) {
  cat("\nSpecies in Excel but no matching .tif found:\n")
  print(not_found)
} else {
  cat("\nAll species in Excel were matched to .tif files.\n")
}
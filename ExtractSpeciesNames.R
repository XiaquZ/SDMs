# Load required package
library(openxlsx)  # install.packages("openxlsx") if not already installed

# Define the folder path
folder_path <- "D:/SDMs/SDMs_current/Occurrences_done"

# List all .csv files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = FALSE)

# Remove the '.csv' extension to extract species names
species_names <- tools::file_path_sans_ext(csv_files)

# Create a data frame
species_df <- data.frame(Species_Name = species_names)

# Define output Excel file path
output_file <- file.path("I:/DATA", "species_names_SDMs.xlsx")

# Write to Excel
write.xlsx(species_df, file = output_file, rowNames = FALSE)

cat("Species names saved to:", output_file, "\n")
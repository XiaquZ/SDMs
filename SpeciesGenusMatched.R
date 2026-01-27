library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Read files.
dispersal <- read_excel("I:/DATA/Lososova_et_al_2023_Dispersal_version2.xlsx")
forest_sp <- read_excel("I:/DATA/Comparing species .xlsx")

# normalize species names for matching
forest_sp2 <- forest_sp %>%
  mutate(species_key = str_squish(`species name`))

dispersal2 <- dispersal %>%
  mutate(taxon_key = str_squish(Taxon)) %>%
  select(taxon_key, Genus)

  # join genus
forest_sp_out <- forest_sp2 %>%
  left_join(dispersal2, by = c("species_key" = "taxon_key")) %>%
  select(-species_key)

forest_sp_clean <- forest_sp_out %>%
  mutate(Genus = Genus.y) %>%          # take Genus.y as the true Genus
  select(
    `species name`,
    Genus,
    everything(),
    -Genus.x,
    -Genus.y
  )
write_xlsx(forest_sp_clean, "I:/DATA/forest_sp_with_genus.xlsx")

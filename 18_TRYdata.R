library(dplyr)
library(rgbif)
library(openxlsx)
library(readxl)
library(writexl)
library(tidyr)
library(stringr)

# Load TRY age of maturity data (long format)

forest_sp <- read.csv(
  "I:\\DATA\\ForestSpecialist.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

testdf <- read.delim(
  "I:\\DATA\\46884.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = "",
  fill = TRUE
)

# Clean TRY data: extract only age of maturity related entries.
age_df <- testdf %>%
  filter(
    TraitID == 155 |              # age of maturity
    DataID %in% c(782, 472)        # primary & secondary juvenile period
  )
age_df <- age_df %>%
  select(
    SpeciesName,
    AccSpeciesName,
    OrigValueStr
  )
str(age_df)

keep_sp <- forest_sp$species.name

age_140 <- age_df %>%
  mutate(
    SpeciesName    = str_squish(SpeciesName),
    AccSpeciesName = str_squish(AccSpeciesName)
  ) %>%
  pivot_longer(
    cols = c(SpeciesName, AccSpeciesName),
    names_to = "matched_from",
    values_to = "matched_species"
  ) %>%
  filter(matched_species %in% keep_sp)

age_140_clean <- age_140 %>%
  filter(
    !is.na(OrigValueStr),
    str_trim(OrigValueStr) != ""
  )
age_140_clean <- age_140_clean %>%
  select(matched_species, OrigValueStr, matched_from)

# Based on species synonyms, we could probably match more species.
# Use rgbif to check the synonyms and get accepted names.
gbif_match <- name_backbone_checklist(
  name_data = keep_sp,
  kingdom = "Plantae",
  verbose = TRUE
)
str(gbif_match)

gbif_clean <- gbif_match %>%
  filter(confidence >= 50) %>%   # conservative, you can relax to 80
  mutate(
    accepted_name = ifelse(
      !is.na(acceptedUsageKey),
      species,          # accepted species name
      canonicalName     # already accepted
    )
  ) %>%
  distinct(verbatim_name, accepted_name)

# gbif_clean has: verbatim_name, accepted_name
gbif_lookup <- gbif_clean %>%
  mutate(
    verbatim_name = str_squish(verbatim_name),
    accepted_name = str_squish(accepted_name)
  ) %>%
  pivot_longer(
    cols = c(verbatim_name, accepted_name),
    names_to = "name_type",
    values_to = "name"
  ) %>%
  filter(!is.na(name), name != "") %>%
  distinct(name)

# Match age_df again.
age_140_clean2 <- age_df %>%
  mutate(
    SpeciesName    = str_squish(SpeciesName),
    AccSpeciesName = str_squish(AccSpeciesName),
    OrigValueStr   = str_squish(OrigValueStr)
  ) %>%
  pivot_longer(
    cols = c(SpeciesName, AccSpeciesName),
    names_to = "matched_from",
    values_to = "matched_species"
  ) %>%
  semi_join(gbif_lookup, by = c("matched_species" = "name")) %>%
  filter(OrigValueStr != "") %>%                     # removes empty strings
  select(matched_species, OrigValueStr, matched_from) # reorder columns

age_140_merged <- bind_rows(
  age_140_clean  = age_140_clean,
  age_140_clean2 = age_140_clean2,
  .id = "source_df"
)
unique(age_140_merged$OrigValueStr)

write.xlsx(
  age_140_merged,
  file = "I:\\DATA\\age_140_clean_merged02.xlsx",
  overwrite = TRUE
)

# Read in the manually cleaned age_140_merge data from TRY.
age_140_merge_clean <-read_excel(
  "I:\\DATA\\age_140_clean_merged02.xlsx",
  sheet = 1,
  col_names = TRUE
)
str(age_140_merge_clean)
age_140_merge_clean <- age_140_merge_clean %>%
  mutate(
    OrigValueStr = as.numeric(OrigValueStr)
  )

# Remove columns of source_df and matched_from.
age_140_merge_clean <- age_140_merge_clean %>%
  select(
    matched_species,
    OrigValueStr
  )

# Group by species and get median age of maturity.
age_140_final <- age_140_merge_clean %>%
  group_by(matched_species) %>%
  summarise(
    OrigValueStr = median(OrigValueStr, na.rm = TRUE),
    Comment = "Median",
    .groups = "drop"
  )

# Merge with the forest species list to add the ages.
forest_age <- forest_sp %>%
  left_join(
    age_140_final,
    by = c("species.name" = "matched_species")
  ) %>%
  rename(
    age_maturity_final = OrigValueStr
  )

# Check if forest species can find age of maturity by average genus-level age.
age_missing <-forest_age %>% filter(is.na(age_maturity_final)) %>% arrange(species.name)

# Average genus-level age of maturity for remaining missing species.
# --- 2) Add genus to final_140 (from species name) ---
age_missing <- age_missing %>%
  mutate(genus = sub(" .*", "", species.name))
head(age_missing)

# Rename the columns.
colnames(age_140_final) <- c(
  "species.name",
  "age_maturity",
  "Source"
)

# --- 3) Prepare long dataset: extract genus and ensure numeric ages ---
age_genus <- age_140_final %>%
  mutate(
    species.name = trimws(species.name),
    genus = sub(" .*", "", species.name),
    age_maturity_num = suppressWarnings(as.numeric(age_maturity))
  )
# --- 4) Compute genus-level mean age from long dataset ---
genus_mean <- age_genus %>%
  filter(!is.na(age_maturity_num)) %>%
  group_by(genus) %>%
  summarise(genus_age_mean = mean(age_maturity_num), n_species = n(), .groups = "drop")

# --- 5) Fill missing species ages using genus mean (if available) ---
age_missing_filled <- age_missing %>%
  left_join(genus_mean, by = "genus") %>%
  mutate(
    age_maturity_genus = ifelse(
      is.na(age_maturity_final),
      genus_age_mean,
      age_maturity_final
    ),
    Comment = ifelse(
      is.na(Comment) & !is.na(genus_age_mean),
      "Average genus",
      Comment
    )
  ) %>%
  select(-genus_age_mean, -n_species, -age_maturity_final)  # optional cleanup

# Merge back to get the full dataset.
forest_age_filled <- forest_age %>%
  left_join(age_missing_filled %>% select(species.name, age_maturity_genus),
    by = "species.name") %>%
  mutate(
    age_maturity_final = ifelse(
      is.na(age_maturity_final),
      age_maturity_genus,
      age_maturity_final
    ),
    Comment = ifelse(
      is.na(Comment ) & !is.na(age_maturity_genus),
      "Average genus",
      Comment
    )
  ) %>%
  select(-age_maturity_genus)  # optional cleanup

# Check remaining missing
nrow(forest_age_filled)                    # should be 140
sum(is.na(forest_age_filled$age_maturity_final)) # how many still missing after genus fill? 63

# Export final dataset with age of maturity as .xlsx (including genus-level imputations)
write.xlsx(
  forest_age_filled,
  file = "I:\\DATA\\forestSpecialist_age_maturity.xlsx",
  overwrite = TRUE
)


##### Match dispersal mode for each species.
dispersal_mode <- read_excel(
  "I:\\DATA\\Lososova_et_al_2023_Dispersal_version2.xlsx",
  sheet = 1,
  col_names = TRUE
)
str(dispersal_mode)

forest_sp <- read_excel(
  "I:\\DATA\\forest_species_age_maturity_140.xlsx",
  sheet = 1,
  col_names = TRUE
)

dispersal_mode_sel <- dispersal_mode %>%
  select(
    Taxon,
    Genus,
    Family,
    `Efficient dispersal mode - common`,
    `Dispersal distance class (1-6)`
  )


age_with_dispersal <- all_age %>%
  left_join(
    dispersal_mode_sel,
    by = c("species.name" = "Taxon")
  )
sum(is.na(age_with_dispersal$'Dispersal distance class (1-6)'))

write.xlsx(
  age_with_dispersal,
  file = "I:/DATA/forest_with_maturity_dispersalClass.xlsx",
  overwrite = TRUE
)


library(dplyr)
library(rgbif)
library(openxlsx)

df <- read.delim(
  "I:\\DATA\\tde202612215846.txt",
  sep = "\t",
  header = TRUE,
  skip = 3,          # skip the metadata lines
  stringsAsFactors = FALSE
)

head(df)
str(df)
df <- df[, c("AccSpeciesName", "Measurements")]
colnames(df) <- c("species.name", "age_maturity")

forest_sp <- read.csv(
  "I:\\DATA\\ForestSpecialist.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# (optional but recommended) standardise column names + trim spaces
forest_sp <- forest_sp %>%
  mutate(species.name = trimws(species.name))

df <- df %>%
  mutate(species.name = trimws(species.name))

# Check duplicates in df.
dup_species <- df %>%
  count(species.name) %>%
  filter(n > 1) %>%
  arrange(desc(n))
any(duplicated(df$species.name)) # FALSE.
## There are no duplicates.

# join: keep all 140 species, bring in age_maturity where available
forest_age <- forest_sp %>%
  left_join(df, by = "species.name")

# Check how many species did not get age_maturity info.
missing_age <- forest_age %>%
  filter(is.na(age_maturity))

write.csv(
  forest_age,
  file = "I:\\DATA\\ForestSpecialist_withAgeMaturity.csv",
  row.names = FALSE
)
write.csv(
    missing_age,
    file = "I:\\DATA\\ForestSpecialist_missingAgeMaturity.csv",
    row.names = FALSE
)

# load missing species and check GBIF backbone
missing_age <- read.csv(
  "I:\\DATA\\ForestSpecialist_missingAgeMaturity.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

gbif_match <- name_backbone_checklist(
  name_data = missing_age["species.name"],
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

colnames(gbif_match)


## join back to missing_age to get accepted names and then age_maturity
forest_age_fixed <- forest_age %>%
  left_join(
    gbif_clean,
    by = c("species.name" = "verbatim_name")
  ) %>%
  mutate(
    name_for_join = coalesce(accepted_name, species.name)
  ) %>%
  left_join(
    df %>% select(species.name, age_maturity),
    by = c("name_for_join" = "species.name"),
    suffix = c("", "_from_long")
  ) %>%
  mutate(
    age_maturity_final = coalesce(
      suppressWarnings(as.numeric(age_maturity)),
      suppressWarnings(as.numeric(age_maturity_from_long))
    )
  )
sum(is.na(forest_age_fixed$age_maturity_final))
str(forest_age_fixed)

final_140 <- forest_age_fixed %>%
  mutate(
    age_maturity_final = suppressWarnings(as.numeric(age_maturity_final))
    ) %>%
  group_by(species.name) %>%
  summarise(
    age_maturity = mean(age_maturity_final, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # mean(..., na.rm=TRUE) gives NaN if all values were NA -> convert to NA
  mutate(
    age_maturity = ifelse(is.nan(age_maturity), NA_real_, age_maturity)
    ) %>%
  select(species.name, age_maturity)
final_missing <-final_140 %>% filter(is.na(age_maturity)) %>% arrange(species.name)

# Average genus-level age of maturity for remaining missing species.
# --- 2) Add genus to final_140 (from species name) ---
final_140 <- final_140 %>%
  mutate(genus = sub(" .*", "", species.name))
head(final_140)
# --- 3) Prepare long dataset: extract genus and ensure numeric ages ---
df_long <- df %>%
  mutate(
    species.name = trimws(species.name),
    genus = sub(" .*", "", species.name),
    age_maturity_num = suppressWarnings(as.numeric(age_maturity))
  )
# --- 4) Compute genus-level mean age from long dataset ---
genus_mean <- df_long %>%
  filter(!is.na(age_maturity_num)) %>%
  group_by(genus) %>%
  summarise(genus_age_mean = mean(age_maturity_num), n_species = n(), .groups = "drop")

# --- 5) Fill missing species ages using genus mean (if available) ---
final_140_imputed <- final_140 %>%
  left_join(genus_mean, by = "genus") %>%
  mutate(
    age_maturity_filled = ifelse(is.na(age_maturity), genus_age_mean, age_maturity)
  ) %>%
  select(species.name, age_maturity = age_maturity_filled)

final_140_imputed <- final_140 %>%
  left_join(genus_mean, by = "genus") %>%
  mutate(
    age_maturity = dplyr::coalesce(age_maturity, genus_age_mean)
  ) %>%
  select(species.name, age_maturity)

# Check remaining missing
nrow(final_140_imputed)                    # should be 140
sum(is.na(final_140_imputed$age_maturity)) # how many still missing after genus fill?

# Export final dataset with age of maturity as .xlsx (including genus-level imputations)
library(writexl)

write_xlsx(
  final_140_imputed,
  path = "I:\\DATA\\forest_species_age_maturity_140.xlsx"
)

# New data frame from TRY dataset for age of maturity.
# Test if we can extract more precise values from this original TRY data.
testdf <- read.delim(
  "I:\\DATA\\46884.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = "",
  fill = TRUE
)

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

library(stringr)
age_df_clean <- age_df %>%
  mutate(
    maturity_age = case_when(
      # ranges like "2-3", "6-10"
      str_detect(OrigValueStr, "-") ~ {
        vals <- str_split(OrigValueStr, "-", simplify = TRUE)
        rowMeans(apply(vals, 2, as.numeric), na.rm = TRUE)
      },

      # values with < or >
      str_detect(OrigValueStr, "[<>]") ~ 
        as.numeric(str_remove_all(OrigValueStr, "[<>]")),

      # plain numeric values
      TRUE ~ as.numeric(OrigValueStr)
    )
  )

bad_vals <- age_df_clean %>%
  filter(is.na(maturity_age)) %>%
  distinct(OrigValueStr) %>%
  arrange(OrigValueStr)

bad_vals

keep_sp <- forest_sp$species.name

age_140 <- age_df_clean %>%
  mutate(
    matched_species = case_when(
      SpeciesName %in% keep_sp    ~ SpeciesName,
      AccSpeciesName %in% keep_sp ~ AccSpeciesName,
      TRUE                        ~ NA_character_
    )
  ) %>%
  filter(!is.na(matched_species))
unique(age_140$OrigValueStr)

##### Match dispersal mode for each species.
library(readxl)
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


forest_with_dispersal <- forest_sp %>%
  left_join(
    dispersal_mode_sel,
    by = c("species.name" = "Taxon")
  )
sum(is.na(forest_with_dispersal$'Dispersal distance class (1-6)'))

write.xlsx(
  forest_with_dispersal,
  file = "I:/DATA/forest_with_maturity_dispersalClass.xlsx",
  overwrite = TRUE
)


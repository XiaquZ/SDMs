library(sf)
library(dplyr)
library(terra)

wdpa_dir <- "I:/DATA/WDPA_May2026_Public_shp"

# Find all shapefiles inside the three folders
shp_files <- list.files(
  wdpa_dir,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

shp_files
# Only select the polygon shapefiles.
poly_files <- shp_files[grepl("polygons\\.shp$", shp_files)]

poly_files

# Read and combine the polygons:
wdpa_list <- lapply(poly_files, st_read, quiet = TRUE)
wdpa_poly <- bind_rows(wdpa_list)

nrow(wdpa_poly)
names(wdpa_poly)
st_crs(wdpa_poly)

# Check the key fields:
wdpa_poly %>%
  st_drop_geometry() %>%
  select(
    SITE_ID, SITE_PID, SITE_TYPE, NAME_ENG, NAME,
    DESIG_ENG, DESIG_TYPE, IUCN_CAT,
    REALM, STATUS, STATUS_YR,
    VERIF, PRNT_ISO3, ISO3,
    GIS_AREA, METADATAID
  ) %>%
  head()

# Check the unique values:
unique(wdpa_poly$SITE_TYPE)
unique(wdpa_poly$REALM)
unique(wdpa_poly$STATUS)
unique(wdpa_poly$DESIG_TYPE)
unique(wdpa_poly$IUCN_CAT)

# Include Europe study-area ISO3 list.
europe_iso3 <- c(
  # EU-27
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
  "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
  "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE",
  
  # Non-EU countries visible/relevant in your map
  "GBR",  # United Kingdom
  "NOR",  # Norway
  "ISL",  # Iceland, if included in your raster extent
  "CHE",  # Switzerland
  "LIE",  # Liechtenstein
  "ALB",  # Albania
  "BIH",  # Bosnia and Herzegovina
  "MNE",  # Montenegro
  "MKD",  # North Macedonia
  "SRB",  # Serbia
  "XKX",  # Kosovo, if present in WDPA
  "MDA",  # Moldova, if included
  "UKR",  # Ukraine, if included
  "BLR",  # Belarus, if included
  "AND",  # Andorra
  "MCO",  # Monaco
  "SMR",  # San Marino
  "VAT",  # Vatican City
  "TUR"   # Turkey, only relevant for European Turkey
)

europe_pattern <- paste(europe_iso3, collapse = "|")

# Filter the countries.
wdpa_europe_terr <- wdpa_poly %>%
  filter(
    grepl(europe_pattern, ISO3) |
      grepl(europe_pattern, PRNT_ISO3)
  ) %>%
  filter(
    SITE_TYPE == "PA",
    REALM == "Terrestrial",
    STATUS %in% c("Designated", "Established", "Inscribed", "Adopted")
  )

nrow(wdpa_europe_terr)
table(wdpa_europe_terr$ISO3)
table(wdpa_europe_terr$REALM)
table(wdpa_europe_terr$STATUS)

# Crop the WDPA to the MF areas.
mf <- rast("N:/SDMs/Multifunctionality_RedList/MF_average_4MIs_RedList.tif")

crs(mf)
ext(mf)

# Smaller dataset:
wdpa_europe_terr_small <- wdpa_europe_terr %>%
  select(
    SITE_ID, SITE_PID, NAME_ENG, NAME, DESIG_ENG, DESIG_TYPE,
    IUCN_CAT, REALM, STATUS, STATUS_YR,
    VERIF, PRNT_ISO3, ISO3, GIS_AREA, METADATAID,
    geometry
  )

# Reproject:
wdpa_europe_terr_3035 <- st_transform(wdpa_europe_terr_small, crs(mf))

# Create polygons based on MF:
mf_extent_sf <- st_as_sf(as.polygons(ext(mf), crs = crs(mf)))

# Keep WDPA polygons that fall inside the broad area of my raster.
wdpa_crop <- st_crop(
  wdpa_europe_terr_3035,
  st_bbox(mf_extent_sf)
)

# Clean cropped WDPA geometries
wdpa_crop_clean <- wdpa_crop %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON", warn = FALSE) %>%
  st_cast("MULTIPOLYGON", warn = FALSE)

# Remove empty geometries
wdpa_crop_clean <- wdpa_crop_clean[!st_is_empty(wdpa_crop_clean), ]

# Keep only the field needed for rasterization
wdpa_crop_clean <- wdpa_crop_clean %>%
  mutate(protected = 1) %>%
  select(protected, geometry)

# Check geometry types
table(st_geometry_type(wdpa_crop_clean))
st_crs(wdpa_crop_clean)
crs(mf)
nrow(wdpa_crop)
nrow(wdpa_crop_clean)
# Convert cleaned sf object to terra SpatVector
wdpa_vect <- vect(wdpa_crop_clean)

# Save the rasterize wdpa:
writeVector(
  wdpa_vect,
  "N:/SDMs/WDPA_ProtectedAreas/wdpa_europe_terr_clean_3035.gpkg",
  overwrite = TRUE
)

# Rasterize directly to the MF raster grid
pa_mask <- rasterize(
  wdpa_vect,
  mf,
  field = "protected",
  background = 0
)
# Check output
pa_mask
plot(pa_mask)

# Save the PA mask.
writeRaster(
  pa_mask,
  "N:/SDMs/WDPA_ProtectedAreas/WDPA_PA_mask_Europe_3035.tif",
  overwrite = TRUE
)

# Reload the PA map:
pa_mask <- rast("N:/SDMs/WDPA_ProtectedAreas/WDPA_PA_mask_Europe_3035.tif")

# mask the forest area.
pa_mask_forest <- mask(
  pa_mask,
  mf
)
plot(pa_mask_forest)

writeRaster(
  pa_mask_forest,
  "N:/SDMs/WDPA_ProtectedAreas/WDPA_PA_mask_Europe_Forest_3035.tif",
  overwrite = TRUE
)


pa_mask_forest <- rast("N:/SDMs/WDPA_ProtectedAreas/WDPA_PA_mask_Europe_Forest_3035.tif")

freq_pa_forest <- freq(pa_mask_forest)
freq_pa_forest

# Extract count:
n_forest_unprotected <- freq_pa_forest$count[freq_pa_forest$value == 0]
n_forest_protected   <- freq_pa_forest$count[freq_pa_forest$value == 1]

n_forest <- n_forest_unprotected + n_forest_protected

prop_forest_protected <- n_forest_protected / n_forest
prop_forest_protected
## 0.3063

# Higher-MF values, with top 10 percentile:
set.seed(123)

# Create sample
mf_sample <- spatSample(
  mf,
  size = 1000000,
  method = "random",
  na.rm = TRUE,
  as.df = TRUE
)

q90 <- quantile(mf_sample[[1]], probs = 0.9, na.rm = TRUE)

# Create high-MF mask: 1 = top 10% MF, NA = everything else
high_mf <- ifel(
  !is.na(mf) & mf >= q90,
  1,
  NA,
  filename = "N:/SDMs/WDPA_ProtectedAreas/MF_average_RedList_4MIs/high_mf_q90_mask.tif",
  overwrite = TRUE
)
plot(high_mf)
# Areas with high MF.
pa_high_mf <- mask(
  pa_mask_forest,
  high_mf,
  filename = "N:/SDMs/WDPA_ProtectedAreas/MF_average_RedList_4MIs/pa_high_mf_q90_RedList.tif",
  overwrite = TRUE,
  datatype = "INT1U",
  NAflag = 255
)

plot(pa_high_mf)
freq(pa_high_mf)
## 1  = high-MF forest cell inside protected area
## 0  = high-MF forest cell outside protected area
## NA = not high-MF or outside forest
# Then calculated:
freq_pa_high <- freq(pa_high_mf)

n_high_unprotected <- freq_pa_high$count[freq_pa_high$value == 0]
n_high_protected   <- freq_pa_high$count[freq_pa_high$value == 1]

n_high <- n_high_unprotected + n_high_protected

prop_high_mf_protected <- n_high_protected / n_high

prop_high_mf_protected
prop_high_mf_protected * 100
## For MF average of Red List species, the prop_high_mf_protected is 0.3287.

#### When choose 50 percentile:####
q50 <- quantile(mf_sample[[1]], probs = 0.5, na.rm = TRUE)

# Create high-MF mask: 1 = top 50% MF, NA = everything else
high_mf50 <- ifel(
  !is.na(mf) & mf >= q50,
  1,
  NA,
  filename = "N:/SDMs/WDPA_ProtectedAreas/high_mf_q50_mask.tif",
  overwrite = TRUE
)
# Areas with high MF.
pa_high_mf50 <- mask(
  pa_mask_forest,
  high_mf50,
  filename = "N:/SDMs/WDPA_ProtectedAreas/pa_high_mf_q50.tif",
  overwrite = TRUE,
  datatype = "INT1U",
  NAflag = 255
)

plot(pa_high_mf50)
freq(pa_high_mf50)
## 1  = high-MF forest cell inside protected area
## 0  = high-MF forest cell outside protected area
## NA = not high-MF or outside forest
# Then calculated:
freq_pa_high50 <- freq(pa_high_mf50)

n_high_unprotected50 <- freq_pa_high50$count[freq_pa_high50$value == 0]
n_high_protected50   <- freq_pa_high50$count[freq_pa_high50$value == 1]

n_high50 <- n_high_unprotected50 + n_high_protected50

prop_high_mf_protected50 <- n_high_protected50 / n_high50

prop_high_mf_protected50
prop_high_mf_protected50 * 100

library(terra)

# Load the data from linear models.
lmbio5 <- readRDS(file = "I:/DATA/output/BIO5BIO6_lm/lm_offsetVSterraBIO5.rds")
lmbio6 <- readRDS(file = "I:/DATA/output/BIO5BIO6_lm/lm_offsetVSterraBIO6.rds")

# Extract slope values.
slope5 <- lmbio5$coefficients[[2]]
slope6 <- lmbio6$coefficients[[2]]

# Calculate the delta macroclimate between current and future.
# BIO5
macrobio5_fut <- rast(
    "E:/Input/CHELSAdata/BIO5BIO6/2071-2100/2071-2100chelsaBIO5_EUforests_25m.tif"
)
macrobio5_pre <- rast(
    "E:/Input/TerraClimate/2000-2020terraBIO5_EUforests_25m.tif"
)

# BIO6
macrobio6_fut <- rast(
    "E:/Input/CHELSAdata/BIO5BIO6/2071-2100/2071-2100chelsaBIO6_EUforests_25m.tif"
)
macrobio6_pre <- rast(
    "E:/Input/TerraClimate/2000-2020terraBIO6_EUforests_25m.tif"
)

deltamacro5 <- macrobio5_fut - macrobio5_pre
deltamacro6 <- macrobio6_fut - macrobio6_pre

#### Future offset BIO5 BIO6 ####
# Load offset of current time.
offsetbio5_pre <- rast("E:/Input/TerraClimate/terraOffsetBIO5_2000-2020.tif")
offsetbio6_pre <- rast("E:/Input/TerraClimate/terraOffsetBIO6_2000-2020.tif")

# Calculate future offset.
offsetbio5_fut <- slope5 * deltamacro5 + offsetbio5_pre
offsetbio6_fut <- slope6 * deltamacro6 + offsetbio6_pre

#### Calculate the future microclimate bio5 and 6.
# First, load the future macroclimate bio5 and 6.
microbio5_fut <- macrobio5_fut + offsetbio5_fut
microbio6_fut <- macrobio6_fut + offsetbio6_fut
print(microbio5_fut)
print(microbio6_fut)

# Round the data.
microbio5_fut <- round(microbio5_fut, digits = 1)
microbio6_fut <- round(microbio6_fut, digits = 1)
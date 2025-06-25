library(terra)
library(tibble)
library(ggplot2)
library(ggpubr)

###########################
#### Extract sample data####
###########################

# Load offset and macroclimate data of BIO5.
offsetbio5 <- rast(
  "E:/Output/BIO5BIO6/ForestClimTerraClimate_OffsetBIO5_2000-2020.tif"
)
macrobio5 <- rast(
  "E:/Output/BIO5BIO6/2000-2020terraBIO5_EUforests_25m.tif"
)
offsetbio5
macrobio5
plot(offsetbio5, main = "OffsetBIO5 2000-2020")
plot(macrobio5, main = "Terra BIO5 2000-2020")

# Create sample data
s <- c(offsetbio5, macrobio5)
set.seed(123)
SampleOffsetbio5 <- spatSample(s, 10^6, "regular", na.rm = TRUE, xy = TRUE)
head(SampleOffsetbio5)
colnames(SampleOffsetbio5)[3] <- "offsetBIO5"
colnames(SampleOffsetbio5)[4] <- "terraMacroBIO5"

#### Check normal distribution####
hist(SampleOffsetbio5$offsetBIO5)
hist(SampleOffsetbio5$terraMacroBIO5)

########################
#### Fit linear model####
########################
reg1 <- lm(offsetBIO5 ~ terraMacroBIO5, data = SampleOffsetbio5)
summary(reg1)

# Save lm results.
saveRDS(reg1, file = "I:/DATA/output/BIO5BIO6_lm/lm_offsetVSterraBIO5.rds")

# Add the regression line
ggplot(SampleOffsetbio5, aes(x = terraMacroBIO5, y = offsetBIO5)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(
    label.x = min(SampleOffsetbio5$terraMacroBIO5),
    label.y = max(SampleOffsetbio5$offsetBIO5),
    size = 10
  ) +
  stat_cor(
    aes(label = ..rr.label..),
    label.x = min(SampleOffsetbio5$terraMacroBIO5),
    label.y = max(SampleOffsetbio5$offsetBIO5) - 0.7,
    size = 10
  ) +
  theme(
    axis.title = element_text(size = 16), # Adjust x and y axis title size
    axis.text = element_text(size = 16) # Adjust x and y axis tick mark size
  )

#### Load offset and macroclimate data of BIO6.
offsetbio6 <- rast(
  "E:/Output/BIO5BIO6/ForestClimTerraClimate_OffsetBIO6_2000-2020.tif"
  )
macrobio6 <- rast(
  "E:/Output/BIO5BIO6/2000-2020terraBIO6_EUforests_25m.tif"
)
plot(offsetbio6, main = "OffsetBIO6 1981-2010")
plot(macrobio6, main = " CHELSA BIO6 1981-2010")
offsetbio6
macrobio6

# Create sample data
sbio6 <- c(offsetbio6, macrobio6)

set.seed(23)
SampleOffsetbio6 <- spatSample(sbio6, 10^6, "regular", na.rm = TRUE, xy = TRUE)
head(SampleOffsetbio6)
colnames(SampleOffsetbio6)[3] <- "offsetBIO6"
colnames(SampleOffsetbio6)[4] <- "terraMacroBIO6"

########################
#### Fit linear model####
########################
reg2 <- lm(offsetBIO6 ~ terraMacroBIO6, data = SampleOffsetbio6)
summary(reg2)

# Save lm results.
saveRDS(reg2, file = "I:/DATA/output/BIO5BIO6_lm/lm_offsetVSterraBIO6.rds")

# Add the regression line
ggplot(SampleOffsetbio6, aes(x = terraMacroBIO6, y = offsetBIO6)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(
    label.x = min(SampleOffsetbio6$terraMacroBIO6),
    label.y = max(SampleOffsetbio6$offsetBIO6),
    size = 10
  ) +
  stat_cor(
    aes(label = ..rr.label..),
    label.x = min(SampleOffsetbio6$terraMacroBIO6),
    label.y = max(SampleOffsetbio6$offsetBIO6) - 0.7,
    size = 10
  ) +
  theme(
    axis.title = element_text(size = 16), # Adjust x and y axis title size
    axis.text = element_text(size = 16) # Adjust x and y axis tick mark size
  )

#### Check normal distribution####
hist(SampleOffset$offsetBIO6)
hist(SampleOffset$chelsaBIO6)

################################################
#### compare microclimate with macroclimate.####
microbio6 <- rast("E:/Input/ForestBioClim/ForestClim_06.tif")
s <- c(microbio6, macrobio6)

# extract samples
set.seed(123)
sample_micmacro <- spatSample(s, 10^6, "regular", na.rm = TRUE, xy = TRUE)
head(sample_micmacro)
colnames(sample_micmacro)[3] <- "microBIO6"
colnames(sample_micmacro)[4] <- "macroBIO6"

reg <- lm(microBIO6 ~ macroBIO6, data = sample_micmacro)
summary(reg) # slope = 0.752 <1 , buffering?

# Add the regression line
ggplot(sample_micmacro, aes(x = macroBIO6, y = microBIO6)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(
    label.x = min(sample_micmacro$macroBIO6),
    label.y = max(sample_micmacro$microBIO6),
    size = 10
  ) +
  stat_cor(
    aes(label = ..rr.label..),
    label.x = min(sample_micmacro$macroBIO6),
    label.y = max(sample_micmacro$microBIO6) - 1.0,
    size = 10
  ) +
  theme(
    axis.title = element_text(size = 16), # Adjust x and y axis title size
    axis.text = element_text(size = 16) # Adjust x and y axis tick mark size
  )

#### BIO5.
microbio5 <- rast("E:/Input/ForestBioClim/ForestClim_05.tif")
s <- c(microbio5, macrobio5)

# extract samples
set.seed(123)
sample_micmacro02 <- spatSample(s, 10^6, "regular", na.rm = TRUE, xy = TRUE)
head(sample_micmacro02)
colnames(sample_micmacro02)[3] <- "microBIO5"
colnames(sample_micmacro02)[4] <- "macroBIO5"
regbio5 <- lm(microBIO5 ~ macroBIO5, data = sample_micmacro02)
summary(regbio5) # slope > 1 , amplification?

# Add the regression line
ggplot(sample_micmacro02, aes(x = macroBIO5, y = microBIO5)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(
    label.x = min(sample_micmacro02$macroBIO5),
    label.y = max(sample_micmacro02$microBIO5),
    size = 10
  ) +
  stat_cor(
    aes(label = ..rr.label..),
    label.x = min(sample_micmacro02$macroBIO5),
    label.y = max(sample_micmacro02$microBIO5) - 1.0,
    size = 10
  ) +
  theme(
    axis.title = element_text(size = 16), # Adjust x and y axis title size
    axis.text = element_text(size = 16) # Adjust x and y axis tick mark size
  )

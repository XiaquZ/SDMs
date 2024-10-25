library(terra)
library(tibble)
library(ggplot2)

###########################
####Extract sample data####
###########################

# Load offset and macroclimate data of BIO5.
offsetbio5 <- rast("E:/Input/TerraClimate/terraOffsetBIO5_2000-2020.tif")
macrobio5 <- rast(
  "E:/Input/TerraClimate/2000-2020terraBIO5_EUforests_25m.tif"
)
plot(offsetbio5, main = "OffsetBIO5 1981-2010")
plot(macrobio5, main = "Terra BIO5 1981-2010")

# Create sample data
s <- c(offsetbio5, macrobio5)
# set.seed(123)
# x <- spatSample(s, 10^6, "regular", na.rm = TRUE, xy = TRUE)
# anyNA(x$ForestClim_05)
# head(x)
# colnames(x)[3] <- "offsetBIO5"
# colnames(x)[4] <- "chelsaBIO5"

set.seed(123)
SampleOffsetbio5 <- spatSample(s, 100000, "regular", na.rm = TRUE, xy = TRUE)
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

# Add the regression line
ggplot(SampleOffsetbio5, aes(x = terraMacroBIO5, y = offsetBIO5)) +
  geom_point() +
  geom_smooth(method = lm)

save(reg1,
  file = "/lustre1/scratch/348/vsc34871/output/FutureMicroData/regression_macroCHELSA_VS_offset.RData"
)


#### Load offset and macroclimate data of BIO6.
offsetbio6 <- rast("E:/Input/TerraClimate/OffsetBIO6_2000-2020.tif")
macrobio6 <- rast(
  "E:/Input/TerraClimate/2000-2020terraBIO6_EUforests_25m.tif"
)
plot(offsetbio6, main = "OffsetBIO6 1981-2010")
plot(macrobio6, main = " CHELSA BIO6 1981-2010")

# Create sample data
sbio6 <- c(offsetbio6, macrobio6)
# set.seed(123)
# x <- spatSample(sbio6, 10^6, "regular", na.rm = TRUE, xy = TRUE)
# anyNA(x$ForestClim_06)
# head(x)
# colnames(x)[3] <- "offsetBIO6"
# colnames(x)[4] <- "chelsaBIO6"

set.seed(23)
SampleOffsetbio6 <- spatSample(sbio6, 100000, "regular", na.rm = TRUE, xy = TRUE)
head(SampleOffsetbio6)
colnames(SampleOffsetbio6)[3] <- "offsetBIO6"
colnames(SampleOffsetbio6)[4] <- "terraMacroBIO6"

########################
#### Fit linear model####
########################
reg2 <- lm(offsetBIO6 ~ terraMacroBIO6, data = SampleOffsetbio6)
summary(reg2)

# Add the regression line
ggplot(SampleOffsetbio6, aes(x = terraMacroBIO6, y = offsetBIO6)) +
  geom_point() +
  geom_smooth(method = lm)

#### Check normal distribution####
hist(SampleOffset$offsetBIO6)
hist(SampleOffset$chelsaBIO6)

#### compare microclimate with macroclimate.####
microbio6 <- rast("E:/Input/ForestBioClim/ForestClim_06.tif")
s <- c(microbio6, macrobio6)

# extract samples
set.seed(123)
SampleOffset <- spatSample(s, 100000, "regular", na.rm = TRUE, xy = TRUE)
head(SampleOffset)
colnames(SampleOffset)[3] <- "microBIO6"
colnames(SampleOffset)[4] <- "macroBIO6"

reg <- lm(microBIO6 ~ macroBIO6, data = SampleOffset)
summary(reg) # slope = 0.96 <1 , buffering?
# Add the regression line
ggplot(SampleOffset, aes(x = macroBIO6, y = microBIO6)) +
  geom_point() +
  geom_smooth(method = lm)

#### BIO5.
microbio5 <- rast("E:/Input/ForestBioClim/ForestClim_05.tif")
s <- c(microbio5, macrobio5)

# extract samples
set.seed(123)
SampleOffsetbio5 <- spatSample(s, 100000, "regular", na.rm = TRUE, xy = TRUE)
head(SampleOffsetbio5)
colnames(SampleOffsetbio5)[3] <- "microBIO5"
colnames(SampleOffsetbio5)[4] <- "macroBIO5"
regbio5 <- lm(microBIO5 ~ macroBIO5, data = SampleOffsetbio5)
summary(regbio5) # slope = 1.05 > 1 , amplification?
# Add the regression line
ggplot(SampleOffsetbio5, aes(x = macroBIO5, y = microBIO5)) +
  geom_point() +
  geom_smooth(method = lm)

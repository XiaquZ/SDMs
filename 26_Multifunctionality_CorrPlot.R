library(terra)
library(ggplot2)
# Load Multifunctionality data.
mf_av_all <- rast(
    "N:/SDMs/Multifunctionality_140species/MF_average_4MIs.tif"
)
mf_av_redlist <- rast(
  "N:/SDMs/Multifunctionality_RedList/MF_average_4MIs_RedList.tif"
  )
mf_singleT_all <- rast(
  "N:/SDMs/Multifunctionality_140species/MF_threshold_count_08_4MIs.tif"
)
mf_singleT_redlist <- rast(
  "N:/SDMs/Multifunctionality_RedList/MF_threshold_count_08_4MIs_RedList.tif"
)

##### MF average of all species vs MF average for Red List species.####
# Stack the variables.
vars_stack01 <- c(mf_av_all, mf_av_redlist)
names(vars_stack01) <- c(
    "mf_av_all", "mf_av_RL"
)
set.seed(123)
samp_vars_raw01 <- spatSample(
  vars_stack01,
  size = 5000,
  method = "random",
  xy = TRUE,
  na.rm = TRUE,
  as.df = TRUE,
  exhaustive = TRUE
)
samp_vars01 <- samp_vars_raw01[complete.cases(samp_vars_raw01), ]

nrow(samp_vars01)
# Scatter plot.
ggplot(samp_vars01, aes(x = mf_av_all, y = mf_av_RL)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1.2,
    color = "blue"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "Multifunctionality (average) for all forest specialist species",
    y = "Multifunctionality (average) for Red List species"
  ) +
  theme_classic()

#### Stack the variables of single threshold MF and average threshold together.####
vars_stack02 <- c(mf_singleT_all, mf_av_all)
names(vars_stack02) <- c(
  "mf_threshold_all", "mf_av_all"
)
set.seed(123)
samp_vars_raw02 <- spatSample(
  vars_stack02,
  size = 5000,
  method = "random",
  xy = TRUE,
  na.rm = TRUE,
  as.df = TRUE,
  exhaustive = TRUE
)
samp_vars02 <- samp_vars_raw02[complete.cases(samp_vars_raw02), ]

## ggplot.
ggplot(samp_vars02, aes(x = mf_threshold_all, y = mf_av_all)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1.2,
    color = "blue"
  ) +
  coord_equal(xlim = c(0, 4), ylim = c(0, 1)) +
  labs(
    x = "Multifunctionality (single-threshold) for all species",
    y = "Multifunctionality (average) for all species"
  ) +
  theme_classic()


#### MF single-threshold of all species vs. MF threshold of Red List species.
mf_threshold_s_allRL <- c(mf_singleT_all, mf_singleT_redlist)
names(mf_threshold_s_allRL) <- c(
  "mf_08threshold_all", "mf_08threshold_RL"
)
set.seed(123)
samp_vars_raw03 <- spatSample(
  mf_threshold_s_allRL,
  size = 5000,
  method = "random",
  xy = TRUE,
  na.rm = TRUE,
  as.df = TRUE,
  exhaustive = TRUE
)
samp_vars03 <- samp_vars_raw03[complete.cases(samp_vars_raw03), ]
## ggplot.
ggplot(samp_vars03, aes(x = mf_08threshold_all, y = mf_08threshold_RL)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1.2,
    color = "blue"
  ) +
  coord_equal(xlim = c(0, 4), ylim = c(0, 4)) +
  labs(
    x = "Multifunctionality (single-threshold) for all species",
    y = "Multifunctionality (single-threshold) for Red List species"
  ) +
  theme_classic()

#### Average MF of Red List species vs threshold MF of Red List species.####
mf_RL <- c(mf_av_redlist, mf_singleT_redlist)
names(mf_RL) <- c(
  "mf_average_RL", "mf_08threshold_RL"
)
set.seed(123)
samp_vars_raw04 <- spatSample(
  mf_RL,
  size = 5000,
  method = "random",
  xy = TRUE,
  na.rm = TRUE,
  as.df = TRUE,
  exhaustive = TRUE
)
samp_vars04 <- samp_vars_raw04[complete.cases(samp_vars_raw04), ]
## ggplot.
ggplot(samp_vars04, aes(x = mf_08threshold_RL, y = mf_average_RL)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1.2,
    color = "blue"
  ) +
  coord_equal(xlim = c(0, 4), ylim = c(0, 1)) +
  labs(
    x = "Multifunctionality (single-threshold) for Red List species",
    y = "Multifunctionality (average) for Red List species"
  ) +
  theme_classic()

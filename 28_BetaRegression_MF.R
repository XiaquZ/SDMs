library(DHARMa)
library(dplyr)
library(mgcv)
library(DHARMa.helpers)
predict_s <- readRDS("I:/DATA/output/MF_SDMs/10000samples_orig_MF_9predicts.rds")

#### Fit the model####
############################
## Prepare data for the models.###
############################

# Edit the forest types.
class(predict_s$type)
predict_s$type[which(predict_s$type == 1)] <- "Broadleaved forest"
predict_s$type[which(predict_s$type == 2)] <- "Coniferous forest"
predict_s$type <- as.factor(predict_s$type)
head(predict_s)

# Standardized predictors.
vars_to_scale <- c(
  "coast", "elevation", "relative_elevation", "slope",
  "TWI", "eastness", "northness", "cover"
)

predict_s[vars_to_scale] <- lapply(
  predict_s[vars_to_scale], function(z) as.numeric(scale(z))
)
str(predict_s)

#### Extract samples if neccesary.
# data_3000 <- sample_n(predict_s, 3000)
# head(data_3000)

#### Poisson model####
# # Poisson WITHOUT intercept.
# mod_pois_no_intercept<- gam(
#   MF_0.8T ~ northness * cover * slope +
#     cover * type +
#     coast +
#     elevation +
#     relative_elevation +
#     TWI +
#     s(x, y, bs = "gp", m = 2) - 1,
#   family = poisson(link = "log"),
#   data = predict_s
# )
# summary(mod_pois_no_intercept)
# simulationOutput_pois <- simulateResiduals(fittedModel = mod_pois_no_intercept)
# plot(simulationOutput_pois)
# testDispersion(simulationOutput_pois) # overdispersion
# testZeroInflation(simulationOutput_pois) # Poisson model has zero-inflation.
#
# par(mfrow = c(1,2))
# plotResiduals(simulationOutput_pois, predict_s$relative_elevation)
# plotResiduals(simulationOutput_pois, predict_s$elevation)
# plotResiduals(simulationOutput_pois, predict_s$type)
# # Overdispersion caused by elevation and forest types?


#### Fit a ZINB model with spatial autocorrelation using brms####
library(brms)

#### Test MF_single08_AllSpecies with binomial model.####
mod_binom_all <- brm(
  MF_single08_AllSpecies | trials(4) ~
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") -1, # "-1" Means remove the intercept.
  family = binomial(link = "logit"),
  data = predict_s,
  chains = 2,
  cores = 2,
  iter = 5000,
  warmup = 1000,
  control = list(adapt_delta = 0.95)
)

summary(mod_binom_all)

# Check the residuals with DHARMa.
# Test assumption:
simres_binom <- dh_check_brms(mod_binom_all, integer = TRUE)
plot(simres_binom)
testDispersion(simres_binom)

#### Test MF_single08_Red List species with binomial model.####
mod_binom_RL <- brm(
  MF_single08_RL| trials(4) ~
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") -1, # "-1" Means remove the intercept.
  family = binomial(link = "logit"),
  data = predict_s,
  chains = 2,
  cores = 2,
  iter = 5000,
  warmup = 1000,
  control = list(adapt_delta = 0.95)
)

summary(mod_binom_RL)
# Test assumption:
simres_binom_RL <- dh_check_brms(mod_binom_RL, integer = TRUE)
testDispersion(simres_binom_RL)


# Test assumption:
simres_pois <- dh_check_brms(mod_pois_all, integer = TRUE)
testDispersion(simres_pois)
## Under dispersion.Test with COM-Poisson

#### Test with com-poisson in glmmTMB.####
library(glmmTMB)

predict_s_mod <- predict_s[complete.cases(
  predict_s[, c(
    "MF_single08_AllSpecies",
    "northness", "slope", "cover",
    "eastness", "type", "coast",
    "elevation", "relative_elevation",
    "TWI", "x", "y"
  )]
), ]

mod_cmp_all <- glmmTMB(
  MF_single08_AllSpecies ~
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") -1,
  family = compois(link = "log"),
  data = predict_s_mod
)
summary(mod_cmp_all)
# Test assumption:
sim_cmp <- simulateResiduals(mod_cmp_all, n = 1000)
plot(sim_cmp)

testDispersion(sim_cmp)
testZeroInflation(sim_cmp)

# COM-Poisson for Red List Species:
mod_cmp_RL <- glmmTMB(
  MF_single08_RL ~
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") -1,
  family = compois(link = "log"),
  data = predict_s_mod
)
summary(mod_cmp_RL)
# Test assumption:
sim_cmp_RL <- simulateResiduals(mod_cmp_RL, n = 1000)
plot(sim_cmp_RL)
testDispersion(sim_cmp_RL)
testZeroInflation(sim_cmp)

# Save model output
save(mod_cmp_all,
     file = "I:/DATA/output/MF_SDMs/Models/MF_singleT08_AllSpecies_COM-Poisson_withoutIntercept.rda"
)
save(mod_cmp_RL,
     file = "I:/DATA/output/MF_SDMs/Models/MF_singleT08_RL_COM-Poisson_withoutIntercept.rda"
)

#### Beta regression accounting spatial autocorrelation. ####
# Let the response variables all greater than 0.
epsilon <- 1e-6

predict_s$MF_av_AllSpecies <- (
  predict_s$MF_av_AllSpecies * (1 - 2 * epsilon)
  ) + epsilon
max(predict_s$MF_av_AllSpecies)
min(predict_s$MF_av_AllSpecies)

## Extract samples if necessary:
# data_3000 <- sample_n(predict_s, 3000)
# max(data_3000$MF_av_AllSpecies)
# min(data_3000$MF_av_AllSpecies)

# Final selected model:Beta regression with 3 interaction terms.
# For all the forest species with phi ~ elevation + cover + type.
mod_bayes_beta_all <- brm(
  bf(
  MF_av_AllSpecies ~
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") - 1, # Without intercept.
  phi ~ elevation + cover + type
),
  family = Beta(),
  data = predict_s,
  cores = 2,
  chains = 2,
  iter = 5000, # Number of iterations
  warmup = 1000
)
summary(mod_bayes_beta_all)


# Save model output
save(mod_bayes_beta_all,
     file = "I:/DATA/output/MF_SDMs/Models/MF_av_AllSpecies_betaRegPhi_withoutIntercept.rda"
)
# Load the model.
load("I:/DATA/output/MF_SDMs/Models/MF_av_Allspecies_betaReg_withoutIntercept.rda")
# Test with DHARMa residuals.
# Test assumption:
sim_beta_all <- dh_check_brms(mod_bayes_beta_all, integer = FALSE)
plot(sim_beta_all)
testDispersion(sim_beta_all)

# MF average For only the Red Listed species:
# Let the response variables all greater than 0.
epsilon <- 1e-6

predict_s$MF_av_RL <- (
  predict_s$MF_av_RL * (1 - 2 * epsilon)
) + epsilon
max(predict_s$MF_av_RL, na.rm = TRUE)
min(predict_s$MF_av_RL, na.rm = TRUE)


mod_bayes_beta_RL <- brm(
  bf(
  MF_av_RL ~
    northness * slope * cover +
    eastness +
    cover * type +
    coast +
    elevation +
    relative_elevation +
    TWI +
    s(x, y, bs = "tp") - 1, # Without intercept.
  phi ~ elevation + cover + type
  ),
  family = Beta(link = "logit"),
  data = predict_s,
  cores = 2,
  chains = 2,
  iter = 5000, # Number of iterations
  warmup = 1000,
  control = list(adapt_delta = 0.95)
)
summary(mod_bayes_beta_RL)

# Save model output
save(mod_bayes_beta_RL,
     file = "I:/DATA/output/MF_SDMs/Models/MF_av_RL_betaRegPhi_withoutIntercept.rda"
)
# Test assumption:
sim_beta_RL <- dh_check_brms(mod_bayes_beta_RL, integer = FALSE)
plot(sim_beta_RL)
testDispersion(sim_beta_RL)

#### plot the interaction.####
library(ggeffects)
# Interactions of cover and forest types.
cover_type <- predict_response(
  mod_bayes_beta02, c("cover", "type"),
  margin = "mean_mode"
)
cover_type
plot(cover_type)

# Interactions of aspect, cover and slope
hist(data_1000$slope)
slo_north_cov <- predict_response(
  mod_bayes_beta02, c("cover", "northness [-1, 0, 1]", "slope[-0.382, 4.126]"),
  margin = "mean_mode" # !Check if "mean_mode" is corret here.
)
plot(slo_north_cov)

# Interactions of aspect and slope.
north_slope <- predict_response(
  mod_bayes_beta02, c("slope", "northness"),
  margin = "mean_mode"
)
plot(north_slope)

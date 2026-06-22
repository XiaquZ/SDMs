library(brms)
library(dplyr)
library(tidybayes)
library(ggplot2)
library(svglite)
# install.packages("svglite")
# install.packages("tidybayes")
# Load model
load("I:/DATA/output/MF_SDMs/Models/MF_av_AllSpecies_betaRegPhi_withoutIntercept.rda")

# Use the correct response variable name from your current model
response_var <- "MF_av_AllSpecies"

model_data <- as_tibble(mod_bayes_beta_all$data) %>%
  mutate(.row = row_number())

# Beta regression cannot handle exact 0 or 1 on the logit scale.
n_obs <- nrow(model_data)

model_data <- model_data %>%
  mutate(
    y_obs = .data[[response_var]],

    # Smithson & Verkuilen style adjustment for beta-type responses
    # This moves exact 0 and 1 slightly inside the open interval (0, 1)
    y_obs_adj = (y_obs * (n_obs - 1) + 0.5) / n_obs,

    y_obs_link = qlogis(y_obs_adj)
  )

# Full fitted linear predictor for mu, on the link scale
eta_full <- posterior_linpred(
  mod_bayes_beta_all,
  newdata = model_data,
  transform = FALSE,
  re_formula = NA,
  dpar = "mu"
)

# Posterior mean full eta
model_data$eta_full_mean <- colMeans(eta_full)

# Link-scale residual
model_data <- model_data %>%
  mutate(
    resid_link = y_obs_link - eta_full_mean
  )

#### Helper function for partial residuals:
make_partial_resid <- function(model, data, focal_vars, fixed_values = list()) {
  
  newdata <- data
  
  standardized_vars <- c(
    "northness", "slope", "cover", "eastness",
    "coast", "elevation", "relative_elevation", "TWI"
  )
  
  standardized_vars <- standardized_vars[standardized_vars %in% names(newdata)]
  
  for (v in standardized_vars) {
    if (!v %in% focal_vars) {
      newdata[[v]] <- 0
    }
  }
  
  # x and y are map coordinates, so do not set them to 0.
  # Use the mean coordinate as a representative spatial location.
  if ("x" %in% names(newdata) && !"x" %in% focal_vars) {
    newdata$x <- mean(data$x, na.rm = TRUE)
  }
  
  if ("y" %in% names(newdata) && !"y" %in% focal_vars) {
    newdata$y <- mean(data$y, na.rm = TRUE)
  }
  
  if ("type" %in% names(newdata) && !"type" %in% focal_vars) {
    newdata$type <- factor(
      levels(newdata$type)[1],
      levels = levels(newdata$type)
    )
  }
  
  if (length(fixed_values) > 0) {
    for (v in names(fixed_values)) {
      newdata[[v]] <- fixed_values[[v]]
    }
  }
  
  eta_focal <- posterior_linpred(
    model,
    newdata = newdata,
    transform = FALSE,
    re_formula = NA,
    dpar = "mu"
  )
  
  eta_focal_mean <- colMeans(eta_focal)
  
  data %>%
    mutate(
      eta_focal_mean = eta_focal_mean,
      partial_resid_link = eta_focal_mean + resid_link,
      partial_resid_response = plogis(partial_resid_link)
    )
}

#### Back-transform cover to original scale for plotting
# Get mean and sd.
# Load the original, unstandardized predictor data
predict_orig <- readRDS(
  "I:/DATA/output/MF_SDMs/10000samples_orig_MF_9predicts.rds"
)

predict_orig <- as_tibble(predict_orig)

# Make sure numeric variables are numeric
predict_orig <- predict_orig %>%
  mutate(
    cover = as.numeric(cover),
    slope = as.numeric(slope),
    northness = as.numeric(northness),
    eastness = as.numeric(eastness),
    coast = as.numeric(coast),
    elevation = as.numeric(elevation),
    relative_elevation = as.numeric(relative_elevation),
    TWI = as.numeric(TWI)
  )

# Back-transformation parameters
mean_cover <- mean(predict_orig$cover, na.rm = TRUE)
sd_cover   <- sd(predict_orig$cover, na.rm = TRUE)

mean_slope <- mean(predict_orig$slope, na.rm = TRUE)
sd_slope   <- sd(predict_orig$slope, na.rm = TRUE)

mean_northness <- mean(predict_orig$northness, na.rm = TRUE)
sd_northness   <- sd(predict_orig$northness, na.rm = TRUE)

mean_eastness <- mean(predict_orig$eastness, na.rm = TRUE)
sd_eastness   <- sd(predict_orig$eastness, na.rm = TRUE)

mean_coast <- mean(predict_orig$coast, na.rm = TRUE)
sd_coast   <- sd(predict_orig$coast, na.rm = TRUE)

mean_elevation <- mean(predict_orig$elevation, na.rm = TRUE)
sd_elevation   <- sd(predict_orig$elevation, na.rm = TRUE)

mean_rela_elevation <- mean(predict_orig$relative_elevation, na.rm = TRUE)
sd_rela_elevation   <- sd(predict_orig$relative_elevation, na.rm = TRUE)

mean_TWI <- mean(predict_orig$TWI, na.rm = TRUE)
sd_TWI   <- sd(predict_orig$TWI, na.rm = TRUE)

#### Create the original values for the variables.
model_data <- model_data %>%
  mutate(
    cover_original = cover * sd_cover + mean_cover,
    slope_original = slope * sd_slope + mean_slope,
    northness_original = northness * sd_northness + mean_northness,
    eastness_original = eastness * sd_eastness + mean_eastness,
    coast_original = coast * sd_coast + mean_coast,
    elevation_original = elevation * sd_elevation + mean_elevation,
    relative_elevation_original = relative_elevation * sd_rela_elevation + mean_rela_elevation,
    TWI_original = TWI * sd_TWI + mean_TWI
  )

# Extract the 95% credible intervals.
fixef_tab <- as.data.frame(fixef(mod_bayes_beta_all, summary = TRUE))
fixef_tab

##### Exaple 1: cover * forest type
ci_cf <- round(
  fixef_tab["cover:typeConiferousforest", c("Q2.5", "Q97.5")],
  2
)

ci95_covertype <- paste0("CI: [", ci_cf[1], ", ", ci_cf[2], "]")

eff_cf <- conditional_effects(
  mod_bayes_beta_all,
  effects = "cover:type",
  re_formula = NA
)[["cover:type"]] %>%
  mutate(
    cover_original = cover * sd_cover + mean_cover
  )

model_data_cover_type <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = c("cover", "type")
)

model_data_cover_type <- model_data_cover_type %>%
  mutate(
    cover_original = cover * sd_cover + mean_cover
  )

# Create and save the plot.
p_cover_type <- ggplot() +
  geom_point(
    data = model_data_cover_type,
    aes(
      x = cover_original,
      y = partial_resid_response,
      color = type
    ),
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = eff_cf,
    aes(
      x = cover_original,
      ymin = lower__,
      ymax = upper__,
      fill = type
    ),
    alpha = 0.5
  ) +
  geom_line(
    data = eff_cf,
    aes(
      x = cover_original,
      y = estimate__,
      color = type
    ),
    linewidth = 1.5
  ) +
  labs(
    x = "Cover (%)",
    y = "Multifunctionality index (average)",
    title = paste0("(a) Cover × Forest type\n", ci95_covertype),
    color = "Forest type",
    fill = "Forest type"
  ) +
  coord_cartesian(ylim = c(0.2, 1.0)) +
  scale_color_manual(
    values = c(
      "Broadleaved forest" = "#5159CA",
      "Coniferous forest" = "#C8CA46"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Broadleaved forest" = "#5159CA",
      "Coniferous forest" = "#C8CA46"
    )
  ) +
  theme_light() +
 theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.title = element_text(size = 28, face = "bold"),
    legend.text = element_text(size = 28),
    plot.margin = margin(10, 10, 10, 10),

    # bottom right legend inside the plotting panel
    legend.position = c(0.98, 0.08),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA)
  )
p_cover_type
# Save the plot
ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/cover_type_partial_residual.svg",
  plot = p_cover_type,
  width = 9,
  height = 9,
  units = "in",
  device = svglite
)

#### Example 2: distance to coast. ####
# Extract CI for the coast coefficient
ci_coast <- round(
  fixef_tab["coast", c("Q2.5", "Q97.5")],
  2
)

ci95_coast <- paste0("CI: [", ci_coast[1], ", ", ci_coast[2], "]")
ci95_coast
# Marginal effect of distance to coast
eff_coast <- conditional_effects(
  mod_bayes_beta_all,
  effects = "coast",
  re_formula = NA
)[["coast"]] %>%
  mutate(
    coast_original = coast * sd_coast + mean_coast
  )

# Points show partial residuals on the response scale; 
#lines and shaded areas show posterior mean predictions 
# and 95% credible intervals.
model_data_coast <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = "coast"
) %>%
  mutate(
    coast_original = coast * sd_coast + mean_coast
  )

# Plot for distance to coast.
p_coast <- ggplot() +
  geom_point(
    data = model_data_coast,
    aes(
      x = coast_original,
      y = partial_resid_response
    ),
    color = "#99914B",
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = eff_coast,
    aes(
      x = coast_original,
      ymin = lower__,
      ymax = upper__
    ),
    fill = "#99914B",
    alpha = 0.5
  ) +
  geom_line(
    data = eff_coast,
    aes(
      x = coast_original,
      y = estimate__
    ),
    color = "#99914B",
    linewidth = 1.5
  ) +
  labs(
    x = "Distance to the coast (km)",
    y = "Multifunctionality index (average)",
    title = paste0("(d) Effect of distance to coast\n", ci95_coast)
  ) +
  coord_cartesian(ylim = c(0.1, 1.02)) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

p_coast
ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/coast_partial_residual.svg",
  plot = p_coast,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)

#### Example 3: cover × northness at slope = 5°
# Standardized slope value for 5 degrees
slope_std5 <- (5 - mean_slope) / sd_slope

# Representative northness values, transformed to the standardized scale
north <- (1 - mean_northness) / sd_northness
south <- (-1 - mean_northness) / sd_northness
flat <- (0 - mean_northness) / sd_northness

northness_vals <- c(south, flat, north)

# Extract posterior draws
posterior <- as_draws_df(mod_bayes_beta_all)

# Conditional cover × northness interaction at slope = 5°
# This is the interaction between cover and northness when slope is fixed at 5 degrees.
cover_north_interaction_slope5 <- posterior %>%
  transmute(
    effect = `b_northness:cover` +
      `b_northness:slope:cover` * slope_std5
  )

ci_cns5 <- round(
  quantile(
    cover_north_interaction_slope5$effect,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  ),
  2
)

ci95_cns5 <- paste0("CI: [", ci_cns5[1], ", ", ci_cns5[2], "]")
ci95_cns5

# Marginal effect of cover at slope = 5° and different northness values
eff_cns5 <- conditional_effects(
  mod_bayes_beta_all,
  effects = "cover",
  conditions = data.frame(
    slope = slope_std5,
    northness = northness_vals
  ),
  re_formula = NA
)[["cover"]] %>%
  mutate(
    cover_original = cover * sd_cover + mean_cover,
    northness_original = northness * sd_northness + mean_northness,
    aspect_group = case_when(
      abs(northness_original + 1) < 1e-6 ~ "South",
      abs(northness_original - 0) < 1e-6 ~ "Flat/Neutral",
      abs(northness_original - 1) < 1e-6 ~ "North",
      TRUE ~ as.character(northness_original)
    ),
    aspect_group = factor(
      aspect_group,
      levels = c("South", "Flat/Neutral", "North")
    )
  )

# Points show partial residuals on the response scale;
# lines and shaded areas show posterior mean predictions
# and 95% credible intervals.
model_data_cns5 <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = c("cover", "northness"),
  fixed_values = list(slope = slope_std5)
) %>%
  mutate(
    cover_original = cover * sd_cover + mean_cover,
    northness_original = northness * sd_northness + mean_northness,
    aspect_group = case_when(
      northness_original <= -0.8 ~ "South",
      abs(northness_original) <= 0.3 ~ "Flat/Neutral",
      northness_original >= 0.8 ~ "North",
      TRUE ~ NA_character_
    ),
    aspect_group = factor(
      aspect_group,
      levels = c("South", "Flat/Neutral", "North")
    )
  )

# Plot cover × northness at slope = 5°
p_cns5 <- ggplot() +
  geom_point(
    data = filter(model_data_cns5, !is.na(aspect_group)),
    aes(
      x = cover_original,
      y = partial_resid_response,
      color = aspect_group
    ),
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = filter(eff_cns5, !is.na(aspect_group)),
    aes(
      x = cover_original,
      ymin = lower__,
      ymax = upper__,
      fill = aspect_group
    ),
    alpha = 0.5
  ) +
  geom_line(
    data = filter(eff_cns5, !is.na(aspect_group)),
    aes(
      x = cover_original,
      y = estimate__,
      color = aspect_group
    ),
    linewidth = 1.5,
    linetype = "dashed"
  ) +
  labs(
    x = "Cover (%)",
    y = NULL,
    title = paste0("(b) Cover × Northness at slope = 5°\n", ci95_cns5),
    color = "Aspect",
    fill = "Aspect"
  ) +
  coord_cartesian(ylim = c(0.1, 1.02)) +
  scale_color_manual(
    values = c(
      "South" = "#D95F02",
      "Flat/Neutral" = "#7570B3",
      "North" = "#1B9E77"
    ),
    breaks = c("South", "Flat/Neutral", "North")
  ) +
  scale_fill_manual(
    values = c(
      "South" = "#D95F02",
      "Flat/Neutral" = "#7570B3",
      "North" = "#1B9E77"
    ),
    breaks = c("South", "Flat/Neutral", "North")
  ) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),

    legend.title = element_text(size = 28, face = "bold"),
    legend.text = element_text(size = 28),

    # Bottom-right legend inside plot
    legend.position = c(0.98, 0.08),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),

    plot.margin = margin(10, 10, 10, 10)
  )

p_cns5

ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/cover_northness_slope5_partial_residual.svg",
  plot = p_cns5,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)


#### Example 3.2: cover × northness at slope = 45° ####
# Standardized slope value for 45 degrees
slope_std45 <- (45 - mean_slope) / sd_slope

# Representative northness values, transformed to the standardized scale
north <- (1 - mean_northness) / sd_northness
south <- (-1 - mean_northness) / sd_northness
flat <- (0 - mean_northness) / sd_northness

northness_vals <- c(south, flat, north)

# Extract posterior draws
posterior <- as_draws_df(mod_bayes_beta_all)

# Conditional cover × northness interaction at slope = 45°
# This is the interaction between cover and northness when slope is fixed at 45 degrees.
cover_north_interaction_slope45 <- posterior %>%
  transmute(
    effect = `b_northness:cover` +
      `b_northness:slope:cover` * slope_std45
  )

ci_cns45 <- round(
  quantile(
    cover_north_interaction_slope45$effect,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  ),
  2
)

ci95_cns45 <- paste0("CI: [", ci_cns45[1], ", ", ci_cns45[2], "]")
ci95_cns45

# Marginal effect of cover at slope = 45° and different northness values
eff_cns45 <- conditional_effects(
  mod_bayes_beta_all,
  effects = "cover",
  conditions = data.frame(
    slope = slope_std45,
    northness = northness_vals
  ),
  re_formula = NA
)[["cover"]] %>%
  mutate(
    cover_original = cover * sd_cover + mean_cover,
    northness_original = northness * sd_northness + mean_northness,
    aspect_group = case_when(
      abs(northness_original + 1) < 1e-6 ~ "South",
      abs(northness_original - 0) < 1e-6 ~ "Flat/Neutral",
      abs(northness_original - 1) < 1e-6 ~ "North",
      TRUE ~ as.character(northness_original)
    ),
    aspect_group = factor(
      aspect_group,
      levels = c("South", "Flat/Neutral", "North")
    )
  )

# Points show partial residuals on the response scale;
# lines and shaded areas show posterior mean predictions
# and 95% credible intervals.
model_data_cns45 <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = c("cover", "northness"),
  fixed_values = list(slope = slope_std45)
) %>%
  mutate(
    cover_original = cover * sd_cover + mean_cover,
    northness_original = northness * sd_northness + mean_northness,
    aspect_group = case_when(
      northness_original <= -0.8 ~ "South",
      abs(northness_original) <= 0.3 ~ "Flat/Neutral",
      northness_original >= 0.8 ~ "North",
      TRUE ~ NA_character_
    ),
    aspect_group = factor(
      aspect_group,
      levels = c("South", "Flat/Neutral", "North")
    )
  )

# Plot cover × northness at slope = 45°
p_cns45 <- ggplot() +
  geom_point(
    data = filter(model_data_cns45, !is.na(aspect_group)),
    aes(
      x = cover_original,
      y = partial_resid_response,
      color = aspect_group
    ),
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = filter(eff_cns45, !is.na(aspect_group)),
    aes(
      x = cover_original,
      ymin = lower__,
      ymax = upper__,
      fill = aspect_group
    ),
    alpha = 0.5
  ) +
  geom_line(
    data = filter(eff_cns45, !is.na(aspect_group)),
    aes(
      x = cover_original,
      y = estimate__,
      color = aspect_group
    ),
    linewidth = 1.5,
    linetype = "dashed"
  ) +
  labs(
    x = "Cover (%)",
    y = NULL,
    title = paste0("(c) Cover × Northness at slope = 45°\n", ci95_cns45),
    color = "Aspect",
    fill = "Aspect"
  ) +
  coord_cartesian(ylim = c(0.1, 1.02)) +
  scale_color_manual(
    values = c(
      "South" = "#D95F02",
      "Flat/Neutral" = "#7570B3",
      "North" = "#1B9E77"
    ),
    breaks = c("South", "Flat/Neutral", "North")
  ) +
  scale_fill_manual(
    values = c(
      "South" = "#D95F02",
      "Flat/Neutral" = "#7570B3",
      "North" = "#1B9E77"
    ),
    breaks = c("South", "Flat/Neutral", "North")
  ) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),

    legend.title = element_text(size = 28, face = "bold"),
    legend.text = element_text(size = 28),

    # Bottom-right legend inside plot
    legend.position = c(0.98, 0.08),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),

    plot.margin = margin(10, 10, 10, 10)
  )

p_cns45

ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/cover_northness_slope45_partial_residual.svg",
  plot = p_cns45,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)

#### Example 5: Effect of slope ####

# Extract CI for the slope coefficient
ci_slope <- round(
  fixef_tab["slope", c("Q2.5", "Q97.5")],
  2
)

ci95_slope <- paste0("CI: [", ci_slope[1], ", ", ci_slope[2], "]")
ci95_slope

# Marginal effect of slope
eff_slope <- conditional_effects(
  mod_bayes_beta_all,
  effects = "slope",
  re_formula = NA
)[["slope"]] %>%
  mutate(
    slope_original = slope * sd_slope + mean_slope
  )

# Points show partial residuals on the response scale;
# lines and shaded areas show posterior mean predictions
# and 95% credible intervals.
model_data_slope <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = "slope"
) %>%
  mutate(
    slope_original = slope * sd_slope + mean_slope
  )

# Plot for slope
p_slope <- ggplot() +
  geom_point(
    data = model_data_slope,
    aes(
      x = slope_original,
      y = partial_resid_response
    ),
    color = "#994b8f",
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = eff_slope,
    aes(
      x = slope_original,
      ymin = lower__,
      ymax = upper__
    ),
    fill = "#994b8f",
    alpha = 0.5
  ) +
  geom_line(
    data = eff_slope,
    aes(
      x = slope_original,
      y = estimate__
    ),
    color = "#994b8f",
    linewidth = 1.5
  ) +
  labs(
    x = "Slope (°)",
    y = NULL,
    title = paste0("(e) Effect of slope\n", ci95_slope)
  ) +
  coord_cartesian(ylim = c(0.1, 1.02)) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

p_slope

ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/slope_partial_residual.svg",
  plot = p_slope,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)

#### Example 6: Effect of elevation ####

# Extract CI for the elevation coefficient
ci_elevation <- round(
  fixef_tab["elevation", c("Q2.5", "Q97.5")],
  2
)

ci95_elevation <- paste0("CI: [", ci_elevation[1], ", ", ci_elevation[2], "]")
ci95_elevation

# Marginal effect of elevation
eff_elevation <- conditional_effects(
  mod_bayes_beta_all,
  effects = "elevation",
  re_formula = NA
)[["elevation"]] %>%
  mutate(
    elevation_original = elevation * sd_elevation + mean_elevation
  )

# Points show partial residuals on the response scale;
# lines and shaded areas show posterior mean predictions
# and 95% credible intervals.
model_data_elevation <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = "elevation"
) %>%
  mutate(
    elevation_original = elevation * sd_elevation + mean_elevation
  )

# Plot for elevation
p_elevation <- ggplot() +
  geom_point(
    data = model_data_elevation,
    aes(
      x = elevation_original,
      y = partial_resid_response
    ),
    color = "#5c64cf",
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = eff_elevation,
    aes(
      x = elevation_original,
      ymin = lower__,
      ymax = upper__
    ),
    fill = "#5c64cf",
    alpha = 0.5
  ) +
  geom_line(
    data = eff_elevation,
    aes(
      x = elevation_original,
      y = estimate__
    ),
    color = "#5c64cf",
    linewidth = 1.5,
    linetype = "dashed"
  ) +
  labs(
    x = "Elevation (m)",
    y = NULL,
    title = paste0("(f) Effect of elevation\n", ci95_elevation)
  ) +
  coord_cartesian(ylim = c(0.1, 1.02)) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

p_elevation

ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/elevation_partial_residual.svg",
  plot = p_elevation,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)

#### Example 7: Effect of relative elevation ####

# Extract CI for the relative elevation coefficient
ci_rela_elevation <- round(
  fixef_tab["relative_elevation", c("Q2.5", "Q97.5")],
  2
)

ci95_rela_elevation <- paste0(
  "CI: [", ci_rela_elevation[1], ", ", ci_rela_elevation[2], "]"
)

ci95_rela_elevation

# Marginal effect of relative elevation
eff_rela_elevation <- conditional_effects(
  mod_bayes_beta_all,
  effects = "relative_elevation",
  re_formula = NA
)[["relative_elevation"]] %>%
  mutate(
    relative_elevation_original =
      relative_elevation * sd_rela_elevation + mean_rela_elevation
  )

# Points show partial residuals on the response scale;
# lines and shaded areas show posterior mean predictions
# and 95% credible intervals.
model_data_rela_elevation <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = "relative_elevation"
) %>%
  mutate(
    relative_elevation_original =
      relative_elevation * sd_rela_elevation + mean_rela_elevation
  )

# Plot for relative elevation
p_rela_elevation <- ggplot() +
  geom_point(
    data = model_data_rela_elevation,
    aes(
      x = relative_elevation_original,
      y = partial_resid_response
    ),
    color = "#dabb56",
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = eff_rela_elevation,
    aes(
      x = relative_elevation_original,
      ymin = lower__,
      ymax = upper__
    ),
    fill = "#dabb56",
    alpha = 0.5
  ) +
  geom_line(
    data = eff_rela_elevation,
    aes(
      x = relative_elevation_original,
      y = estimate__
    ),
    color = "#dabb56",
    linewidth = 1.5
  ) +
  labs(
    x = "Relative elevation (m)",
    y = "Multifunctionality index (average)",
    title = paste0("(g) Effect of relative elevation\n", ci95_rela_elevation)
  ) +
  coord_cartesian(
    xlim = c(0, 500),
    ylim = c(0.1, 1.02)
  ) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

p_rela_elevation

ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/relative_elevation_partial_residual.svg",
  plot = p_rela_elevation,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)

#### Example 8: Effect of eastness ####

# Extract CI for the eastness coefficient
ci_eastness <- round(
  fixef_tab["eastness", c("Q2.5", "Q97.5")],
  2
)

ci95_eastness <- paste0(
  "CI: [", ci_eastness[1], ", ", ci_eastness[2], "]"
)

ci95_eastness

# Marginal effect of eastness
eff_eastness <- conditional_effects(
  mod_bayes_beta_all,
  effects = "eastness",
  re_formula = NA
)[["eastness"]] %>%
  mutate(
    eastness_original = eastness * sd_eastness + mean_eastness
  )
# Points show partial residuals on the response scale;
# lines and shaded areas show posterior mean predictions
# and 95% credible intervals.
model_data_eastness <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = "eastness"
) %>%
  mutate(
    eastness_original = eastness * sd_eastness + mean_eastness
  )

# Plot for eastness
p_eastness <- ggplot() +
  geom_point(
    data = model_data_eastness,
    aes(
      x = eastness_original,
      y = partial_resid_response
    ),
    color = "#5b978d",
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = eff_eastness,
    aes(
      x = eastness_original,
      ymin = lower__,
      ymax = upper__
    ),
    fill = "#5b978d",
    alpha = 0.5
  ) +
  geom_line(
    data = eff_eastness,
    aes(
      x = eastness_original,
      y = estimate__
    ),
    color = "#5b978d",
    linewidth = 1.5,
    linetype = "dashed"
  ) +
  labs(
    x = "Eastness",
    y = NULL,
    title = paste0("(h) Effect of eastness\n", ci95_eastness)
  ) +
  coord_cartesian(
    xlim = c(-1, 1),
    ylim = c(0.1, 1.02)
  ) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

p_eastness

ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/eastness_partial_residual.svg",
  plot = p_eastness,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)

#### Example 9: Effect of TWI ####

# Extract CI for the TWI coefficient
ci_TWI <- round(
  fixef_tab["TWI", c("Q2.5", "Q97.5")],
  2
)

ci95_TWI <- paste0(
  "CI: [", ci_TWI[1], ", ", ci_TWI[2], "]"
)

ci95_TWI

# Marginal effect of TWI
eff_TWI <- conditional_effects(
  mod_bayes_beta_all,
  effects = "TWI",
  re_formula = NA
)[["TWI"]] %>%
  mutate(
    TWI_original = TWI * sd_TWI + mean_TWI
  )

# Points show partial residuals on the response scale;
# lines and shaded areas show posterior mean predictions
# and 95% credible intervals.
model_data_TWI <- make_partial_resid(
  model = mod_bayes_beta_all,
  data = model_data,
  focal_vars = "TWI"
) %>%
  mutate(
    TWI_original = TWI * sd_TWI + mean_TWI
  )

# Plot for TWI
p_TWI <- ggplot() +
  geom_point(
    data = model_data_TWI,
    aes(
      x = TWI_original,
      y = partial_resid_response
    ),
    color = "#AA6646",
    alpha = 0.1,
    size = 2.8,
    shape = 16
  ) +
  geom_ribbon(
    data = eff_TWI,
    aes(
      x = TWI_original,
      ymin = lower__,
      ymax = upper__
    ),
    fill = "#AA6646",
    alpha = 0.5
  ) +
  geom_line(
    data = eff_TWI,
    aes(
      x = TWI_original,
      y = estimate__
    ),
    color = "#AA6646",
    linewidth = 1.5,
    linetype = "dashed"
  ) +
  labs(
    x = "Topographic wetness index (TWI)",
    y = NULL,
    title = paste0("(i) Effect of TWI\n", ci95_TWI)
  ) +
  coord_cartesian(
    ylim = c(0.1, 1.02)
  ) +
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 28),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32, face = "bold"),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

p_TWI

ggsave(
  filename = "I:/Figures_SDMpaper/MF_allSpecies_9predictors/TWI_partial_residual.svg",
  plot = p_TWI,
  width = 9,
  height = 9,
  units = "in",
  device = svglite::svglite
)

library(terra)

#### Correlation metrics of variables. ####
# Load the variables.
bio5 <- rast("D:/SDMs/SDMs_current/Predictors/Micro_BIO5_EU_CHELSAbased_2000-2020.tif")
bio6 <- rast("D:/SDMs/SDMs_current/Predictors/Micro_BIO6_EU_CHELSAbased_2000-2020.tif")
bio12 <- rast("D:/SDMs/SDMs_current/Predictors/CHELSA_bio12_EU_2000-2019.tif")
bio15 <- rast("D:/SDMs/SDMs_current/Predictors/CHELSA_bio15_EU_2000-2019.tif")
clay <- rast("D:/SDMs/SDMs_current/Predictors/clay.tif")
cec <- rast("D:/SDMs/SDMs_current/Predictors/cec.tif")
phh2o <- rast("D:/SDMs/SDMs_current/Predictors/phh2o_0_30_WeightedMean.tif")
eleva <- rast("D:/SDMs/SDMs_current/Predictors/Elevation.tif")
slope <- rast("D:/SDMs/SDMs_current/Predictors/Slope.tif")
twi <- rast("D:/SDMs/SDMs_current/Predictors/TWI.tif")

# Stack the variables.
vars_stack <- c(bio5, bio6, bio12, bio15, clay, cec, phh2o, eleva, slope, twi)
names(vars_stack) <- c(
    "Bio5", "Bio6", "Bio12", "Bio15",
    "Clay", "CEC", "pH_H2O", "Elevation", "Slope", "TWI"
)
set.seed(123)
samp_vars <- spatSample(
    vars_stack, 3000,
    xy = TRUE, "random", na.rm = TRUE, exhaustive = TRUE
)
#### Correlation test.####
library(ggcorrplot)
library(dplyr)

# Select the relevant variables for correlation
df_selected <- samp_vars[, 3:12]  # Keeping only the relevant columns

# Function to compute p-values for correlation matrix
cor.mtest <- function(mat, method = "spearman") {
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- cor.test(mat[, i], mat[, j], method = method, use = "pairwise.complete.obs")
      p.mat[i, j] <- p.mat[j, i] <- test$p.value
    }
  }
  diag(p.mat) <- 0  # Self-correlation has no p-value
  return(p.mat)
}

# Compute Spearman correlation matrix
cor_matrix <- cor(df_selected, method = "spearman", use = "pairwise.complete.obs")

# Compute p-values
p_matrix <- cor.mtest(df_selected, method = "spearman")

# Define new variable names
new_names <- c("BIO5", "BIO6", "BIO12", "BIO15", "Clay", 
               "CEC", "pH_soil", "Elevation", "Slope",
               "TWI")

# Rename the correlation matrix
colnames(cor_matrix) <- rownames(cor_matrix) <- new_names

# Create correlation plot with significance mask
svg("I:/SVG/Cor_matrix_EnvsPredictors.svg")

ggcorrplot(cor_matrix, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3.5, 
           show.diag = FALSE, 
           p.mat = p_matrix,  # Add p-values
           sig.level = 0.05,  # Only show significant correlations (p < 0.05)
           insig = "blank",   # Hide non-significant values
           colors = c("blue", "white", "red"))  # Adjust colors

dev.off()


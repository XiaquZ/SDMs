library(raster)
library(dplyr)
library(MASS)
library(ENMeval)
library(prodlim)
library(rgdal)

#####################################################################################
########################              Running SDMs           ######################## 
#####################################################################################
# Loading the shapefile of Europe (needed for calculation of the 2D kernel estimate)
# Loading macro-environmental data
Europe <- shapefile("D:/Ecology Letters/02_Variables/00_Shapefiles/01_Europe/Europe.shp")

covariates_macro <- 
  raster::stack(
    c(
      TerraClim_05 <- raster("D:/Ecology Letters/02_Variables/01_Macro/TerraClim_05.tif"),
      TerraClim_06 <- raster("D:/Ecology Letters/02_Variables/01_Macro/TerraClim_06.tif"),
      TerraClim_12 <- raster("D:/Ecology Letters/02_Variables/01_Macro/TerraClim_12.tif"),
      TerraClim_15 <- raster("D:/Ecology Letters/02_Variables/01_Macro/TerraClim_15.tif"),
      cec <- raster("D:/Ecology Letters/02_Variables/01_Macro/cec.tif"),
      clay <- raster("D:/Ecology Letters/02_Variables/01_Macro/clay.tif")
    )
  )

names(covariates_macro) <- c("TerraClim_05", "TerraClim_06", "TerraClim_12", "TerraClim_15", "cec", "clay")
covariates_macro_df <- as.data.frame(covariates_macro)

# Loading the cleaned occurrence data
occurrence.files <- list.files("D:/Ecology Letters/01_Occurrences/02_Clean/", pattern = "\\.csv$", full.names = TRUE)
occurrence.names <- list.files("D:/Ecology Letters/01_Occurrences/02_Clean/", pattern = "\\.csv$", full.names = FALSE)
occurrence.tif <- gsub(".csv", ".tif", occurrence.names)

################################################################################################
########################            Template for SDM predictions         ####################### 
################################################################################################
# Make predictions with our best model at the end
# Here, we make some objects that are needed in the code
### Create a template raster to fill with the predictions
covs.files <- c("D:/Ecology Letters/02_Variables/01_Macro/TerraClim_05.tif",
                "D:/Ecology Letters/02_Variables/01_Macro/TerraClim_06.tif",
                "D:/Ecology Letters/02_Variables/01_Macro/TerraClim_12.tif",
                "D:/Ecology Letters/02_Variables/01_Macro/TerraClim_15.tif",
                "D:/Ecology Letters/02_Variables/01_Macro/cec.tif",
                "D:/Ecology Letters/02_Variables/01_Macro/clay.tif")

newdata <- readGDAL(covs.files[1], silent = TRUE)

for(j in 2:length(covs.files)){
  newdata@data[,j] <- readGDAL(covs.files[j], silent = TRUE)$band1
}

names(newdata) <- c("TerraClim_05", "TerraClim_06", "TerraClim_12", "TerraClim_15", "cec", "clay")
newdata$grid <- 15

sel.pr <- complete.cases(newdata@data)
indices <- which(sel.pr)

g <- as(newdata["grid"], "SpatialPixelsDataFrame")

# Only retain relevant objects to be used in the loop
rm(list=ls()[! ls() %in% c("covariates_macro", "occurrence.files", "occurrence.names", 
                           "occurrence.tif", "Europe", "indices", "g", "covariates_macro_df")])

########################################################################
########################            SDMs         ####################### 
########################################################################

for(r in 1:length(occurrence.files)) {
  
  print(r)
  
  species_of_interest <- read.csv(occurrence.files[r], header = TRUE, sep = ",")

  # First we make a random 80-20 split
  # 80% for internal calibration + cross-validation
  # 20% for external validation + metric calculations
  # We take a random sample so we first set.seed(1)
  set.seed(1)
  
  rows.int <- sample(1:nrow(species_of_interest), replace = FALSE, size = 0.8*nrow(species_of_interest))
  internal <- species_of_interest[rows.int, ]
  external <- species_of_interest[-rows.int, ]

  ########################################################
  ##############     2D Kernel estimate     ##############
  ########################################################
  # Create bias kernel density raster of two-dimensional kernel density estimate of sampling bias across study extent
  # Convert all NA's to zero-values, otherwise we cannot sample from the layer later on
  bias <- kde2d(internal[, "Longitude"], internal[, "Latitude"], 
                n = c(ncol(covariates_macro), nrow(covariates_macro)),
                lims = c(range(coordinates(covariates_macro)[, 1]), range(coordinates(covariates_macro)[, 2]))) # limits of density funtion
  bias.ras <- raster(bias)
  names(bias.ras) <- "Prob"
  bias.ras <- mask(bias.ras, Europe)
  bias.ras[is.na(bias.ras[])] <- 0 
  
  ####################################################################
  ##############        occurrence & background         ##############
  ####################################################################
  # Extract the macro-environmental data at the occurrence points
  occs.z <- cbind(internal[, 2:3], raster::extract(covariates_macro, internal[, 2:3]))
  occs.z <- na.omit(occs.z)

  # Select background points using 2D kernel estimate
  # We take a random sample so we first set.seed(1)
  set.seed(1)
  bg <- as.data.frame(xyFromCell(bias.ras, sample(ncell(bias.ras), 3*nrow(occs.z), prob = values(bias.ras))))
  bg$prob <- raster::extract(bias.ras, bg)
  bg.z <- cbind(bg, raster::extract(covariates_macro, bg[, c("x", "y")]))
  bg.z <- na.omit(bg.z)
  colnames(bg.z) <- c("Longitude", "Latitude", "Prob", "TerraClim_05", "TerraClim_06", "TerraClim_12", "TerraClim_15", "cec", "clay")
 
  set.seed(1)
  bg.z <- bg.z[sample(nrow(bg.z), nrow(occs.z), prob = bg.z$Prob), ]
  bg.z <- bg.z[, c("Longitude", "Latitude", "TerraClim_05", "TerraClim_06", "TerraClim_12", "TerraClim_15", "cec", "clay")]

  ####################################################
  ##############        ENMeval         ##############
  ####################################################
  # We use the blocked method and the feature classes "linear", "quadratic", "product"
  # No raster data (a.k.a, samples with data, or SWD): no full model raster predictions created, so will run faster!
  # Afterwards predict to raster for the best model only
  set.seed(1)
  e.mx_rp.f <- ENMevaluate(occs = occs.z, 
                           bg = bg.z, 
                           algorithm = "maxnet", 
                           partitions = "block", 
                           tune.args = list(fc = c("L","Q","P", "LQ", "QP", "LP", "LQP"), rm = c(0.5, 1, 2, 3, 4, 5)), 
                           parallel = TRUE, 
                           numCores = 10)
  
  #####################################################################################
  ########################            Model selection          ######################## 
  #####################################################################################
  # Get the results of the SDM
  res <- eval.results(e.mx_rp.f)
  
  # Select the best SDM based on delta AIC
  # This dplyr operation executes the sequential criteria explained above.
  opt.seq <- filter(res, delta.AICc == min(delta.AICc))
  
  # Get the row of the best model
  best.mdl <- row.match(opt.seq, res)
  
  #####################################################################################
  ########################            Model prediction          ####################### 
  ##################################################################################### 
  # We make the predictions in a matrix
  # Later on, we convert the matrix to a spatial object
  logistic <- matrix(NA, nrow = length(indices), ncol = length(best.mdl))

  # If there are multiple "best" models, we average the predictions of all the best performing models
  for(q in 1:length(best.mdl)){
    
    logistic.mdl <- predict(object = e.mx_rp.f@models[[best.mdl[q]]], newdata = covariates_macro_df[indices, ], type = "logistic")
    logistic.mdl <- logistic.mdl*100
    logistic.mdl <- round(logistic.mdl, digits = 0)
    logistic[, q] <- logistic.mdl
  
  }
  
  logistic <- rowMeans(logistic)
  
  # Convert logistic matrix into a logistic map
  g[indices,"logistic"] <- logistic
  logistic.ras <- raster(g["logistic"])
  plot(logistic.ras)
  
  # Calculate the 10% training presence as a threshold (= P10)
  occs.s <- cbind(internal[, 2:3], raster::extract(logistic.ras, internal[, 2:3]))
  colnames(occs.s) <- c("Longitude", "Latitude", "Probability")
  or.10p.avg <- as.numeric(quantile(occs.s$Probability, probs = c(0.1), na.rm = TRUE))
  
  # Convert all the values below the 10% threshold to 0
  # Convert all the values above the 10% threshold to 1
  # This means that at least 90% of the occurrence points are categorized as suitable
  binary.ras <- logistic.ras
  binary.ras[binary.ras <= or.10p.avg] <- 0
  binary.ras[binary.ras > or.10p.avg] <- 1
  plot(binary.ras)

  # Both the logistic and binary maps can be saved for further use
  # writeRaster(logistic.ras, paste0("path-to-directory", occurrence.tif[r]), overwrite = TRUE)
  # writeRaster(binary.ras, paste0("path-to-directory", occurrence.tif[r]), overwrite = TRUE)
  
  ###########################################
  ###########################################
  #####        Calculate the CBI        #####
  ###########################################
  ###########################################
  
  # Extract the predictions on the validation points
  # There can be no NA's in the vector
  obs <- extract(logistic.ras, external[, c("Longitude", "Latitude")])
  obs <- na.omit(obs)
  
  # Create a vector of all predictions
  # There can be no NA's in the vector
  set.seed(1)
  logistic.vct <- sampleRandom(logistic.ras, 10000000, na.rm = TRUE)
  
  # Calculate the CBI of our model
  ecospat.boyce(fit = logistic.vct, obs = obs)$cor
  
  ###############################################################
  ###############################################################
  #####               Calculate the sensitivity             #####
  ###############################################################
  ###############################################################
  
  # Calculate the sensitivity of our model
  sensitivity_df <- cbind(external[, 2:3], raster::extract(logistic.ras, external[, 2:3]))
  sensitivity_df <- na.omit(sensitivity_df)
  colnames(sensitivity_df) <- c("Longitude", "Latitude", "Predicted")
  sensitivity_df$Observed <- 1
  
  sensitivity_df$Predicted[sensitivity_df$Predicted <= or.10p.avg] <- 0
  sensitivity_df$Predicted[sensitivity_df$Predicted > or.10p.avg] <- 1
  
  conf_matrix <- table(sensitivity_df$Observed, sensitivity_df$Predicted)
  sensitivity <- conf_matrix[2] / (conf_matrix[1] + conf_matrix[2])
  
  rm(list=ls()[! ls() %in% c("covariates_macro", "occurrence.files", "occurrence.names", 
                             "occurrence.tif", "Europe", "indices", "g", "covariates_macro_df")])
  
}

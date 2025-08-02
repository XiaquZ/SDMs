library(ENMeval)
library(terra)
predict_CurrentSDM <- function(input_folders, mdl_paths) {
  # 1. Point to the folder where your tiles live:
  tile_dir <- input_folders[[1]]

  # 2. List only the cec_*.tif files
  tile_files <- list.files(tile_dir,
    pattern = ".tif",
    full.names = TRUE
  )

  # 3. Extract the numeric part of each filename
  tile_nums <- as.integer(sub("^cec_(\\d+)\\.tif$", "\\1", basename(tile_files)))
  # 4. Sort them (optional, but usually handy)
  o <- order(tile_nums)
  tile_nums <- tile_nums[o]
  tile_files <- tile_files[o]

  # Iterate through tiles (assumes tiles are numbered from 1 to 9)
  for (i in tile_nums) {
    # Initialize an empty list to store predictors for this tile
    # Define predictor keywords
    predictors <- c(
      "Micro_BIO5_EU_CHELSAbased_2000.2020", "Micro_BIO6_EU_CHELSAbased_2000.2020",
      "CHELSA_bio12_EU_2000.2019", "CHELSA_bio15_EU_2000.2019", "cec", "clay",
      "Slope", "Elevation"
    )

    # Get file paths of all predictors
    files <- c()
    # Iterate through predictors and read corresponding rasters
    for (predictor in predictors) {
      # Find the folder for the predictor
      folder <- grep(predictor, input_folders, value = TRUE)

      # Construct the file path
      files <- c(files, paste0(folder, predictor, "_", i, ".tif"))
    }

    # --- NEW: Check if all files exist for this tile ---
    if (!all(file.exists(files))) {
      message(paste0("Skipping tile ", i, " because one or more predictor files are missing."))
      next
    }

    # Create raster stack for predictors.
    stack_preds <- vrt(files, options = "-separate") # if there is no this i number tile, go to next tile.
    names(stack_preds) <- c(
      "Micro_BIO5_EU_CHELSAbased_2000.2020", "Micro_BIO6_EU_CHELSAbased_2000.2020",
      "CHELSA_bio12_EU_2000.2019", "CHELSA_bio15_EU_2000.2019", "cec", "clay",
      "Slope", "Elevation"
    )
    print(stack_preds)

    # Load one of the SDMs
    species_name <- gsub("_ENMeval_swd.RData", "", basename(mdl_paths))
    print(paste0("Start selecting lowest AIC model for: ", species_name))

    # Load model object
    mdl <- load(mdl_paths[[2]])
    mdl <- e.swd

    # Select the best SDM based on delta AIC
    res <- eval.results(mdl)
    min_index <- which(res$delta.AICc == min(res$delta.AICc))


    if (length(min_index) == 1) {
      mdl_select <- mdl@models[[min_index]]
    } else {
      warning(paste0(species_name, " has more than one selected model"))
      mdl_select <- mdl@models[[min_index]]
    }

    # Predict the future distribution for each raster tile
    print(paste0(
      "Start predicting the current SDM for: ",
      species_name, "_tile_", i
    ))

    if (length(min_index) == 1) {
      pred_ras <- ENMeval::maxnet.predictRaster(
        mod = mdl_select,
        envs = stack_preds,
        pred.type = "cloglog",
        doClamp = TRUE,
      )
      # futsd <- predictMaxNet(mdl_select, stack_preds, type = "logistic")
      # futsd <- futsd * 100
      pred_ras <- pred_ras * 100 # convert to suitability.
      pred_ras <- round(pred_ras, digits = 1)
      print(pred_ras)
      writeRaster(pred_ras,
        filename = paste0(
          "I:/SDMs_France/Results/current_sdms/",
          species_name, "_tile_", i, ".tif", overwrite = TRUE
        ),
      )
    } else {
      for (k in seq_along(min_index)) {
        mdl_select <- mdl@models[[min_index[[k]]]]
        # futsd <- predictMaxNet(mdl_select, stack_preds, type = "logistic")
        # futsd <- futsd * 100
        pred_ras <- ENMeval::maxnet.predictRaster(
          mod = mdl_select,
          envs = stack_preds,
          pred.type = "cloglog",
          doClamp = TRUE,
        )
        pred_ras <- pred_ras * 100 # convert to suitability.
        print(pred_ras)
        writeRaster(pred_ras,
          filename = paste0(
            "I:/SDMs_France/Results/current_sdms/",
            species_name, "_tile_", i, "_model", k, ".tif", overwrite = TRUE
          ),
        )
      }
    }
  }
}

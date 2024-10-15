library("codetools", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("sp", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("raster", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("rgdal", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("GSIF", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("dplyr", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("prodlim", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("maxnet", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("magrittr", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("ENMeval", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
library("gdalUtils", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")

mdls.files <- list.files("/lustre1/scratch/332/vsc33264/02_Comparative/00_Final/01_Occs_micro/03_Micro/02_Model/", pattern = "\\.RData$", full.names = TRUE)
mdls.names <- list.files("/lustre1/scratch/332/vsc33264/02_Comparative/00_Final/01_Occs_micro/03_Micro/02_Model/", pattern = "\\.RData$", full.names = FALSE)
mdls.names <- gsub(".RData", "", mdls.names)

covs.files <- c("/lustre1/scratch/332/vsc33264/02_Comparative/01_Covariates/03_Micro/ForestClim_05.tif",
                "/lustre1/scratch/332/vsc33264/02_Comparative/01_Covariates/03_Micro/ForestClim_06.tif",
                "/lustre1/scratch/332/vsc33264/02_Comparative/01_Covariates/03_Micro/ForestClim_12.tif",
                "/lustre1/scratch/332/vsc33264/02_Comparative/01_Covariates/03_Micro/ForestClim_15.tif",
                "/lustre1/scratch/332/vsc33264/02_Comparative/01_Covariates/03_Micro/cec.tif",
                "/lustre1/scratch/332/vsc33264/02_Comparative/01_Covariates/03_Micro/clay.tif")

## tile to 10km blocks:
objB <- GDALinfo("/lustre1/scratch/332/vsc33264/02_Comparative/01_Covariates/03_Micro/ForestClim_05.tif")
tileB.tbl <- getSpatialTiles(objB, block.x=1e5, return.SpatialPolygons=FALSE)
tileB.tbl$ID <- as.character(1:nrow(tileB.tbl))

# Prediction function for tiling
predict_maxent <- function(mdls, indx, i, name, tileB.tbl, covs.files){
  
  out.tif <- paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name, "/tile_", tileB.tbl[i,"ID"], ".tif")
  
  newdata <- readGDAL(covs.files[1], offset=unlist(tileB.tbl[i,c("offset.y","offset.x")]),
                      region.dim=unlist(tileB.tbl[i,c("region.dim.y","region.dim.x")]),
                      output.dim=unlist(tileB.tbl[i,c("region.dim.y","region.dim.x")]),
                      silent = TRUE)
  for(j in 2:length(covs.files)){
    newdata@data[,j] <- readGDAL(covs.files[j],
                                 offset=unlist(tileB.tbl[i,c("offset.y","offset.x")]),
                                 region.dim=unlist(tileB.tbl[i,c("region.dim.y","region.dim.x")]),
                                 output.dim=unlist(tileB.tbl[i,c("region.dim.y","region.dim.x")]),
                                 silent = TRUE)$band1
  }
  
  names(newdata) <- c("ForestClim_05", "ForestClim_06", "ForestClim_12", "ForestClim_15", "cec", "clay")
  newdata$grid <- 15
  
  sel.pr <- complete.cases(newdata@data)
  indices <- which(sel.pr)
  
  logistic <- matrix(NA, nrow = length(indices), ncol = length(indx))
  
  for(r in 1:length(indx)){
    
    logistic_mdl <- predict(object = mdls@models[[indx[r]]], newdata = newdata@data[indices, 1:6], type = "logistic")
    logistic_mdl <- logistic_mdl * 100
    logistic_mdl <- round(logistic_mdl, digits = 0)
    logistic[, r] <- logistic_mdl
    
  }
  
  logistic <- rowMeans(logistic)
  
  g <- as(newdata["grid"], "SpatialPixelsDataFrame")
  g[indices,"logistic"] <- logistic
  writeGDAL(g["logistic"], out.tif, options="COMPRESS=DEFLATE", type = "Byte", mvFlag = 255)
  
  rm(list=setdiff(ls(), c("predict_maxent", "mdls", "indx", "name", "tileB.tbl", "covs.files", "q", "mdls.files", "mdls.names")))
  gc()
}

# Create maps
for(q in 1:50) { #length(gbm_list)
  
  print(q)
  
  # Load model
  load(mdls.files[q])
  mdls <- e.mx_rp.f
  name <- mdls.names[q]
  
  # Create a folder to save the tiles of each species
  dir.create(paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name))
  
  # Select the best SDM based on delta AIC
  # This dplyr operation executes the sequential criteria explained above.
  res <- eval.results(mdls)
  opt.seq <- filter(res, delta.AICc == min(delta.AICc))
  indx <- row.match(opt.seq, res)
  
  ### Make predictions per tile ###
  # Load snowfall, initialize cluster, load libraries and export objects to each core
  library("snow", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
  library("snowfall", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/")
  sfInit(parallel=TRUE, cpus=parallel::detectCores()) #parallel::detectCores()
  sfExport("predict_maxent", "mdls", "indx", "name", "tileB.tbl", "covs.files", "q")
  sfLibrary("rgdal", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/", character.only = TRUE)
  sfLibrary("maxnet", lib.loc = "/data/leuven/332/vsc33264/1_Library/4.0.2-foss/", character.only = TRUE)
  
  # Run function to create prediction for each tile (total tiles = 1394)
  outB.lst <- sfClusterApplyLB(1:nrow(tileB.tbl), #nrow(tileB.tbl)
                               function(x){ predict_maxent(mdls, indx, x, name, tileB.tbl, covs.files) })
  
  # Stop the parallel processing
  sfStop()
  
  ### Merge all tiles together to one tiff for Europe ###
  # list al 1394 tiles with predictions
  tB.lst <- list.files(paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name), pattern=".tif", full.names=TRUE)
  tB.lst <- tB.lst[order(nchar(tB.lst), tB.lst)]
  outB.tmp <- paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name, ".txt")
  
  # Path to final map
  final <- paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name, ".tif")
  
  # Create a virtual 
  vrtB.tmp <- paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name, ".vrt")
  cat(tB.lst, sep="\n", file=outB.tmp)
  gdalbuildvrt(input_file_list = outB.tmp, output.vrt = vrtB.tmp)
  gdalwarp(vrtB.tmp, final, 
           co = c("BIGTIFF=YES", "COMPRESS=DEFLATE"),
           multi = TRUE, overwrite = TRUE,
           wo = "NUM_THREADS=36", verbose = TRUE)
  
  # Delete all seperate part files to save memory
  f <- list.files(paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name), include.dirs = F, full.names = T, recursive = T)
  # remove the files
  file.remove(f)
  file.remove(paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name, ".vrt"))
  file.remove(paste0("/lustre1/scratch/332/vsc33264/02_Comparative/", name, ".txt"))
  
  rm(list=setdiff(ls(), c("predict_maxent", "tileB.tbl", "covs.files", "q", "mdls.files", "mdls.names")))
  gc()
  
}

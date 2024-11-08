library(terra)
library(predicts)
library(geodata)
library(rgbif)

# Load data
filename <- file.path(system.file(package = "predicts"), "ex/bradypus.csv")

# this is the file we will use:
basename(filename)
## [1] "bradypus.csv"
bradypus <- read.csv(filename)
bradypus <- bradypus[, 2:3]

# #### Download data from gbif. ####
# # Set up gbif account
# library(usethis)
# # install.packages("usethis")
# usethis::edit_r_environ()
# name_backbone("Lepus saxatilis")
# test <- occ_download(pred("taxonKey", 2436775))

# # remember to set up your GBIF credentials
# occ_download(pred("taxonKey", 2436775), format = "SIMPLE_CSV")
# d <- occ_download_get("0002305-241106120511685") %>%
#     occ_download_import()
# occ_download_wait("0079311-210914110416597") # checks if download is finished

# # A more realistic download
# # shorter equivalent to download above
# occ_download(
# pred_default(), 
# pred("taxonKey", 2436775), 
# format = "SIMPLE_CSV"
# )


# download data by geodata
acaule <- sp_occurrence("solanum", "acaule", geo = TRUE)
# load the saved S. acaule data
acfile <- file.path(system.file(package="predicts"), "ex/acaule.csv")
acaule <- read.csv(acfile)
# how many rows and colums?
dim(acaule)
colnames(acaule)
acgeo <- subset(acaule, !is.na(lon) & !is.na(lat))
dim(acgeo)
acgeo[1:5, c(1:5,7:10)]

# Roughly check if the points are in the right location.
wrld <- world(path=".") #contain rough country outlines.
plot(wrld, xlim=c(-110,60), ylim=c(-80,40), col="light yellow", border="light gray")
# add the points
points(acgeo$lon, acgeo$lat, col='red', pch=20)
acgeo[c(303,885),1:10]

# Check the three points at the sea.
lonzero <- subset(acgeo, lon==0)

# Duplicate records
# which records are duplicates (only for the first 10 columns)?
dups <- duplicated(lonzero[, 1:10])
# remove duplicates
lonzero  <-  lonzero[dups, ]

# differentiating by (sub) species
# dups2 <- duplicated(acgeo[, c('species', 'lon', 'lat')])

# ignoring (sub) species and other naming variation
dups2 <- duplicated(acgeo[, c('lon', 'lat')])
# number of duplicates
sum(dups2)
## [1] 483
# keep the records that are _not_ duplicated
acg <- acgeo[!dups2, ]
# Repatriate records to Argentina. Remove lon=0.
i <- acg$lon > 0 & acg$lat > 0
acg$lon[i] <- -1 * acg$lon[i]
acg$lat[i] <- -1 * acg$lat[i]
acg <- acg[acg$lon < -60 & acg$lat > -50, ]

# Cross-checking coordinates.
acv <- vect(acg, geom=c("lon", "lat"), crs="+proj=longlat +datum=WGS84")
# Spatial query of the polygons.
ovr <- extract(acv, wrld)
head(ovr)
## ovr has matching record from wrld for each point.
cntr <- ovr$id.y

# Which points do not match any country
i <- which(is.na(cntr))
i
## i=0, since we removed the points in the ocean.
## integer(0)

# Which points have xy that are in a different country than gbif.
j <- which(cntr != acv$country)
# for the mismatches, bind the country names of the polygons and points
m <- cbind(cntr[j], acg$country[j])
colnames(m) <- c("polygons", "acaule")
m
##      polygons acaule
plot(acv)
lines(wrld, col='blue', lwd=2)
## The wrld is not very precise...
bo <- gadm(country='BOL', level=0)

# Georeferencing. Records that do not have coordinates, but 
# do have a locality description.
georef <- subset(acaule, (is.na(lon) | is.na(lat)) & ! is.na(locality) )
georef[1:3,1:13]

# Sampling bias
# create a SpatRaster with the extent of acgeo
r <- rast(acv)
# set the resolution of the cells to (for example) 1 degree
res(r) <- 1
# extend (expand) the extent of the SpatRaster a little
r <- extend(r, ext(r)+1)
# sample:
set.seed(13)
acsel <- spatSample(acv, size=1, "random", strata=r)
# to illustrate the method and show the result
p <- as.polygons(r)
plot(p, border='gray')
points(acv)
# selected points in red
points(acsel, cex=1, col='red', pch='x')

t.lst <- list.files("I:/SDMs_France/Results/current_sdms/", pattern=".tif", full.names=TRUE)
MIs <- function(t.lst, fout="") {
  r <- vrt(t.lst)
  if (fout != "") {
    writeRaster(r, fout, overwrite=TRUE)
    fout
  } else {
    wrap(r)
  }
} 

MIs(
  t.lst,
  fout = "I:/SDMs_France/Results/current_sdms/Allium_ursinum_fr.tif"
)

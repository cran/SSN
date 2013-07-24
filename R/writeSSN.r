################################################################################
# Write SSN object
#
# Observed sites are selected based on a logical expression. The results are
# exported to a new .ssn folder. There is also an option to clip edges and
# prediction sites, based on the NetworkID values contained in the observed
# sites selection set.
################################################################################

writeSSN <- function(ssn, filename) {

	## #Insert our version of write.dbf into the foreign package
	## #Not. Very. Pretty. At all. So sorry.
	##  unlockBinding("DoWritedbf", environment(write.dbf))
	##assign("DoWritedbfSSN", DoWritedbfSSN, envir=environment(write.dbf.SSN))
	##  lockBinding("DoWritedbf", environment(write.dbf))

  suppressWarnings(warning("writeSSN"))
  dir.create(filename)
  oldwd <- getwd()
  setwd(filename)

  on.exit({
	setwd(oldwd)
  })


  ssn.tmp <- ssn
  pred.len <- length(ssn.tmp@predpoints@ID)


  ssn.tmp@path <- getwd()


  ssn.list <- list.files(ssn@path)
  for (i in 1:length(ssn.list)) {
    fn.old <- file.path(ssn@path, ssn.list[i])
    if (basename(fn.old) != "distance")  {
      if (substr(basename(fn.old), 1, 5) != "sites") {

        fn.new <- file.path(ssn.tmp@path, ssn.list[i])

        file.copy(fn.old, fn.new, overwrite = TRUE)}}
  }
    rm(fn.old, fn.new)


  # create spatial points data frame...
  coords <- ssn.tmp@obspoints@SSNPoints[[1]]@point.coords

  proj4string <- ssn.tmp@proj4string
  data.tmp <- ssn.tmp@obspoints@SSNPoints[[1]]@point.data

  ind.xy <- names(data.tmp) == "coords_x1" | names(data.tmp) == "coords_x2"
  if (sum(ind.xy) > 0) {
      data.tmp <- data.tmp[,!ind.xy]}

  sites.sub <- SpatialPointsDataFrame(coords = coords, data = data.tmp, proj4string = proj4string)
  writeSpatialShape(sites.sub, "sites")
  write.dbf.SSN(sites.sub@data, "sites")

  if (pred.len ==0) {
    ssn.tmp <- importSSN(ssn.tmp@path, o.write = T)}

  if (pred.len > 0) {
      pred.name.list <- ssn.tmp@predpoints@ID
      ssn.tmp <- importSSN(ssn.tmp@path, o.write = T)

      for (j in 1:pred.len) {
        ssn.tmp <- importPredpts(ssn.tmp, pred.name.list[j], obj.type = "ssn")}
  }

  setwd(oldwd)

  ##ssn.tmp
}


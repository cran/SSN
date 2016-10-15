reachNeiSSN = function(object)
{
  liNeighFun = function(x, SL) {
      gTouches(SpatialLines(list(x)), spgeom2 = SL, byid = TRUE)
    }
  SL = as.SpatialLines(object)
  SLlist = SL@lines
  ReachNei = sapply(SLlist, liNeighFun, SL = SL)
  colnames(ReachNei) = object@data$rid
  rownames(ReachNei) = object@data$rid
  ReachNei = Matrix(ReachNei, sparse = TRUE)
}

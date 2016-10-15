FCSD01 <- function(object, ResponseName, breaks, ReachNei, ...)
{
	if(class(object) == "influenceSSN") object <- object$ssn.object
	data <- object@obspoints@SSNPoints[[1]]@point.data
	data <- cbind(data, object@obspoints@SSNPoints[[1]]@point.coords)
	xcol <- "coords.x1"
	ycol <- "coords.x2"
	distord <- order(data[,"netID"],data[,"pid"])
	names(distord) <- rownames(data)[distord]

  whichCol = function(vec, Mat) {
    Tmp = NULL    
      for(j in 1:length(vec))  Tmp = c(Tmp,
        which(vec[j] == colnames(Mat)))
    Tmp
  }

	nIDs <- sort(as.integer(as.character(unique(data[,"netID"]))))
	mnz <- mean(data[,ResponseName], na.rm = TRUE)
  svgmall = vector("list", length(nIDs))
	nsofar <- 0
  loopi = 1
	for(i in nIDs) {
		workspace.name <- paste("dist.net", i, ".RData", sep = "")
		path <- file.path(object@path, "distance", "obs", 
			workspace.name)
		if(!file.exists(path)) {
			stop("Unable to locate required distance matrix")
		}
		file_handle <- file(path, open="rb")
		distmat <- unserialize(file_handle)
		close(file_handle)
		ordi <- order(as.numeric(rownames(distmat)))
		distmat <- distmat[ordi, ordi, drop = F]
		Ds <- distmat + t(distmat)
		FCs <- 1 - (pmin(distmat,t(distmat)) > 0)*1
		zs <- data[data$pid %in% rownames(distmat), 
			ResponseName, drop = F]
		attr(zs,"pid") <- data[data$pid %in% rownames(distmat),"pid"]
		zs <- zs[order(attr(zs,"pid")),ResponseName]
    zrid <- data[data$pid %in% rownames(distmat), 
			'rid', drop = F]
    attr(zrid,"pid") <- data[data$pid %in% rownames(distmat),"pid"]
		zrid <- zrid[order(attr(zrid,"pid")),'rid']
    zrid = as.character(zrid)
		ind <- !is.na(zs)
		ni <- sum(ind)
		zs <- zs[ind, drop = F]
		Ds <- Ds[ind,ind, drop = F]
    Ds = Ds[lower.tri(Ds)]
		FCs <- FCs[ind,ind, drop = F]
    FCs = FCs[lower.tri(FCs)]
    zrid = zrid[ind, drop = F]
		rs <- zs - mnz
		CP <- rs%o%rs
    CP = CP[lower.tri(CP)]
		df2 <- (abs(zs%o%rep(1, times = ni) - 
			rep(1, times = ni)%o%zs))^2
    df2 = df2[lower.tri(df2)]
    ridMat = matrix(rep(zrid, times = length(zrid)), nrow = length(zrid))
    rid0 = ridMat == t(ridMat)
    rid0 = rid0[lower.tri(rid0)]
		Ds0 = Ds[FCs == 1 & rid0]
		CP0 = CP[FCs == 1 & rid0]
		df20 = df2[FCs == 1 & rid0]/2
    cutvec = cut(Ds0, breaks = breaks)
    svgm0 = NULL
    svgm0 = cbind(
	    aggregate(df20, by = list(cutvec), mean),
		  aggregate(Ds0, by = list(cutvec), mean)[2],
	    aggregate(CP0, by = list(cutvec), mean)[,2],
      aggregate(df20, by = list(cutvec), function(x) {length(x)})[,2]
		)	
    svgm0 = svgm0[,c(1,3,2,4,5)]
    colnames(svgm0) = c('brkClass','brkDist','semivar','covar','np')
    RNid = whichCol(zrid,ReachNei)
    rid1 = ReachNei[RNid,]
    rid1 = rid1[,RNid]
    rid1 = rid1[lower.tri(rid1)]
    Ds1 = Ds[FCs == 1 & rid1]
		CP1 = CP[FCs == 1 & rid1]
		df21 = df2[FCs == 1 & rid1]/2
		cutvec = cut(Ds1, breaks = breaks)
    svgm1 = NULL
    if(length(Ds1) > 0) {
    svgm1 = cbind(
	    aggregate(df21, by = list(cutvec), mean),
		  aggregate(Ds1, by = list(cutvec), mean)[2],
	    aggregate(CP1, by = list(cutvec), mean)[,2],
      aggregate(df21, by = list(cutvec), function(x) {length(x)})[,2]
		)	
    svgm1 = svgm1[,c(1,3,2,4,5)]
    colnames(svgm1) = c('brkClass','brkDist','semivar','covar','np')
    }
    svgmall[[loopi]] = list(svgm0 = svgm0, svgm1 = svgm1)
      blevels = levels(cutvec)
    svgmall[[loopi]] = lapply(svgmall[[loopi]], 
      function(db) {
      db$brkClass <- factor(as.character(db$brkClass), levels=blevels)
      merge(db, data.frame(brkClass=setdiff(blevels, db$brkClass)), all=T)
    })
    loopi = loopi + 1
	}
  names(svgmall) = nIDs
  out = list(FCSD01 = svgmall, breaks = breaks)
	class(out) <- "FCSD01"
	out
}



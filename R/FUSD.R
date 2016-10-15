FUSD <- function(object, ResponseName, breaks, ...)
{
	if(class(object) == "influenceSSN") object <- object$ssn.object
	data <- object@obspoints@SSNPoints[[1]]@point.data
	data <- cbind(data, object@obspoints@SSNPoints[[1]]@point.coords)
	xcol <- "coords.x1"
	ycol <- "coords.x2"
	distord <- order(data[,"netID"],data[,"pid"])
	names(distord) <- rownames(data)[distord]

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
		ind <- !is.na(zs)
		ni <- sum(ind)
		zs <- zs[ind, drop = F]
		Ds <- Ds[ind,ind, drop = F]
		FCs <- FCs[ind,ind, drop = F]
		rs <- zs - mnz
		CP <- rs%o%rs
		df2 <- (abs(zs%o%rep(1, times = ni) - 
			rep(1, times = ni)%o%zs))^2
    FCs = FCs[lower.tri(FCs)]
		Ds = Ds[lower.tri(Ds)]
    Ds = Ds[FCs == 0]
		CP = CP[lower.tri(CP)]
    CP = CP[FCs == 0]
		df2 = df2[lower.tri(df2)]/2
		df2 = df2[FCs == 0]

    cutvec = cut(Ds, breaks = breaks,...)
    svgm = cbind(
      aggregate(df2, by = list(cutvec), mean, na.action = na.omit),
		  aggregate(Ds, by = list(cutvec), mean)[2],
      aggregate(CP, by = list(cutvec), mean)[,2],
      aggregate(df2, by = list(cutvec), function(x) {length(x)})[,2]
		)		
    svgm = svgm[,c(1,3,2,4,5)]
  	colnames(svgm) = c('brkClass','brkDist','semivar','covar','np')
    svgmall[[loopi]] = svgm
    loopi = loopi + 1
	}
  names(svgmall) = nIDs
  blevels = levels(cutvec)
  svgmall = lapply(svgmall, 
  function(db) {
    db$brkClass <- factor(as.character(db$brkClass), levels=blevels)
      merge(db, data.frame(brkClass=setdiff(blevels, db$brkClass)), all=T)
  })
  out = list(FUSD = svgmall, breaks = breaks)
	class(out) <- "FUSD"
	out
}



FUDJ <- function(object, ResponseName, breaks = NULL, ...)
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
		Ds = distmat + t(distmat)
		FCs = 1 - (pmin(distmat,t(distmat)) > 0)*1
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
		df2 <- ((zs%o%rep(1, times = ni) - 
			rep(1, times = ni)%o%zs))^2
    ## maximum distance to common junction between two sites
		a.mat <- pmax(distmat,t(distmat))
		## minimum distance to common junction between two sites
		b.mat <- pmin(distmat,t(distmat))
    FCs = FCs[lower.tri(FCs)]
		Ds = Ds[lower.tri(Ds)]
    Ds = Ds[FCs == 0]
		CP = CP[lower.tri(CP)]
		CP = CP[FCs == 0]
		df2 = df2[lower.tri(df2)]/2	
		df2 = df2[FCs == 0]  
		a.mat = a.mat[lower.tri(a.mat)]
		a.mat = a.mat[FCs == 0]    
		b.mat = b.mat[lower.tri(b.mat)]
		b.mat = b.mat[FCs == 0]    
#    cutvec = cut(c(b.mat,a.mat), breaks = breaks,...)
    cuta = cut(a.mat, breaks = breaks, ...)
		cutb = cut(b.mat, breaks = breaks, ...)
    svgm = NULL
		for(j in 1:max(as.integer(cutb), na.rm = TRUE)) {
      nj = length(aggregate(df2[as.integer(cutb) == j], 
		    by = list(cuta[as.integer(cutb) == j]), mean)[,2])
		  svgm = rbind(svgm,
        cbind( 
#        aggregate(df2, by = list(cutvec), mean, na.action = na.omit),
		    aggregate(df2[as.integer(cutb) == j], 
		      by = list(cuta[as.integer(cutb) == j]), mean),
		    rep(levels(cutb)[j], times = nj),
		    aggregate(a.mat[as.integer(cutb) == j], 
		      by = list(cuta[as.integer(cutb) == j]), mean)[,2],
        aggregate(b.mat[as.integer(cutb) == j], 
		      by = list(cuta[as.integer(cutb) == j]), mean)[,2],
	      aggregate(df2[as.integer(cutb) == j], 
		      by = list(cuta[as.integer(cutb) == j]), function(x){length(x)})[,2],
        aggregate(CP[as.integer(cutb) == j], 
		      by = list(cuta[as.integer(cutb) == j]), mean)[2]
		    )
      )
    }
    svgm = svgm[,c(1,4,3,5,2,7,6)]
	  colnames(svgm) = c('brksLong','distLong',
      'brksShrt','distShrt','semivar','covar','np')
    svgmall[[loopi]] = svgm
    loopi = loopi + 1
	}
  blevels = unique(as.vector(unlist(lapply(svgmall, 
    function(db) {
      paste0(db$brksLong,db$brksShrt)
    }))))
  svgmall = lapply(svgmall, 
    function(db) {
      db$brkClass <- factor(as.character(paste0(db$brksLong,
        db$brksShrt)), levels=blevels)
      merge(db, data.frame(brkClass=setdiff(blevels, 
        paste0(db$brksLong,db$brksShrt))), all=T)
  })
  svgmall = lapply(svgmall, function(db){db[,2:length(db[1,])]})
  names(svgmall) = nIDs
	out = list(svgm = svgmall, breaks = breaks)
	class(out) <- "FUDJ"
	out
}



AIC.glmssn <- function(object,...,k) {
    if(class(object) != "glmssn") return("Not a glmssn object")
    if(!missing(k)) cat("This argument has no effect\n")
    if(object$args$family == "poisson") return(NA)
    if(object$args$family =="binomial") return(NA)
    if(object$args$algorithm=="orig") cp <- unlist(covparms(object))
    if(object$args$algorithm=="multi") cp <- covparms(object)[,3]
    nparmstheta <- length(cp)
    rankX <- object$sampinfo$rankX
    if(object$args$EstMeth == "REML")
        k <- nparmstheta
    if(object$args$EstMeth == "ML")
        k <- rankX + nparmstheta
    object$estimates$m2LL + 2*k
}


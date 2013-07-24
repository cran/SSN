summary.glmssn <-
function(object, ...)
{
	effnames <- object$sampinfo$effnames
	setzero <- object$sampinfo$setzero
	setNA <- object$sampinfo$setNA
	if(length(setNA)==1) setNA <- FALSE
	setNA2 <- object$sampinfo$setNA2
	if(length(setNA2)==1) setNA2 <- FALSE
	b.hat <- object$estimates$betahat
	bhat.se <- sqrt(diag(object$estimates$covb))
	n.allxy <- object$sampinfo$obs.sample.size
	p <- object$sampinfo$rankX

        if(any(rownames(b.hat) %in% effnames == FALSE)) {
            ## dataXY issue
            stop(cat("glmssn has computed estimates for",rownames(b.hat),"but the summary command expects estimates for",effnames,collapse=" "))
        }

	bhat.0.NA <- rep(NA, times = length(effnames))
	bhat.0.NA[setzero] <- 0
	bhat.0.NA[!setzero & !setNA2] <- b.hat
	NAvec <- rep(NA, times = length(effnames))
	bhatse.0.NA <- NAvec
	bhatse.0.NA[!setzero & !setNA2] <- bhat.se
	tvec <- NAvec
	tvec[!setzero & !setNA2] <- b.hat/bhat.se
	pvec <- NAvec
	pvec[!setzero & !setNA2] <-
			round(100000*(1 - pt(abs(b.hat/bhat.se), df = n.allxy - p))*2)/100000
	fixed.eff.est <- data.frame(FactorLevel = effnames, Estimate = bhat.0.NA,
			std.err = bhatse.0.NA, t.value = tvec, prob.t = pvec)
	fixed.effects.estimates = fixed.eff.est

        if(object$args$algorithm=="orig") {
            covmodels <- object$estimates$theta
            covmodels <- data.frame(Covariance.Model=attributes(
				covmodels)$terms,Parameter=attributes(covmodels)$type,
				Estimate=covmodels)
        }

        if(object$args$algorithm=="multi") {
            covmodels <- object$estimates$theta
            covmodels <- data.frame(Covariance.Model=attributes(covmodels)$terms,Parameter=attributes(covmodels)$type,Estimate=covmodels)
        }

        if(object$args$algorithm=="regress") {
            cat("improve this summary\n")
            covmodels <- object$estimates$theta
        }
		Warnlog <- object$estimates$Warnlog
        list(fixed.effects.estimates = fixed.effects.estimates,
			covariance.parameter.estimates = covmodels,
			Warnings = Warnlog)

}

print.glmssn <- function(x,...) {
    print(summary(x,...))
}

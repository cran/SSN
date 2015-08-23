CrossValidationStatsSSN <- function(object)
{
	cv.out <- CrossValidationSSN(object)
	zt <- object$sampinf$z
	n <-  object$sampinf$obs.sample.size
	dfobs <- n - object$sampinf$rankX
	cv.stats <- data.frame(bias = sum(cv.out[,1] - zt)/n,
		std.bias = sum((cv.out[,1] - zt)/sqrt(cv.out[,2]))/n,
		RMSPE = sqrt(sum((zt - cv.out[,1])^2)/n),
		RAV = sqrt(sum(cv.out[,2]^2)/n),
		std.MSPE = sum((cv.out[,1] - zt)^2/cv.out[,2]^2)/n,
		cov.80 = sum(abs((zt - cv.out[,1])/cv.out[,2]) < qt(.90, dfobs))/n,
		cov.90 = sum(abs((zt - cv.out[,1])/cv.out[,2]) < qt(.95, dfobs))/n,
		cov.95 = sum(abs((zt - cv.out[,1])/cv.out[,2]) < qt(.975, dfobs))/n)
  cv.stats
}

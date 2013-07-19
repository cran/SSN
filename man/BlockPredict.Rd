\name{BlockPredict}
\alias{BlockPredict}

\title{
Block Predicton for Streams Data}
\description{
Block prediction for objects of class \link{glmssn-class}
}
\usage{
BlockPredict(object, predpointsID)
}

\arguments{
  \item{object}{
    an object of class \link{glmssn}
  }
  \item{predpointsID}{
    a valid prediction points ID
  }
}
\details{
  This function operates on \code{glmssn} objects in much the same
  way as the \command{predict} function. \command{BlockPredict} uses the
  locations in the predpointsID data set to compute the average prediction
  value in the area defined by the prediction locations.  These prediction
  locations are used to approximate the integral over that area, so they
  should be evenly spaced and dense in the area where block prediction is
  desired.  The user needs to create these prediction locations and include
  them in the SSN object prior to fitting the model with glmssn.
}
\value{
  A data.frame with one row and two columns.  The first column, BlockPredEst,
  is the average prediction value, and the second column, BlockPredSE, is the
  standard error of the block prediction.
}
\references{
Ver Hoef, J. M.. Peterson, E. E. and Theobald, D. (2006) Spatial statistical
  models that use flow and stream distance. \emph{Environmental and
  Ecological Statistics} \bold{13}, 449-464. DOI: 10.1007/s10651-006-0022-8.
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\examples{

library(SSN)
# NOT RUN 
# mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
#	        package = "SSN"), o.write = TRUE)
# use SpatialStreamNetwork object mf04 that was already created
data(mf04p)
#make sure mf04p has the correct path, will vary for each users installation
mf04p <- updatePath(mf04p, system.file("lsndata/MiddleFork04.ssn", package = "SSN"))

## NOT RUN Distance Matrix has already been created
## createDistMat(mf04)
# mf04p <- importPredpts(mf04p, "Knapp", "ssn")
# mf04p <- importPredpts(mf04p, "CapeHorn", "ssn")
names(mf04p)
## NOTE: need the amongpreds distance matrices for block prediction
#createDistMat(mf04p, predpts = "Knapp", o.write = TRUE, amongpreds = TRUE)
# just do CapeHorn Example
createDistMat(mf04p, predpts = "CapeHorn", o.write = TRUE, amongpreds = TRUE)

# NOT RUN see densely gridded prediction points on stream
# plot(mf04p, PredPointsID = "Knapp")

# NOT RUN   fit the model first
#fitSpBk <- glmssn(Summer_mn ~ ELEV_DEM + netID,
#    ssn.object = mf04p, EstMeth = "REML", family = "Gaussian",
#    CorModels = c("Exponential.tailup","Exponential.taildown",
#    "Exponential.Euclid"), addfunccol = "afvArea")
data(modelFits)
fitSpBk$ssn.object <- updatePath(fitSpBk$ssn.object, 
	system.file("lsndata/MiddleFork04.ssn", package = "SSN"))
# one-at-a-time predictions for CapeHorn stream
fitSpPredC <- predict(fitSpBk, "CapeHorn")
# NOT RUN plot densely gridded prediction points on stream
# plot(glmssn.BPCapeHorn, "Summer_mn")
# block prediction for CapeHorn stream
BlockPredict(fitSpBk, "CapeHorn")
## NOT RUN Another example
# one-at-a-time predictions for Knapp stream
#fitSpPredK <- predict(fitSpBk, "Knapp")
# NOT RUN plot densely gridded prediction points on stream
# plot(fitSpPredK, "Summer_mn")
# block prediction for Knapp stream
#BlockPredict(fitSpBk, "Knapp")

}


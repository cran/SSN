\name{as.SpatialLines}
\alias{as.SpatialLines}
\alias{as.SpatialPoints}
\alias{as.SpatialPointsDataFrame}
\alias{as.SpatialLines.SpatialStreamNetwork}
\alias{as.SpatialPoints.SpatialStreamNetwork}
\alias{as.SpatialPointsDataFrame.SpatialStreamNetwork}
\title{Methods to convert SpatialStreamNetwork objects classes to sp classes}
\description{
  Converts \code{SpatialStreamNetwork} objects to \code{sp} objects.
}
\usage{
\method{as.SpatialLines}{SpatialStreamNetwork}(x, ...)
\method{as.SpatialPoints}{SpatialStreamNetwork}(x, data = "Obs", ...)
\method{as.SpatialPointsDataFrame}{SpatialStreamNetwork}(x, data = "Obs", ...)
}
\arguments{
  \item{x}{
    an \code{SpatialStreamNetwork} object to be converted to class \link{SpatialLines}, \link{SpatialPoints}, or \link{SpatialPointsDataFrame} from the \code{sp} package. 
  }
  \item{data}{ the data set in the \code{SpatialStreamNetwork} object to convert. The \code{SpatialStreamNetwork} object can hold multiple spatial point data sets, including the observed data and multiple prediction data sets.  See \link{SpatialStreamNetwork-class}.}
  \item{...}{
    optional arguments for specific methods written for these generics
  }
}

\value{
\code{as.SpatialLines.SpatialStreamNetwork} converts an object of class \code{SpatialStreamNetwork} to an object of class \code{SpatialLines} from the \code{sp} package, \code{as.SpatialPoints.SpatialStreamNetwork} converts an object of class \code{SpatialStreamNetwork} to an object of class \code{SpatialPoints} from the \code{sp} package, and \code{as.SpatialPointsDataFrame.SpatialStreamNetwork} converts an object of class \code{SpatialStreamNetwork} to an object of class \code{SpatialPointsDataFrame} from the \code{sp} package,  
}
\seealso{
  \link{spplot}
}
\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}
\examples{
library(SSN)
# NOT RUN
#mf04 <- importSSN(system.file("lsndata/MiddleFork04.ssn",
#  package = "SSN"), o.write = TRUE)
#  use SpatialStreamNetwork object mf04p that was already created
data(mf04p)
#Update path in mf04, will vary for each users installation
mf04p <- updatePath(mf04p, system.file("lsndata/MiddleFork04.ssn", package = "SSN"))

names(mf04p)

#---------
# make plots using sp methods
#---------
#plot the stream lines
plot(as.SpatialLines(mf04p), col = "blue")
# add the observed locations with size proportional 
# to mean summer temperature
plot(as.SpatialPoints(mf04p), pch = 19, 
  cex = as.SpatialPointsDataFrame(mf04p)$Summer_mn/9 , add = TRUE)
# add the prediction locations on the 1 km spacing
plot(as.SpatialPoints(mf04p, data = "pred1km"), cex = 1.5, add = TRUE)
# add the dense set of points for block prediction on Knapp segment
plot(as.SpatialPoints(mf04p, data = "Knapp"), pch = 19, cex = 0.3, 
  col = "red", add = TRUE)
}


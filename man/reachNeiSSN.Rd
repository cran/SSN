\name{reachNeiSSN}
\alias{reachNeiSSN}

\title{
Find neighboring stream segments
}

\description{
Create a neighborhood matrix of 0's and 1's that indicate the neighboring stream segments (reaches) of each stream segment
}

\usage{
reachNeiSSN(object)
}

\arguments{
  \item{object}{
  An object of class \link{SpatialStreamNetwork-class}
  }
}

\details{
Returns a sparse matrix with 1's indicating neighboring stream segments (reaches).  The \code{\link{rownames}} and \code{\link{colnames}} are the same as the "rid" values for the stream segments.  That is, the \code{\link{data.frame}} for the stream segments is in the \code{@data} slot of the \code{\link{SpatialStreamNetwork-class}} object, and that \code{data.frame} always has a unique reach identifier in the "rid" column.
}
\value{
  A sparse matrix.
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
data(mf04)
# for examples, copy MiddleFork04.ssn directory to R's temporary directory
copyLSN2temp()
#make sure mf04p has the correct path, will vary for each users installation
mf04 <- updatePath(mf04, paste0(tempdir(),'/MiddleFork04.ssn'))

neiMat = reachNeiSSN(mf04)
all(rownames(neiMat) == mf04@data$rid)
}

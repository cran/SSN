\name{FUSD}
\alias{FUSD}

\title{
Empirical Semivariogram Using Hydrologic Distance Between Flow-unconnected Sites
}
\description{
\command{FUSD} computes the empirical semivariogram from
the data based on hydrologic distance between flow-unconnected sites by network.
}
\usage{
FUSD(object, ResponseName, breaks, ...)
}

\arguments{
  \item{object}{
    an object of class \link{SpatialStreamNetwork-class} or \link{influenceSSN-class}
}
  \item{ResponseName}{
    a response or residual variable name in the data.frame of observed data in
    the SpatialStreamNetwork or influenceSSN object.
}
  \item{breaks}{
     either a numeric vector of two or more unique cut points or a
     single number (greater than or equal to 2) giving the number
          of intervals into which the response values are to be cut.
}
  \item{...}{
      further arguments passed from the \link{cut} function.
}
}
\details{
The \command{FUSD} function creates a list of two items. The first is another list of data.frames containing hydrologic distances and
empirical semivariogram values, along with number of pairs of points in each
bin, for flow-unconnected sites.  The second list item is a vector of the breaks used to create the distance classes. The output is of class FUSD.
}
\value{
A list of two items.  The first is another list of data.frames (one for each network) describing the semivariance values for each bin and the hydrologic distances and number of pairs used to estimate those values. Computed for flow-unconnected sites only.  The data.frames have the following columns:
  \item{brkClass}{
    a factor variable with labels showing the upper and lower bounds for each break class.
}
  \item{brkDist}{
    the mean hydrologic distance separating pairs of flow-connected sites used
    to calculate the semivariance for each bin 
}
  \item{semivar}{
    the mean semivariance for flow-unconnected sites in each bin
}
  \item{covar}{
    the mean empirical covariance for flow-unconnected sites in each bin
}
  \item{np}{
    the number of pairs of flow-unconnected sites used to calculate the semivariance or covariance for each bin
}
}

\author{
Jay Ver Hoef \email{support@SpatialStreamNetworks.com}
}

\seealso{
The \code{\link{cut}} function is used to create distance classes.  The \code{\link{Torgegram}} function computes semivariance values for both flow-connected and flow-unconnected that are pooled across networks.
}

\examples{

  library(SSN)
  # NOT RUN 
  #mf04p <- importSSN(system.file("lsndata/MiddleFork04.ssn", 
  #  package = "SSN"), predpts = "pred1km", o.write = TRUE)
  #  use SpatialStreamNetwork object mf04p that was already created
  data(mf04p)
  #make sure mf04p has the correct path, will vary for each users installation
  mf04p@path <- system.file("lsndata/MiddleFork04.ssn", package = "SSN")
  #get maximum stream distance among sampled points
  Ds = getStreamDistMat(mf04p)
  DsNet1 = Ds$dist.net1 + t(Ds$dist.net1)
  DsNet2 = Ds$dist.net2 + t(Ds$dist.net2)
  max(DsNet1,DsNet2)
  #set number of lags
  nbrksfu = 6
  # set maximum lag
  maxbrksfu = 22000
  # create breaks
  fubrks = c(-.001, (1:nbrksfu)/nbrksfu*maxbrksfu)
  vout = FUSD(mf04p, 'Summer_mn', breaks = fubrks, include.lowest = TRUE)
  str(vout)

}



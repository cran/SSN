\name{updatePath}
\alias{updatePath}


\title{
  Update Path Slot in SpatialStreamNetwork Object }

\description{
Updates the path slot in an existing SpatialStreamNetwork object based on a
user-defined filepath.  }

\usage{ updatePath(ssn, filepath) }

\arguments{
  \item{ssn}{
    a \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}} object
}
  \item{filepath}{
    path name to the .ssn folder, in string format including quotes. Also
    include the .ssn folder in the path name
}
}
\details{
At times, it may be necessary to move a .ssn directory, which is linked
to a \code{\link[=SpatialStreamNetwork-class]{SpatialStreamNetwork}}
object in an R workspace. If the .ssn directory is moved, the path slot
must be updated before using the \code{glmssn} function. The
\code{updatePath} function serves this purpose.
}


\author{
Erin E. Peterson \email{support@SpatialStreamNetworks.com}
}



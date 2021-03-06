% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expp-package.R
\docType{package}
\name{expp-package}
\alias{expp-package}
\alias{expp}
\title{Tools and data to accompany Schlicht, Valcu and Kempenaers "Spatial patterns
of extra-pair paternity: beyond paternity gains and losses"}
\description{
The expp package provides classes and functions for the investigation of the
probability of having extra-pair young within local networks of breeding
pairs including both realized and potential extra-pairings.
}
\details{
\tabular{ll}{ Package: \tab expp\cr Type: \tab Package\cr Version: \tab
1.0\cr Date: \tab 2014-Aug-04\cr License: \tab GPL-3\cr } See
\code{help(epp)} and \code{vignette('expp') }
}
\section{Functions}{
 \tabular{ll}{ \code{\link{epp}} \tab Final
data-transformation to male-female combinations and their extra-pair levels
\cr \code{\link{eppSimDat}} \tab "Toy"-dataset creation to investigate
potential Type I error rate inflation for models where the datapoints are
male-female combinations \cr

\cr ---------------------------------------- \cr
\code{\link{DirichletPolygons}} \tab Territory calculation via Dirichlet
tesselation\cr \code{\link{eppMatrix}} \tab \code{data.frame} to
\code{eppMatrix} object\cr \code{\link{neighborsDataFrame}} \tab \code{nb}
object to \code{data.frame}\cr \code{\link{SpatialPointsBreeding}} \tab
\code{data.frame} to \code{SpatialPointsBreeding object} \cr

}
}

\references{
Schlicht, Lotte, Mihai Valcu, and Bart Kempenaers. 
"Spatial patterns of extra-pair paternity: beyond paternity gains and losses." 
 Journal of Animal Ecology 84.2 (2015): 518-531.
}
\author{
Mihai Valcu and Lotte Schlicht \cr Maintainer: Mihai Valcu
<valcu@orn.mpg.de>
}
\keyword{package}

\name{bluetit_boundary}
\alias{bluetit_boundary}
\docType{data}
\title{
Study area boundary.
}

\description{
	\code{SpatialPolygonsDataFrame} 
 Study area boundary of two Blue Tit populations: Kolbeterberg, Vienna, Austria (1998 through 2004) and 
Westerholz, Bavaria, Germany (2007 through 2011) .
	}

\usage{data(bluetit_boundary)}
\format{
  A SpatialPolygonsDataFrame with 12 SpatialPolygons.
  \describe{
    \item{\code{year_}}{numeric. The year of the observation.}
     }
}

\examples{
data(bluetit_boundary)
summary(bluetit_boundary)
}

\keyword{datasets}

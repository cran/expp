\name{eppMatrix}
\alias{eppMatrix}
\alias{eppMatrix-class}

\title{Convert a \code{data.frame}  to an eppMatrix object.}

\description{
Converts a \code{data.frame} to a eppMatrix object using a \code{~male+female} formula.
}

\usage{eppMatrix(data, pairs = ~ male + female)}

\section{Slots}{
  \describe{
    \item{\code{male}:}{Object of class \code{"character"}: extra-pair male ID }
    \item{\code{female}:}{Object of class \code{"character"}:extra-pair female ID}
  }
}


\arguments{
  \item{data}{ a \code{data.frame} }
  \item{pairs}{a formula indicating the extra-pair male and 
      the extra-pair female in that order. }
}


\value{
An object of class \code{eppMatrix} with two slots.
}

\seealso{\code{\link[expp]{epp}}}

\examples{
eppPairs = data.frame(male = c("m1", "m2", "m1"), female=c("f3", "f1", "f2") )
e = eppMatrix(eppPairs,  pairs = ~ male + female)
class(e)
showClass("eppMatrix")

data(westerholzBreeding)
data(westerholzEPP)
b = westerholzBreeding[westerholzBreeding$year_ == 2010, ]
eppPairs = westerholzEPP[westerholzEPP$year_ == 2010, ]

breedingDat  = SpatialPointsBreeding(b, id = 'id', coords = ~ x + y, breeding = ~ male + female)
eppDat = eppMatrix(eppPairs, pairs = ~ male + female)

plot(breedingDat, eppDat)


}

\keyword{spatial}
























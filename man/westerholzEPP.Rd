\name{westerholzEPP}
\alias{westerholzEPP}
\docType{data}
\title{
Westerholz extra-pair paternity data (2009 and 2010).
}

\description{
	\code{data.frame} containing all extra-pair male-female combinations determined based on parentage analysis.
	}

\usage{data(westerholzEPP)}
\format{
  A data frame with 61 observations on the following 3 variables.
  \describe{
    \item{\code{year_}}{
		numeric. The year of the observation.}
    \item{\code{female}}{
		character. The female involved in the respective EPP event.}
    \item{\code{male}}{
		character. The male involved in the respective EPP event.}
  }
}

\examples{
data(westerholzEPP)
head(westerholzEPP)
}

\keyword{datasets}

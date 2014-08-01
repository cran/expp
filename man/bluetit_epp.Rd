\name{bluetit_epp}
\alias{bluetit_epp}
\docType{data}
\title{
Blue tit extra-pair paternity data.
}

\description{
	\code{data.frame} 
  Extra-pair paternity data recorded for two Blue Tit populations in Kolbeterberg, Vienna, Austria (1998 through 2004) and 
Westerholz, Bavaria, Germany (2007 through 2011) .
	}

\usage{data(bluetit_epp)}
\format{
  A data frame with 425 observations on the following 3 variables.
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
data(bluetit_epp)
head(bluetit_epp)
}

\keyword{datasets}

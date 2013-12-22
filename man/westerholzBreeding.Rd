\name{westerholzBreeding}
\alias{westerholzBreeding}
\docType{data}
\title{
Westerholz breeding data (2009 and 2010).
}
\description{
Breeding data recorded for  a Blue Tit population in 2009 and 2010 in Westerholz, Bavaria, Germany.
This data set contains the location of breeding attempts, the respective social pair, and several individual and nest parameters. 
}

\usage{data(westerholzBreeding)}
\format{
  A data frame with 152 observations on the following 9 variables.
  \describe{
    \item{\code{year_}}{numeric. The year of the observation.}
    \item{\code{id}}{numeric. The identity of the nest box in which the breeding attempt took place.}
    \item{\code{x}}{numeric. The east-west location of the nest box.}
    \item{\code{y}}{numeric. The north-south location of the nest box.}
    \item{\code{layingDate}}{numeric. The day of the year when the first egg was produced.}
    \item{\code{male_age}}{character. The age class of the male ('juv' = 1st year breeder; 'adult' = older)}
    \item{\code{female_age}}{character. The age class of the female ('juv' = 1st year breeder; 'adult' = older)}
    \item{\code{female}}{character. The identity of the female.}
    \item{\code{male}}{character. The identity of the male. }
  }
}

\examples{
data(westerholzBreeding)
head(westerholzBreeding)
}

\keyword{datasets}

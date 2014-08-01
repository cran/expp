\name{bluetit_breeding}
\alias{bluetit_breeding}
\docType{data}
\title{
Blue Tit breeding data.
}
\description{
Breeding data recorded for two Blue Tit populations in Kolbeterberg, Vienna, Austria (1998 through 2004) and 
Westerholz, Bavaria, Germany (2007 through 2011) .
The data set contains breeding attempts locations, the respective social pair, and several individual and nest parameters. 
}

\usage{data(bluetit_breeding)}
\format{
  A data frame with 1025 observations on the following 10 variables.
  \describe{
    \item{\code{year_}}{numeric. The year of the observation.}
    \item{\code{id}}{numeric. The identity of the nest box in which the breeding attempt took place.}
    \item{\code{x}}{numeric. The east-west location of the nest box.}
    \item{\code{y}}{numeric. The north-south location of the nest box.}
    \item{\code{female}}{character. The identity of the female.}
    \item{\code{male}}{character. The identity of the male. }
    \item{\code{layingDate}}{numeric. The day of the year when the first egg was produced.}
    \item{\code{male_age}}{character. The age class of the male ('juv' = 1st year breeder; 'adult' = older)}    
    \item{\code{male_tarsus}}{numeric. tarsus length (mm))}
    \item{\code{study_area}}{character. The study area name. }
  }
}

\examples{
data(bluetit_breeding)
head(bluetit_breeding)
}

\keyword{datasets}

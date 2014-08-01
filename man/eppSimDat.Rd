\name{eppSimDat}
\alias{eppSimDat}

\title{
Type I error rate simulations
}

\description{
A helper function to perform Type I error rate simulations. 
}

\usage{
eppSimDat(N, meanClutch, eppRate, eppMax, eppMales, nLags)
}

\arguments{
  \item{N}{Number of breeding pairs; default value is 10}
  \item{meanClutch}{Mean clutch size (integer); clutch size it is assumed to be Poisson distributed; default is 10}
  \item{eppRate}{Proportion of extra-pair young in population; default is 0.10}
  \item{eppMax}{Maximum number of extra-pair young by male; default is 12}
  \item{eppMales}{Proportion of extra-pair males in population; default is 0.35}
  \item{nLags}{ \code{maxlag} parameter to pass to \link[expp]{DirichletPolygons}  }

}

\details{
All default values match the values found in one of our study populations ('Westerholz').
}

\value{
An object of class \link[expp]{epp}
The \code{data.frame} of the \code{EPP} slot contains two variable (\code{trait_MALE} \code{trait_FEMALE} ) simulated independent from the \code{epp} variable.

}



\examples{
d = eppSimDat()
plot(d)


\dontrun{
# Type I error rate simulation

require(lme4)
pval_glmer = vector(mode = "numeric", length = 0)
pval_glm = vector(mode = "numeric", length = 0)

for(i in 1:500) {
  x = as.data.frame(eppSimDat(N = 120, meanClutch = 10, eppRate = 0.10, eppMax = 12, 
      eppMales = 0.35, nLags = 3))
  
  fm1glmer = glmer(epp ~ rank + trait_MALE + trait_FEMALE + (1 | male) + (1 | female) , 
  data = x, family = binomial, nAGQ =  0)
  fm0glmer = update(fm1glmer, epp ~ 1 + (1 | male) + (1 | female) )
  pval_glmer[i] = anova(fm0glmer, fm1glmer)$"Pr(>Chisq)"[2]
  
  fm1glm = glm(epp ~ rank + trait_MALE + trait_FEMALE  , data = x, family = binomial)
  fm0glm = update(fm1glm, epp ~ 1 )
  pval_glm[i] = anova(fm0glm, fm1glm, test = "Chisq")$"Pr(>Chi)"[2]
  
  print(i)
 }

# Type I error rate of glmer models
table(pval_glmer<0.05)[2]/length(pval_glmer)

# TRUE 
# 0.038 

# Type I error rate of the equivalent glm models
table(pval_glm<0.05)[2]/length(pval_glm)

# TRUE 
# 0.078 

}

}




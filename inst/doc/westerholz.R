
## ----eval=FALSE----------------------------------------------------------
## install.packages("expp")


## ----echo=FALSE, message=FALSE-------------------------------------------
require(rgeos); require(sp); require(spdep); require(spatstat); require(deldir)
par(mar = c(0,0,1,0) )


## ------------------------------------------------------------------------
require(expp)


## ----eval=FALSE----------------------------------------------------------
## help(westerholzBreeding)
## help(westerholzEPP)


## ----, eval = FALSE------------------------------------------------------
## data(westerholzBreeding)
## head(westerholzBreeding)


## ----, eval = FALSE------------------------------------------------------
## data(westerholzEPP)
## head(westerholzEPP)


## ----, echo=FALSE, results='asis'----------------------------------------
data(westerholzBreeding)
knitr::kable(head(westerholzBreeding))


## ----, echo=FALSE, results='asis'----------------------------------------
data(westerholzEPP)
knitr::kable(head(westerholzEPP))


## ------------------------------------------------------------------------
b = split(westerholzBreeding, westerholzBreeding$year_)
e = split(westerholzEPP, westerholzEPP$year_) 

# sample sizes by year
lapply(b, nrow)
lapply(e, nrow)


## ----tidy=FALSE----------------------------------------------------------
breedingDat = lapply(b, SpatialPointsBreeding, coords= ~x+y, id='id', breeding= ~male + female)
eppDat = lapply(e, eppMatrix, pairs = ~ male + female)




## ------------------------------------------------------------------------
polygonsDat = lapply(breedingDat, DirichletPolygons)


## ------------------------------------------------------------------------
maxlag = 10
O = mapply(FUN = epp, breedingDat, polygonsDat, eppDat, maxlag)


## ----results='hide', dpi=100, fig.width=7, fig.height=10, fig.align='left'----
#op = par(mfrow = c(1,2))

for(year in c("2009", "2010") ) { 
  plot(O[[year]], cex = 0.7, lwd = .5, border = "navy" )
  title(main = year)
  }

# par(op)


## ----, warning=FALSE, dpi=100, fig.width=6, fig.height=8, fig.align='left'----
year = '2010'
box = 110
O10 = O[[year]]
plot(O10 , zoom = box, maxlag = 2,cex = .7,  border = 'white', col = 'grey70', zoom.col = "bisque")



## ----results='hide',fig.width=8, fig.height=6----------------------------
op = par(mfrow = c(1,2))
    
barplot(O[[1]],relativeValues = TRUE, main = 2009) 
legend(x="topright", legend = c('Observed', 'Potential'), lty = c(1, 2),bty='n')
barplot(O[[2]], relativeValues = TRUE, main = 2010)

par(op)


## ------------------------------------------------------------------------
dat = lapply(O, as.data.frame) # a list of data.frame-s
dat = do.call(rbind, dat)
dat$year_ = dat$year__MALE; dat$year__FEMALE = NULL



## ------------------------------------------------------------------------
dat$rank = dat$rank - min(dat$rank)
table(dat$rank)


## ------------------------------------------------------------------------
center = function(x) { return(x - mean(x, na.rm = TRUE)) }
scale2 = function(x) { return(x/(2*sd(x, na.rm = TRUE))) }

# Compute asynchrony
dat$asynchrony = abs(dat$layingDate_MALE - dat$layingDate_FEMALE)

#a Compute relative within-rank asynchrony
MALE_splitBy = paste(dat$year_, dat$id_MALE, dat$male, dat$rank, sep = "_")
dat$relative_asynchrony_MALE = unsplit(lapply(split(dat$asynchrony, MALE_splitBy), center), MALE_splitBy)
dat$relative_asynchrony_MALE = scale2(dat$relative_asynchrony_MALE)

FEMALE_splitBy = paste(dat$year_, dat$id_FEMALE, dat$female, dat$rank, sep = "_")
dat$relative_asynchrony_FEMALE = unsplit(lapply(split(dat$asynchrony, FEMALE_splitBy), center), FEMALE_splitBy)
dat$relative_asynchrony_FEMALE = scale2(dat$relative_asynchrony_FEMALE)


## ----eval=FALSE----------------------------------------------------------
## table(dat$epp, dat$year_) #extra-pair frequency by year.


## ----echo = FALSE, results='asis'----------------------------------------
knitr::kable(table(dat$epp, dat$year_))


## ----eval=FALSE----------------------------------------------------------
## require(lme4)
## dat$age2 = ifelse(dat$male_age_MALE == 'juv', 1, 2)
## fm = glmer(epp ~ rank + male_age_MALE + relative_asynchrony_MALE + relative_asynchrony_FEMALE +
##              (1|male) + (1|female) + (1|year_), data = dat, family = binomial)
## summary(fm)



<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Blue Tits Case study}
-->

Extra-pair paternity in Blue Tits (_Cyanistes caeruleus_): a case study from Westerholz, Bavaria 
========================================================================================================
Supplement to Schlicht, Valcu and Kempenaers _"Spatial patterns of extra-pair paternity: beyond paternity gains and losses"_ (in prep.)
-------------------------------------------------------------------------------------------------------------------------------------


### 1. Getting started
 * Download and install [R](http://cran.rstudio.com/).
 * Open R, and install _expp_ by copying the following line into your R console:  

```r
install.packages("expp")
```

  


 * To load _expp_ type:   

```r
require(expp)
```


### 2. Load datasets
For info on the data-sets type: 

```r
help(westerholzBreeding)
help(westerholzEPP)
```


```r
data(westerholzBreeding)
head(westerholzBreeding)
```


```r
data(westerholzEPP)
head(westerholzEPP)
```


|  year_|   id|        x|        y|  layingDate|male_age  |female_age  |female  |male  |
|------:|----:|--------:|--------:|-----------:|:---------|:-----------|:-------|:-----|
|   2009|  277|  4417476|  5335000|          99|adult     |adult       |f11     |m16   |
|   2009|   82|  4417816|  5334307|         106|adult     |adult       |f23     |m48   |
|   2010|  214|  4417785|  5334719|         111|adult     |juv         |f63     |m35   |
|   2009|   59|  4417518|  5334309|         104|adult     |adult       |f7      |m10   |
|   2009|   97|  4417227|  5334376|         103|adult     |adult       |f18     |m28   |
|   2010|   95|  4417285|  5334377|         107|adult     |adult       |f18     |m28   |

***** 
|  year_|female  |male  |
|------:|:-------|:-----|
|   2009|f29     |m16   |
|   2009|f53     |m16   |
|   2009|f36     |m16   |
|   2009|f47     |m41   |
|   2009|f51     |m79   |
|   2009|f38     |m13   |





### 3. Prepare data

#### 3.1 Split by year (each year needs to be processed separately)


```r
b = split(westerholzBreeding, westerholzBreeding$year_)
e = split(westerholzEPP, westerholzEPP$year_)

# sample sizes by year
lapply(b, nrow)
```

```
## $`2009`
## [1] 56
## 
## $`2010`
## [1] 96
```

```r
lapply(e, nrow)
```

```
## $`2009`
## [1] 29
## 
## $`2010`
## [1] 32
```


#### 3.2 Run a couple of helper functions on both breeding data and extra-pair paternity data 

```r
breedingDat = lapply(b, SpatialPointsBreeding, coords= ~x+y, id='id', breeding= ~male + female)
eppDat = lapply(e, eppMatrix, pairs = ~ male + female)

```


#### 3.3. Compute Dirichlet polygons based on the `SpatialPointsBreeding` object

```r
polygonsDat = lapply(breedingDat, DirichletPolygons)
```

********************************************************************************

### 4. All the objects are now ready to be processed by the `epp` function.

```r
maxlag = 10
O = mapply(FUN = epp, breedingDat, polygonsDat, eppDat, maxlag)
```

```
## Warning: maxlag of 10 is exceeding the number of possible lags, reverting
## to 8 lags.
```



```r
# op = par(mfrow = c(1,2))

for (year in c("2009", "2010")) {
    plot(O[[year]], cex = 0.7, lwd = 0.5, border = "navy")
    title(main = year)
}
```

```
## Warning: EP male 'm41' not found.
## Warning: EP male 'm110' not found.
## Warning: EP male 'm110' not found.
```

<img src="figure/unnamed-chunk-131.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto auto auto 0;" /><img src="figure/unnamed-chunk-132.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto auto auto 0;" />

```r

# par(op)
```

#### Select one nest-box of a given year and zoom in.

```r
year = "2010"
box = 110
O10 = O[[year]]
plot(O10, zoom = box, maxlag = 2, cex = 0.7, border = "white", col = "grey70", 
    zoom.col = "bisque")
```

<img src="figure/unnamed-chunk-14.png" title="plot of chunk unnamed-chunk-14" alt="plot of chunk unnamed-chunk-14" style="display: block; margin: auto auto auto 0;" />



```r
op = par(mfrow = c(1, 2))

barplot(O[[1]], relativeValues = TRUE, main = 2009)
legend(x = "topright", legend = c("Observed", "Potential"), lty = c(1, 2), bty = "n")
barplot(O[[2]], relativeValues = TRUE, main = 2010)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

```r

par(op)
```


### 5. Fitting a _glmm_ 

#### 5.1 Convert `O` (a list of 2 _epp_ objects) into a `data.frame`.

```r
dat = lapply(O, as.data.frame)  # a list of data.frame-s
dat = do.call(rbind, dat)
dat$year_ = dat$year__MALE
dat$year__FEMALE = NULL
```


#### 5.2. Data transformations prior to modelling.

##### Rescale rank; rank 1 becames rank 0

```r
dat$rank = dat$rank - min(dat$rank)
table(dat$rank)
```

```
## 
##    0    1    2    3    4    5    6    7    8    9 
##  776 1404 1862 2012 1890 1542 1180  774  434  230
```

##### Center and re-scale breeding asynchrony (i.e. the difference in laying data between male and female) within each rank.


```r
center = function(x) {
    return(x - mean(x, na.rm = TRUE))
}
scale2 = function(x) {
    return(x/(2 * sd(x, na.rm = TRUE)))
}

# Compute asynchrony
dat$asynchrony = abs(dat$layingDate_MALE - dat$layingDate_FEMALE)

# a Compute relative within-rank asynchrony
MALE_splitBy = paste(dat$year_, dat$id_MALE, dat$male, dat$rank, sep = "_")
dat$relative_asynchrony_MALE = unsplit(lapply(split(dat$asynchrony, MALE_splitBy), 
    center), MALE_splitBy)
dat$relative_asynchrony_MALE = scale2(dat$relative_asynchrony_MALE)

FEMALE_splitBy = paste(dat$year_, dat$id_FEMALE, dat$female, dat$rank, sep = "_")
dat$relative_asynchrony_FEMALE = unsplit(lapply(split(dat$asynchrony, FEMALE_splitBy), 
    center), FEMALE_splitBy)
dat$relative_asynchrony_FEMALE = scale2(dat$relative_asynchrony_FEMALE)
```


#### 5.3 Run _glmm_
##### Check if sample size is sufficient for the number of variables we aim to include into the model.

```r
table(dat$epp, dat$year_)  #extra-pair frequency by year.
```

|id  |  2009|  2010|
|:---|-----:|-----:|
|0   |  3053|  8991|
|1   |    27|    33|


##### Run the glmm model (this may take a while depending on your system!).

```r
require(lme4)
dat$age2 = ifelse(dat$male_age_MALE == "juv", 1, 2)
fm = glmer(epp ~ rank + male_age_MALE + relative_asynchrony_MALE + relative_asynchrony_FEMALE + 
    (1 | male) + (1 | female) + (1 | year_), data = dat, family = binomial)
summary(fm)
```


```
## Generalized linear mixed model fit by maximum likelihood ['glmerMod']
##  Family: binomial ( logit )
## Formula: epp ~ rank + male_age_MALE + relative_asynchrony_MALE + relative_asynchrony_FEMALE +      (1 | male) + (1 | female) + (1 | year_) 
##    Data: dat 
## 
##      AIC      BIC   logLik deviance 
##    588.3    647.5   -286.2    572.3 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  male   (Intercept) 1.12e+00 1.05790 
##  female (Intercept) 9.87e-02 0.31423 
##  year_  (Intercept) 1.54e-05 0.00393 
## Number of obs: 12104, groups: male, 119; female, 117; year_, 2
## 
## Fixed effects:
##                            Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -3.275      0.245  -13.39  < 2e-16 ***
## rank                         -1.237      0.156   -7.95  1.9e-15 ***
## male_age_MALEjuv             -1.365      0.449   -3.04   0.0023 ** 
## relative_asynchrony_MALE     -0.386      0.408   -0.95   0.3442    
## relative_asynchrony_FEMALE    0.076      0.376    0.20   0.8397    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) rank   m__MAL r__MAL
## rank        -0.449                     
## ml_g_MALEjv -0.382  0.010              
## rltv_s_MALE  0.091  0.017  0.000       
## rlt__FEMALE -0.008 -0.001 -0.024 -0.345
```










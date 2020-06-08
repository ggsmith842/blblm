# blblm

<!-- badges: start -->
<!-- badges: end -->

## Bag of Little Bootstraps: STA141C Final Proj

## Overview

The package blblm is a statistical analysis package used for finding the bootstrap statistic 
from a linear model. The package is able to provide the user with statistics for coefficeints, sigma, 
predicted values, and relevent confidence intervals. 

Additionally this package has been modified from its original version in order to provide
improvements in the speed at which calculations are carried out. 

To learn more about the modifications made please see the "blblm-vignette"

## Here are some examples to get you started
### Examples

``` r
library(blblm)
# Updated version which uses cpp for lm
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

# Parallelization
library(parallel)
cl<-makeCluster(4)
fit <- par_blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100,cl) 
stopCluster(cl)

# Parallelization with chunking

library(parallel)
cl<-makeCluster(4)
fit <- par_blblm_chunk(mpg ~ wt * hp, data = mtcars, m = 3, B = 100,cl,2) #2 chunks
stopCluster(cl)

coef(fit)
#> (Intercept)          wt          hp       wt:hp 
#> 48.88428523 -7.88702986 -0.11576659  0.02600976

confint(fit, c("wt", "hp"))
#>           2.5%       97.5%
#> wt -10.7902240 -5.61586271
#> hp  -0.1960903 -0.07049867

sigma(fit)
#> [1] 1.838911
sigma(fit, confidence = TRUE)
#>    sigma      lwr      upr 
#> 1.838911 1.350269 2.276347

predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#>        1        2 
#> 21.55538 18.80785

predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#>        fit      lwr      upr
#> 1 21.55538 20.02457 22.48764
#> 2 18.80785 17.50654 19.71772

```

## The naive method would be:
##
## rtnorm.naive <- function(n, mean = 0, sd = 1, lower, upper)
## {
##   lower <- (lower - mean) / sd ## Algorithm works on mean 0, sd 1 scale
##   upper <- (upper - mean) / sd

##   x <- rnorm(n)
##   repeat {
##     w <- which( x > upper | x < lower)
##     if (length(w) == 0) break
##     x[w] <- rnorm(length(w))
##   }
##   return(x * sd + mean)
## }
##
## There is also https://cran.r-project.org/web/packages/truncnorm/

## Function to sample according to a truncated normal distribution 
## This function comes from the R package 'msm' maintained by 
## Christopher Jackson <chris.jackson at mrc-bsu.cam.ac.uk>
# 
# Package: msm
# Version: 1.7.1
# Date: 2023-03-23
# Title: Multi-State Markov and Hidden Markov Models in Continuous Time
# Author: Christopher Jackson <chris.jackson@mrc-bsu.cam.ac.uk>
# Maintainer: Christopher Jackson <chris.jackson@mrc-bsu.cam.ac.uk>
# Description: Functions for fitting continuous-time Markov and hidden
#     Markov multi-state models to longitudinal data.  Designed for
#     processes observed at arbitrary times in continuous time (panel data)
#     but some other observation schemes are supported. Both Markov
#     transition rates and the hidden Markov output process can be modelled
#     in terms of covariates, which may be constant or piecewise-constant
#     in time.
# License: GPL (>= 2)
# Imports:
#     survival,mvtnorm,expm
# Suggests:
#     mstate,minqa,doParallel,foreach,numDeriv,testthat,flexsurv
# URL: https://github.com/chjackson/msm
# # Repository: CRAN

## Rejection sampling algorithm by Robert (Stat. Comp (1995), 5, 121-5)
## for simulating from the truncated normal distribution.

rtnorm <- function (n, mean = 0, sd = 1, lower, upper) {
  stopifnot(length(n) == 1L && length(mean) == 1L
            && length(sd) == 1L && length(lower) == 1L && length(upper) == 1L)
  stopifnot(is.finite(lower) && is.finite(upper))
  lower <- (lower - mean) / sd # Algorithm works on mean 0, sd 1 scale
  upper <- (upper - mean) / sd
  sdzero <- (sd < .Machine$double.eps)
  nas <- is.na(mean) || is.na(sd) || (sdzero && (lower > mean || mean > upper))
  ret <- rep_len(NaN, n)
  if (nas) {
    warning("NAs produced")
    return(ret) # return NaN
  } else if (lower > upper) {
    return(ret) # return NaN
  } else if (sdzero) {
    return(rep_len(mean, n)) # SD zero, so set the sampled value to the mean. 
  } else if ((lower < 0) && (upper > 0) && (upper - lower > sqrt(2*pi))) {
    # standard "simulate from normal and reject if outside limits" method. Use if bounds are wide.
    ind.no <- seq_len(n)
    while (length(ind.no) > 0) {
      y <- rnorm(length(ind.no))
      done <- (y >= lower & y <= upper)
      ret[ind.no[done]] <- y[done]
      ind.no <- ind.no[!done]
    }
  } else if (lower >= 0 && (upper > lower + 2*sqrt(exp(1)) /
                            (lower + sqrt(lower^2 + 4)) * exp((lower*2 - lower*sqrt(lower^2 + 4)) / 4))) {
    # rejection sampling with exponential proposal. Use if lower >> mean
    ind.expl <- seq_len(n)
    a <- (sqrt(lower^2 + 4) + lower) / 2
    while (length(ind.expl) > 0) {
      z <- rexp(length(ind.expl), a) + lower
      u <- runif(length(ind.expl))
      done <- (u <= exp(-(z - a)^2 / 2)) & (z <= upper)
      ret[ind.expl[done]] <- z[done]
      ind.expl <- ind.expl[!done]
    }
  } else if (upper <= 0 && (-lower > -upper + 2*sqrt(exp(1)) /
                            (-upper + sqrt(upper^2 + 4)) * exp((upper*2 - -upper*sqrt(upper^2 + 4)) / 4))) {
    # rejection sampling with exponential proposal. Use if upper << mean.
    ind.expu <- seq_len(n)
    a <- (sqrt(upper^2 +4) - upper) / 2
    while (length(ind.expu) > 0) {
      z <- rexp(length(ind.expu), a) - upper
      u <- runif(length(ind.expu))
      done <- (u <= exp(-(z - a)^2 / 2)) & (z <= -lower)
      ret[ind.expu[done]] <- -z[done]
      ind.expu <- ind.expu[!done]
    }
  } else {
    # rejection sampling with uniform proposal. Use if bounds are narrow and central.
    ind.u <- seq_len(n)
    K <- if (lower > 0) lower^2 else if (upper < 0) upper^2 else 0
    while (length(ind.u) > 0) {
      z <- runif(length(ind.u), lower, upper)
      rho <- exp((K - z^2) / 2)
      u <- runif(length(ind.u))
      done <- (u <= rho)
      ret[ind.u[done]] <- z[done]
      ind.u <- ind.u[!done]
    }
  }        
  ret*sd + mean
}

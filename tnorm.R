## Function to sample according to a truncated normal distribution 
## This function comes from the R package 'msm' maintained by 
## Christopher Jackson <chris.jackson at mrc-bsu.cam.ac.uk>
# 
# Package: msm
# Version: 0.9.6
# Date: 2010-02-09
# Title: Multi-state Markov and hidden Markov models in continuous time
# Author: Christopher Jackson <chris.jackson@mrc-bsu.cam.ac.uk>
# Maintainer: Christopher Jackson <chris.jackson@mrc-bsu.cam.ac.uk>
# Description: Functions for fitting general continuous-time Markov and
#         hidden Markov multi-state models to longitudinal data.  Both
#         Markov transition rates and the hidden Markov output process
#         can be modelled in terms of covariates.  A variety of
#         observation schemes are supported, including processes observed
#         at arbitrary times, completely-observed processes, and censored
#         states.
# License: GPL (>= 2)
# Imports: survival,mvtnorm
# Packaged: 2010-02-09 11:11:36 UTC; chris
# Repository: CRAN
# Date/Publication: 2010-02-11 18:33:21





## Rejection sampling algorithm by Robert (Stat. Comp (1995), 5, 121-5)
## for simulating from the truncated normal distribution.

rtnorm <- function (n, mean = 0, sd = 1, lower = -Inf, upper = Inf) {
    if (length(n) > 1)
        n <- length(n)
    mean <- rep(mean, length=n)
    sd <- rep(sd, length=n)
    lower <- rep(lower, length=n)
    upper <- rep(upper, length=n)
    lower <- (lower - mean) / sd ## Algorithm works on mean 0, sd 1 scale
    upper <- (upper - mean) / sd
    ind <- seq(length=n)
    ret <- numeric(n)
    ## Different algorithms depending on where upper/lower limits lie.
    alg <- ifelse(
                  lower > upper,
                  -1,# return NaN if lower > upper
                  ifelse(
                         ((lower < 0 & upper == Inf) |
                          (lower == -Inf & upper > 0) |
                          (is.finite(lower) & is.finite(upper) & (lower < 0) & (upper > 0) & (upper-lower > sqrt(2*pi)))
                          ),
                         0, # standard "simulate from normal and reject if outside limits" method. Use if bounds are wide.  FIXME HSOULD BE 
                         ifelse(
                                (lower >= 0 & (upper > lower + 2*sqrt(exp(1)) /
                                 (lower + sqrt(lower^2 + 4)) * exp((lower*2 - lower*sqrt(lower^2 + 4)) / 4))),
                                1, # rejection sampling with exponential proposal. Use if lower >> mean
                                ifelse(upper <= 0 & (-lower > -upper + 2*sqrt(exp(1)) /
                                       (-upper + sqrt(upper^2 + 4)) * exp((upper*2 - -upper*sqrt(upper^2 + 4)) / 4)),
                                       2, # rejection sampling with exponential proposal. Use if upper << mean.
                                       3)))) # rejection sampling with uniform proposal. Use if bounds are narrow and central.

    
    ind.nan <- ind[alg==-1]; ind.no <- ind[alg==0]; ind.expl <- ind[alg==1]; ind.expu <- ind[alg==2]; ind.u <- ind[alg==3]
    ret[ind.nan] <- NaN
    while (length(ind.no) > 0) {
        y <- rnorm(length(ind.no))
        done <- which(y >= lower[ind.no] & y <= upper[ind.no])
        ret[ind.no[done]] <- y[done]
        ind.no <- setdiff(ind.no, ind.no[done])
    }
    stopifnot(length(ind.no) == 0)
    while (length(ind.expl) > 0) {
        a <- (lower[ind.expl] + sqrt(lower[ind.expl]^2 + 4)) / 2
        z <- rexp(length(ind.expl), a) + lower[ind.expl]
        u <- runif(length(ind.expl))
        done <- which((u <= exp(-(z - a)^2 / 2)) & (z <= upper[ind.expl]))
        ret[ind.expl[done]] <- z[done]
        ind.expl <- setdiff(ind.expl, ind.expl[done])
    }
    stopifnot(length(ind.expl) == 0)
    while (length(ind.expu) > 0) {
        a <- (-upper[ind.expu] + sqrt(upper[ind.expu]^2 +4)) / 2
        z <- rexp(length(ind.expu), a) - upper[ind.expu]
        u <- runif(length(ind.expu))
        done <- which((u <= exp(-(z - a)^2 / 2)) & (z <= -lower[ind.expu]))
        ret[ind.expu[done]] <- -z[done]
        ind.expu <- setdiff(ind.expu, ind.expu[done])
    }
    stopifnot(length(ind.expu) == 0)
    while (length(ind.u) > 0) {
        z <- runif(length(ind.u), lower[ind.u], upper[ind.u])
        rho <- ifelse(lower[ind.u] > 0,
                      exp((lower[ind.u]^2 - z^2) / 2), ifelse(upper[ind.u] < 0,
                                                            exp((upper[ind.u]^2 - z^2) / 2),
                                                            exp(-z^2/2)))
        u <- runif(length(ind.u))
        done <- which(u <= rho)
        ret[ind.u[done]] <- z[done]
        ind.u <- setdiff(ind.u, ind.u[done])
    }
    stopifnot(length(ind.u) == 0)
    ret*sd + mean
}
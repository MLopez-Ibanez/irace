#######################################
## GENERATE CONFIGURATIONS
#######################################

## When called with an unconditional parameter, it
## must return TRUE
conditionsSatisfied <- function(parameters, partialConfiguration, paramName)
{
  condition <- parameters$conditions[[paramName]]
  # If there is no condition, do not waste time evaluating it.
  if (isTRUE(condition)) return(TRUE)

  v <- eval(condition, as.list(partialConfiguration))
  # Return TRUE if TRUE, FALSE if FALSE or NA
  ## FIXME: If we byte-compile the condition, then we should incorporate the
  ## following into the condition directly.
  !is.na(v) && v
}

get_fixed_value <- function(parameters, param)
{
  type <- parameters$types[[param]]
  irace.assert(type == "c")
  parameters$domain[[param]][[1L]]
}

repairConfigurations <- function(x, parameters, repair)
{
  if (!is.null(repair)) {
    # FIXME: Pass the whole newConfigurations to repair and let it handle each row.
    j <- colnames(x)
    for (i in seq_nrow(x))
      set(x, i, j = j, value = repair(as.data.frame(x[i]), parameters))
  }
  x
}

## Calculates the parameter bounds when parameters domain is dependent
getDependentBound <- function(parameters, param, configuration)
{
  domain <- parameters$domain[[param]]
  if (is.expression(domain)) {
    # Depends contains parameters that enable param and parameters that define
    # its domain. If this is a partial configuration, we need only the latter.
    # Use names() here in case the configuration is simply a list.
    deps <- intersect(names(configuration), parameters$depends[[param]])
    # If it depends on a parameter that is disabled, then this is disabled.
    if (anyNA(configuration[deps])) return(NA)

    domain <- sapply(domain, eval, configuration)
    irace.assert(all(is.finite(domain)))
    # Value gets truncated (defined from robotics initial requirements)
    if (parameters$types[param] == "i") domain <- as.integer(domain)
    if (domain[[1L]] > domain[[2L]]) {
      irace.error ("Invalid domain (", paste0(domain, collapse=", "),
                   ") generated for parameter '", param,
                   "' that depends on parameters (",
                   paste0(parameters$depends[[param]], collapse=", "),
                   "). This is NOT a bug in irace. Check the definition of these parameters.")
    }
  }
  domain
}

## Calculates the parameter bounds when the parameter is dependent.
get_dependent_domain <- function(parameters, param, configuration)
{
  # FIXME: Make this function handle a data.table and return a list of domains.
  configuration <- as.list(configuration)
  # Depends contains parameters that enable param and parameters that define
  # its domain. If this is a partial configuration, we need only the latter.
  # Use names() here in case the configuration is simply a list.
  deps <- intersect(names(configuration), parameters$depends[[param]])
  # FIXME: This function should not be called if the parent is disabled.
  # If it depends on a parameter that is disabled, then this is disabled.
  if (anyNA(configuration[deps])) return(NA)

  domain <- parameters$domain[[param]]
  irace.assert(is.expression(domain))
  domain <- sapply(domain, eval, configuration)
  irace.assert(all(is.finite(domain)))
  # Value gets truncated (defined from robotics initial requirements)
  if (parameters$types[param] == "i") domain <- as.integer(domain)
  if (domain[[1L]] > domain[[2L]]) {
    # FIXME: Add test for this error.
    irace.error ("Invalid domain (", paste0(domain, collapse=", "),
                 ") generated for parameter '", param,
                 "' that depends on parameters (",
                 paste0(parameters$depends[[param]], collapse=", "),
                 "). This is NOT a bug in irace. Check the definition of these parameters.")
  }
  domain
}

param_qunif_c <- function(x, domain, transf, digits = NULL)
{
  n <- length(domain)
  z <- integer_round(as.numeric(x) * n + 1L, 1L, n)
  domain[z]
}

param_qunif_i <- function(x, domain, transf, digits = NULL)
{
  upper <- domain[2L]
  lower <- domain[1L]
  x <- as.numeric(x)
  if (transf == "log") {
    # +1 for correct rounding before floor()
    x <- transform_from_log(x, transf, lower, upper + 1L)
  } else {
    x <- x * (upper + 1L - lower) + lower
  }
  integer_round(x, lower, upper)
}

param_qunif_r <- function(x, domain, transf, digits)
{
  upper <- domain[2L]
  lower <- domain[1L]
  x <- as.numeric(x)
  if (transf == "log")
    return(round(transform_from_log(x, transf, lower, upper), digits))
  
  x <- x * (upper - lower) + lower
  clamp(round(x, digits), lower, upper)
}

.param_qunif <- c(i = param_qunif_i, r = param_qunif_r, c = param_qunif_c,
  o = param_qunif_c)

generate_sobol <- function(parameters, n, repair = NULL)
{
  # Do not use .Machine$integer.max to minimize differences between machines.
  seed <- sample.int(2147483647L, size = 1L)
  confs <- spacefillr::generate_sobol_set(n, dim = parameters$nbVariable, seed = seed)
  confs <- data.table(confs)
  setnames(confs, parameters$names[!parameters$isFixed])
  nodep_names <- parameters$names[!parameters$isDependent & !parameters$isFixed]
  # FIXME: How to do this faster using data.table?
  for (x in nodep_names) {
    this_qunif <- .param_qunif[[parameters$types[x]]]
    set(confs, j = x, value = this_qunif(confs[[x]], parameters$domain[[x]], parameters$transform[[x]],
      digits = parameters$digits[x]))
  }
  for (x in parameters$names[parameters$isFixed]) {
    set(confs, j = x, value = parameters$domain[[x]][[1L]])
  }
  setcolorder(confs, parameters$names)
  
  hierarchy <- parameters$hierarchy
  max_level <- max(hierarchy)
  if (max_level > 1L) {
    .NEWVALUE <- .DOMAIN <- NULL # To silence CRAN warnings.
    for (k in seq_len(max_level - 1L)) {
      prev_names <- names(hierarchy)[hierarchy <= k]
      dep_names <- names(hierarchy)[hierarchy == k+1L]
      for (p in dep_names) {
        idx_satisfied <- which_satisfied(confs, parameters$conditions[[p]])
        if (parameters$isDependent[[p]] && length(idx_satisfied) > 0L) {
          this_qunif <- .param_qunif[[parameters$types[p]]]
          confs[idx_satisfied, let(.DOMAIN = list(get_dependent_domain(parameters, p, .SD))), by=.I, .SDcols=prev_names]
          confs[idx_satisfied, .NEWVALUE := this_qunif(.SD, unlist(.DOMAIN), parameters$transform[[p]], digits = parameters$digits[p]), by=.I, .SDcols=p]
          confs[, (p):=.NEWVALUE]
          confs[, let(.NEWVALUE=NULL, .DOMAIN=NULL)]
        } else if (length(idx_satisfied) < n) {
          idx_not_satisfied <- if (length(idx_satisfied))
                                 seq_len(n)[-idx_satisfied] else NULL
          na_value <- switch(parameters$types[p],
            i = NA_integer_,
            r = NA_real_,
            c = NA_character_,
            o = NA_character_,
            irace.internal.error("Unknown type '", parameters$types[p], "'"))
          set(confs, i = idx_not_satisfied, j = p, value = na_value)
        }
      }
    }
  }
  repairConfigurations(confs, parameters, repair)
  set(confs, j = ".PARENT.", value = NA_integer_)
  confs
}

sampleSobol <- function(parameters, n, repair = NULL)
{
  newConfigurations <- generate_sobol(parameters, n, repair)
  newConfigurations <- unique(newConfigurations)
  forbidden <- parameters$forbidden  
  newConfigurations <- filter_forbidden(newConfigurations, forbidden)
  have <- nrow(newConfigurations)
  if (have < n) {
    needed <- max(ceiling(n + (n - have) * (2 - have / n)), min(parameters$nbVariable * 5L, 100L))
    newConfigurations <- generate_sobol(parameters, needed, repair)
    newConfigurations <- unique(newConfigurations)
    newConfigurations <- filter_forbidden(newConfigurations, forbidden)
    if (nrow(newConfigurations) == 0L) {
      irace.error("irace tried to sample a configuration not forbidden without success, perhaps your constraints are too strict?")
    }
    newConfigurations <- truncate_rows(newConfigurations, n)
  }
  setDF(newConfigurations)
  newConfigurations
}


generate_uniform <- function(parameters, nbConfigurations, repair = NULL)
{
  newConfigurations <- configurations_alloc(parameters$names, nrow = nbConfigurations, parameters = parameters)
  namesParameters <- names(parameters$conditions)
  for (currentParameter in namesParameters) {
    # We must be careful because parameters$types does not have the same order
    # as namesParameters, because we sample in the order of the conditions.
    condition <- parameters$conditions[[currentParameter]]
    idx <- which_satisfied(newConfigurations, condition)
    if (length(idx) == 0L)
      next
    currentType <- parameters$types[[currentParameter]]
    if (isFixed(currentParameter, parameters)) {
      # We don't need to sample, there is only one value.
      newVals <- get_fixed_value(parameters, currentParameter)
    } else if (currentType == "r" || currentType == "i") {
      transf <- parameters$transform[[currentParameter]]
      if (parameters$isDependent[[currentParameter]]) {
        if (currentType == "i") {
          newVals <- sapply(idx, function(i) {
            domain <- get_dependent_domain(parameters, currentParameter, newConfigurations[i,])
            sample_i_unif(1L, lower = domain[[1L]], upper = domain[[2L]], transf = transf)
          })
        } else {
          digits <- parameters$digits[[currentParameter]]
          newVals <- sapply(idx, function(i) {
            domain <- get_dependent_domain(parameters, currentParameter, newConfigurations[i,])
            sample_r_unif(1L, lower = domain[[1L]], upper = domain[[2L]], transf = transf, digits = digits)
          })
        }
      } else {
        domain <- parameters$domain[[currentParameter]]
        if (currentType == "i") {
          newVals <- sample_i_unif(length(idx), lower = domain[[1L]], upper = domain[2L], transf = transf)
        } else {
          newVals <- sample_r_unif(length(idx), lower = domain[[1L]], upper = domain[2L], transf = transf, digits = parameters$digits[[currentParameter]])
        }
      }
    } else if (currentType == "c" || currentType == "o") {
      possibleValues <- parameters$domain[[currentParameter]]
      newVals <- sample(possibleValues, length(idx), replace=TRUE)
    } else {
      irace.internal.error("Unknown parameter type '", currentType, "'")
    }
    set(newConfigurations, i = idx, j = currentParameter, value = newVals)
  }
  repairConfigurations(newConfigurations, parameters, repair)
  set(newConfigurations, j = ".PARENT.", value = NA_integer_)
  newConfigurations
}


### Uniform sampling for the initial generation
sampleUniform <- function(parameters, nbConfigurations, repair = NULL)
{
  newConfigurations <- generate_uniform(parameters, nbConfigurations, repair)
  forbidden <- parameters$forbidden  
  if (is.null(forbidden)) {
    setDF(newConfigurations)
    return(newConfigurations)
  }
  
  retries <- 100L
  repeat {
    newConfigurations <- filter_forbidden(newConfigurations, forbidden)
    needed <- nbConfigurations - nrow(newConfigurations)
    if (needed == 0L) {
      setDF(newConfigurations)
      return(newConfigurations)
    }
    newConfigurations <- rbindlist(list(newConfigurations,
                                        generate_uniform(parameters, needed, repair = repair)))
    retries <- retries - 1L
    if (retries == 0L) {
      irace.error("irace tried 100 times to sample uniformly a configuration not forbidden without success, perhaps your constraints are too strict?")
    }
  }
}

sample_from_model <- function(parameters, eliteConfigurations, model,
                              nbNewConfigurations, repair = NULL)
{
  # FIXME: We only need .WEIGHT. from eliteConfigurations.
  ids_elites <- as.integer(names(model[[1L]]))
  irace.assert(identical(ids_elites, eliteConfigurations[[".ID."]]), {
    print(str(ids_elites))
    print(str(eliteConfigurations[[".ID."]]))
  })
  
  namesParameters <- names(parameters$conditions)
  newConfigurations  <- configurations_alloc(namesParameters, nrow = nbNewConfigurations, parameters)
  idx_elites <- sample.int(n = length(ids_elites), size = nbNewConfigurations,
                           prob = eliteConfigurations[[".WEIGHT."]], replace = TRUE)
  
  # Sample a value for every parameter of the new configuration.
  for (currentParameter in namesParameters) {
    condition <- parameters$conditions[[currentParameter]]
    idx_satisfied <- which_satisfied(newConfigurations, condition)
    if (length(idx_satisfied) == 0L)
      next
    if (isFixed(currentParameter, parameters)) {
      # We don't need to sample, there is only one value.
      newVals <- get_fixed_value(parameters, currentParameter)
      set(newConfigurations, i = idx_satisfied, j = currentParameter, value = newVals)
      next
    }
    currentType <- parameters$types[[currentParameter]]
    this_model <- model[[currentParameter]]
    if (currentType == "r" || currentType == "i") {
      transf <- parameters$transform[[currentParameter]]
      if (currentType == "i") {
        sample_num_values <- function(n, mean, sd, lower, upper) {
          if (is.na(mean))
            return(sample_i_unif(n, lower = lower, upper = upper, transf = transf))
          # If parameters are dependent standard deviation must be computed
          # based on the current domain
          sample_i_norm(n, mean, sd, lower = lower, upper = upper, transf = transf)
        }
      } else {
        digits <- parameters$digits[[currentParameter]]
        sample_num_values <- function(n, mean, sd, lower, upper) {
          if (is.na(mean))
            return(sample_r_unif(n, lower = lower, upper = upper, transf = transf, digits = digits))
          sample_r_norm(n, mean, sd, lower = lower, upper = upper, transf = transf, digits = digits)
        }
      }
      if (parameters$isDependent[[currentParameter]]) {
        newVals <-  mapply(function(idx, sd_mean) {
          domain <- get_dependent_domain(parameters, currentParameter, newConfigurations[idx,])
          lower <- domain[[1L]]
          upper <- domain[[2L]]
          sd <- (upper - lower) * sd_mean[[1L]]
          if (sd < .Machine$double.eps) return(lower)
          # If parameters are dependent standard deviation must be computed
          # based on the current domain
          sample_num_values(1L, mean = sd_mean[[2L]], sd = sd, lower = lower, upper = upper)
        }, idx_satisfied, this_model[idx_elites[idx_satisfied]], USE.NAMES=FALSE)
        set(newConfigurations, i = idx_satisfied, j = currentParameter, value = newVals)
        next # We are done with this parameter.
        
      } else {
        domain <- parameters$domain[[currentParameter]]
        sample_values <- function(n, idx) {
          sd_mean <- this_model[[idx]]
          sample_num_values(n, mean = sd_mean[[2L]], sd = sd_mean[[1L]], lower = domain[[1L]], upper = domain[[2L]])
        }
      }
    } else if (currentType == "c") {
      domain <- parameters$domain[[currentParameter]]
      sample_values <- function(n, idx) sample(domain, size = n, replace = TRUE, prob = this_model[[idx]])
    } else if (currentType == "o") {
      domain <- parameters$domain[[currentParameter]]
      sample_values <- function(n, idx) {
        sd_mean <- this_model[[idx]]
        mean <- sd_mean[[2L]]
        # The elite parent does not have any value for this
        # parameter, let's sample uniformly
        if (is.na(mean))
          return(sample(domain, n, replace = TRUE))
        domain[floor(sample_numeric_norm(n, mean, sd = sd_mean[[1L]], lower = 1L, upper = length(domain), transf = ""))]
      }
    } else {
      irace.internal.error("Unknown parameter type '", currentType, "' in sampleModel()")
    }
    # .BY is a list, so take the first argument.
    newConfigurations[idx_satisfied, c(currentParameter) := list(sample_values(.N, .BY[[1L]])), by = idx_elites[idx_satisfied]]
  }
  repairConfigurations(newConfigurations, parameters, repair)
  set(newConfigurations, j = ".PARENT.", value = ids_elites[idx_elites])
  newConfigurations
}

sampleModel <- function(parameters, eliteConfigurations, model,
                        nbNewConfigurations, repair = NULL)
{
  if (nbNewConfigurations <= 0) {
    irace.error ("The number of configurations to generate appears to be negative or zero.")
  }
  newConfigurations <- sample_from_model(parameters, eliteConfigurations,
                                         model, nbNewConfigurations, repair)
  forbidden <- parameters$forbidden
  if (is.null(forbidden)) {
    setDF(newConfigurations)
    return(newConfigurations)
  }
  
  retries <- 100L
  repeat {
    newConfigurations <- filter_forbidden(newConfigurations, forbidden)
    needed <- nbNewConfigurations - nrow(newConfigurations)
    if (needed == 0) {
      setDF(newConfigurations)
      return(newConfigurations)
    }
    tmp <- sample_from_model(parameters, eliteConfigurations, model, needed, repair = repair)
    newConfigurations <- rbindlist(list(newConfigurations, tmp))
    retries <- retries - 1L
    if (retries == 0L) {
      irace.error("irace tried 100 times to sample from the model a configuration not forbidden without success, perhaps your constraints are too strict?")
    }
  }
}

transform_from_log <- function(x, transf, lower, upper)
{
  trLower <- attr(transf, "lower") 
  trUpper <- attr(transf, "upper")
  x <- exp(trLower + (trUpper - trLower) * x)
  clamp(x, lower, upper)
}

transform_to_log <- function(x, transf)
{
  trLower <- attr(transf, "lower") 
  trUpper <- attr(transf, "upper")
  (log(x) - trLower)/(trUpper - trLower)
}
## How to sample integer values?
#
# The problem: If we have an integer with domain [1,3] and we sample a real value
# and round, then there are more chances of getting 2 than 1 or 3:
# [1, 1,5) -> 1
# [1.5, 2,5) -> 2
# [2.5, 3) -> 3
#
# The solution: Sample in [lowerbound, upperbound + 1], that is, [1, 4], then floor():
# [1, 2) -> 1
# [2, 3) -> 2
# [3, 4) -> 3
#
# Why floor() and not trunc()?
# Because trunc(-1.5) -> -1, while floor(-1.5) -> -2, so for a domain [-3,-1]:
#
# [-3, -2) -> -3
# [-2, -1) -> -2
# [-1, 0)  -> -1
#
# Issue 1: We can sample 4 (upperbound + 1). In that case, we return 3.
#
# Issue 2: When sampling from a truncated normal distribution, the extremes are
# not symmetric.
#
# nsamples <- 100000
# table(floor(rtnorm(nsamples, mean=1, sd=1, lower=1,upper=4)))/nsamples
# table(floor(rtnorm(nsamples, mean=3, sd=1, lower=1,upper=4)))/nsamples
#
# To make them symmetric, we translate by 0.5, so that the mean is at the
# actual center of the interval that will produce the same value after
# truncation, e.g., given an integer value of 1, then mean=1.5, which is at the
# center of [1,2).
#
# nsamples <- 100000
# table(floor(rtnorm(nsamples, mean=1.5, sd=1, lower=1,upper=4)))/nsamples
# table(floor(rtnorm(nsamples, mean=3.5, sd=1, lower=1,upper=4)))/nsamples
#
# The above reasoning also works for log-transformed domains, because 
# floor() happens in the original domain, not in the log-transformed one,
# except for the case of log-transformed negative domains, where we have to
# translate by -0.5.
#
integer_round <- function(x, lower, upper)
{
  x <- floor(x)
  # The probability of this happening is very small, but it happens.
  x <- pmin.int(upper, x)
  irace.assert(all(x >= lower))
  as.integer(x)
}

sample_numeric_unif <- function(n, lower, upper, transf)
{
  if (transf == "log") {
    value <- runif(n, min = 0, max = 1)
    return(transform_from_log(value, transf, lower, upper))
  }
  runif(n, min = lower, max = upper)
}

sample_numeric_norm <- function(n, mean, sd, lower, upper, transf)
{
  if (transf == "log") {
    x <- transform_to_log(mean, transf)
    x <- rtnorm(n, x, sd, lower = 0, upper = 1)
    return(transform_from_log(x, transf, lower, upper))
  }
  rtnorm(n, mean, sd, lower, upper)
}

sample_i_unif <- function(n, lower, upper, transf)
{
  # Dependent domains could be not available because of inactivity of parameters
  # on which they are depedent. In this case, the dependent parameter becomes 
  # not active and we return NA.
  if (is.na(lower) || is.na(upper)) return(NA_integer_)
  # +1 for correct rounding before floor()
  value <- sample_numeric_unif(n, lower, 1L + upper, transf)
  # We use original upperBound, not the +1L for 'i'.
  integer_round(value, lower, upper)
}

sample_r_unif <- function(n, lower, upper, transf, digits)
{
  # Dependent domains could be not available because of inactivity of parameters
  # on which they are depedent. In this case, the dependent parameter becomes 
  # not active and we return NA.
  if (is.na(lower) || is.na(upper)) return(NA)
  x <- sample_numeric_unif(n, lower, upper, transf)
  x <- round(x, digits)
  irace.assert(all(x >= lower) && all(x <= upper))
  x
}

sample_i_norm <- function(n, mean, sd, lower, upper, transf)
{
  if (is.na(lower) || is.na(upper)) return(NA_integer_)
  # + 0.5 because negative domains are log-transformed to positive domains.
  value <- sample_numeric_norm(n, mean + 0.5, sd, lower = lower,
                               # +1 for correct rounding before floor()
                               upper = 1L + upper,
                               transf = transf)
  # We use original upperBound, not the +1L for 'i'.
  integer_round(value, lower, upper)
}

sample_r_norm <- function(n, mean, sd, lower, upper, transf, digits)
{
  # Dependent domains could be not available because of inactivity of parameters
  # on which they are depedent. In this case, the dependent parameter becomes 
  # not active and we return NA.
  if (is.na(lower) || is.na(upper)) return(NA)
  x <- sample_numeric_norm(n, mean, sd, lower, upper, transf)
  x <- round(x, digits)
  irace.assert(all(x >= lower) && all(x <= upper))
  x
}

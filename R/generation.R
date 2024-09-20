#######################################
## GENERATE CONFIGURATIONS
#######################################

## When called with an unconditional parameter, it must return TRUE.
conditionsSatisfied <- function(condition, partialConfiguration)
{
  # If there is no condition, do not waste time evaluating it.
  if (isTRUE(condition)) return(TRUE)

  v <- eval(condition, as.list(partialConfiguration))
  # Return TRUE if TRUE, FALSE if FALSE or NA
  ## FIXME: If we byte-compile the condition, then we should incorporate the
  ## following into the condition directly.
  !is.na(v) && v
}

repairConfigurations <- function(x, parameters, repair)
{
  if (!is.null(repair)) {
    # FIXME: Pass the whole newConfigurations to repair and let it handle each row.
    j <- parameters$names
    df <- as.data.frame(x)
    for (i in seq_nrow(x))
      set(x, i = i, j = j, value = repair(df[i, parameters$names], parameters))
  }
  x
}

## Calculates the parameter bounds when parameters domain is dependent
getDependentBound <- function(param, configuration)
{
  domain <- param[["domain"]]
  if (is.expression(domain)) {
    deps <- all.vars(domain)
    # If it depends on a parameter that is disabled, then this is disabled.
    if (anyNA(configuration[deps])) return(NA)
    domain <- sapply(domain, eval, configuration)
    irace.assert(all(is.finite(domain)))
    # Value gets truncated (defined from robotics initial requirements)
    if (param[["type"]] == "i") domain <- as.integer(domain)
    if (domain[[1L]] > domain[[2L]]) {
      irace.error ("Invalid domain (", paste0(domain, collapse=", "),
                   ") generated for parameter '", param[["name"]],
                   "' that depends on parameters (", paste0(deps, collapse=", "),
                   "). This is NOT a bug in irace. Check the definition of these parameters.")
    }
  }
  domain
}

## Calculates the parameter bounds when the parameter is dependent.
get_dependent_domain <- function(param, configuration)
{
  # FIXME: Make this function handle a data.table and return a list of domains.
  configuration <- as.list(configuration)
  domain <- param[["domain"]]
  deps <- all.vars(domain)
  # FIXME: This function should not be called if the parent is disabled.
  # If it depends on a parameter that is disabled, then this is disabled.
  if (anyNA(configuration[deps])) return(NA)

  irace.assert(is.expression(domain))
  domain <- sapply(domain, eval, configuration, USE.NAMES=FALSE)
  irace.assert(all(is.finite(domain)))
  # Value gets truncated (defined from robotics initial requirements)
  if (param[["type"]] == "i") domain <- as.integer(domain)
  if (domain[[1L]] > domain[[2L]]) {
    # FIXME: Add test for this error.
    irace.error ("Invalid domain (", paste0(domain, collapse=", "),
                 ") generated for parameter '", param[["name"]],
                 "' that depends on parameters (", paste0(deps, collapse=", "),
                 "). This is NOT a bug in irace. Check the definition of these parameters.")
  }
  domain
}


generate_sobol <- function(parameters, n, repair = NULL)
{
  confs <- spacefillr::generate_sobol_set(n, dim = parameters$nbVariable,
    seed = runif_integer(size = 1L))
  confs <- data.table(confs)
  setnames(confs, parameters$names_variable)
  hierarchy <- parameters$hierarchy
  nodep_names <- names(hierarchy)[(hierarchy == 1L) & !parameters$isFixed]
  # FIXME: How to do this faster using data.table?
  for (x in nodep_names)
    set(confs, j = x, value = param_quantile(parameters$get(x), confs[[x]]))
  
  for (x in parameters$names_fixed)
    set(confs, j = x, value = parameters$domains[[x]])
  
  setcolorder(confs, parameters$names)
  
  max_level <- max(hierarchy)
  if (max_level > 1L) {
    .NEWVALUE <- .DOMAIN <- NULL # To silence CRAN warnings.
    for (k in seq_len(max_level - 1L)) {
      prev_names <- names(hierarchy)[hierarchy <= k]
      dep_names <- names(hierarchy)[hierarchy == k+1L]
      for (p in dep_names) {
        param <- parameters$get(p)
        idx_satisfied <- which_satisfied(confs, param[["condition"]])
        if (param$isFixed) {
          # If somehow this fixed parameter was not satisfied sometimes, just set its value to NA.
          confs[!idx_satisfied, (p):= NA ]
        } else if (length(idx_satisfied)) {
          if (param[["is_dependent"]]) {
            confs[idx_satisfied, let(.DOMAIN = list(get_dependent_domain(param, .SD))), by=.I, .SDcols=prev_names]
            confs[idx_satisfied, .NEWVALUE := param_quantile(param, .SD, domain = unlist(.DOMAIN)), by=.I, .SDcols=p]
            confs[, (p):=.NEWVALUE]
            confs[, let(.NEWVALUE=NULL, .DOMAIN=NULL)]
          } else {
            confs[idx_satisfied, .NEWVALUE := param_quantile(param, unlist(.SD)), .SDcols=p]
            confs[, (p):=.NEWVALUE]
            confs[, let(.NEWVALUE=NULL)]
          }
        } else {
          na_value <- switch(param[["type"]],
            i = NA_integer_,
            r = NA_real_,
            c = NA_character_,
            o = NA_character_,
            irace.internal.error("Unknown type '", param[["type"]], "'"))
          set(confs, j = p, value = na_value)
        }
      }
    }
  }
  repairConfigurations(confs, parameters, repair)
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
  set(newConfigurations, j = ".PARENT.", value = NA_integer_)
  newConfigurations
}


generate_uniform <- function(parameters, nbConfigurations, repair = NULL)
{
  newConfigurations <- configurations_alloc(parameters[["names"]],
    nrow = nbConfigurations, parameters = parameters)
  # We sample in the order of the conditions.
  for (param in parameters$get_ordered()) {
    pname <- param[["name"]]
    idx <- which_satisfied(newConfigurations, param[["condition"]])
    if (length(idx) == 0L)
      next
    if (param[["isFixed"]]) {
      # We don't need to sample, there is only one value.
      set(newConfigurations, i = idx, j = pname, value = param[["domain"]])
      next
    }
    if (param[["is_dependent"]]) {
      sample_dep_unif <- function(x)
        sample_unif(param, n = 1L, domain = get_dependent_domain(param, x))
      newConfigurations[idx, c(pname) := sample_dep_unif(.SD), by=.I, .SDcols=parameters$depends[[pname]]]
      ## newVals <- sapply(idx, function(i) {
      ##   domain <- get_dependent_domain(param, newConfigurations[i,])
      ##   runif(param, n = 1L, domain = domain)
      ## })
    } else {
      set(newConfigurations, i = idx, j = pname, value = sample_unif(param, n = length(idx)))
    }
  }
  repairConfigurations(newConfigurations, parameters, repair)
  newConfigurations
}


### Uniform sampling for the initial generation
sampleUniform <- function(parameters, nbConfigurations, repair = NULL)
{
  newConfigurations <- generate_uniform(parameters, nbConfigurations, repair)
  forbidden <- parameters$forbidden  
  if (!is.null(forbidden)) {
    retries <- 100L
    repeat {
      newConfigurations <- filter_forbidden(newConfigurations, forbidden)
      needed <- nbConfigurations - nrow(newConfigurations)
      if (needed == 0L)
        break
      newConfigurations <- rbindlist(list(newConfigurations,
        generate_uniform(parameters, needed, repair = repair)))
      retries <- retries - 1L
      if (retries == 0L) {
        irace.error("irace tried 100 times to sample uniformly a configuration not forbidden without success, perhaps your constraints are too strict?")
      }
    }
  }
  set(newConfigurations, j = ".PARENT.", value = NA_integer_)
  newConfigurations
}

sample_from_model <- function(parameters, eliteConfigurations, model,
                              nbNewConfigurations, repair = NULL)
{
  # FIXME: We only need .WEIGHT. from eliteConfigurations.
  ids_elites <- names(model[[1L]])
  irace.assert(identical(as.integer(ids_elites), as.integer(eliteConfigurations[[".ID."]])), {
    print(str(ids_elites))
    print(str(eliteConfigurations[[".ID."]]))
  })
  
  newConfigurations  <- configurations_alloc(parameters$names, nrow = nbNewConfigurations, parameters)
  idx_elites <- sample.int(n = length(ids_elites), size = nbNewConfigurations,
                           prob = eliteConfigurations[[".WEIGHT."]], replace = TRUE)
  .PARENT. <- NULL # Silence CRAN warning.
  set(newConfigurations, j = ".PARENT.", value = ids_elites[idx_elites])
  # Sample a value for every parameter of the new configuration.
  for (param in parameters$get_ordered()) {
    idx_satisfied <- which_satisfied(newConfigurations, param[["condition"]])
    if (length(idx_satisfied) == 0L)
      next
    if (param[["isFixed"]]) {
      # We don't need to sample, there is only one value.
      set(newConfigurations, i = idx_satisfied, j = param[["name"]], value = param[["domain"]])
      next
    }
    pname <- param[["name"]]
    this_model <- model[[pname]]
    if (param[["is_dependent"]]) {
      sample_model <- if (param[["type"]] == "i") sample_model.ParamInt else sample_model.ParamReal
      dep_rmodel <- function(x, sd_mean) {
        domain <- get_dependent_domain(param, x)
        if (is.na(domain[[1L]])) return(NA)
        # If parameters are dependent standard deviation must be computed
        # based on the current domain
        sd <- (domain[[2L]] - domain[[1L]]) * sd_mean[[1L]]
        if (sd < .Machine$double.eps) return(domain[[1L]])
        sample_model(param, n = 1L, model = c(sd, sd_mean[[2L]]), domain = domain)
      }
      newConfigurations[idx_satisfied, c(pname) := dep_rmodel(.SD, this_model[[.PARENT.]]), by=.I, .SDcols=parameters$depends[[pname]]]
      next # We are done with this parameter.
    }
    # .BY is a list, so take the first argument.
    newConfigurations[idx_satisfied,
      c(pname) := list(sample_model(param, .N, this_model[[ .BY[[1L]] ]])),
      by = .PARENT.]
  }
  newConfigurations[, .PARENT. := as.integer(.PARENT.)]
  repairConfigurations(newConfigurations, parameters, repair)
  newConfigurations
}

sampleModel <- function(parameters, eliteConfigurations, model,
                        nbNewConfigurations, repair = NULL)
{
  if (nbNewConfigurations <= 0)
    irace.error ("The number of configurations to generate appears to be negative or zero.")
  newConfigurations <- sample_from_model(parameters, eliteConfigurations,
                                         model, nbNewConfigurations, repair)
  forbidden <- parameters$forbidden
  if (!is.null(forbidden)) {
    retries <- 100L
    repeat {
      newConfigurations <- filter_forbidden(newConfigurations, forbidden)
      needed <- nbNewConfigurations - nrow(newConfigurations)
      if (needed == 0L)
        break
      tmp <- sample_from_model(parameters, eliteConfigurations, model, needed, repair = repair)
      newConfigurations <- rbindlist(list(newConfigurations, tmp))
      retries <- retries - 1L
      if (retries == 0L) {
        irace.error("irace tried 100 times to sample from the model a configuration not forbidden without success, perhaps your constraints are too strict?")
      }
    }
  }
  newConfigurations
}









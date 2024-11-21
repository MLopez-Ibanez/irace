# Checks that variables in the expressions are within the parameters names.
check_domain_dependencies <- function (params, depends, types)
{
  allnames <- names(params)
  allowed_fx <- c("+", "-", "*", "/", "%%", "min", "max", "round", "floor", "ceiling", "trunc")
  depends <- Filter(length, depends)
  for (p in names(depends)) {
    parameter <- params[[p]]
    domain <- parameter[["domain"]]
    vars <- depends[[p]]
    flag <- vars %in% allnames
    if (!all(flag)) {
      irace.error ("Domain (", paste0(domain, collapse=", "),
                   ") of parameter '", p, "' is not valid: '",
                   paste0(vars[!flag], collapse=", "),
                   "' cannot be found in the scenario parameters: ",
                   paste0(allnames, collapse=", ")," .")
    }
    flag <- types[vars] %in% c("i", "r")
    if (!all(flag)) {
      irace.error ("Domain of parameter '", p, "' depends on non-numerical", 
        " parameters: ", paste0(vars[!flag], collapse=", "), " .")
    }

    # Supported operations for dependent domains
    fx <- setdiff(all.names(domain, unique=TRUE), all.vars(domain, unique=TRUE))
    flag <- fx %in% allowed_fx
    if (!all(flag)) {
      irace.error ("Domain of parameter '", p, "' uses function(s) ", 
                   "not yet supported by irace: ",
                   paste0(fx[!flag], collapse=", "), " .")
    }
  }
  invisible(TRUE)
}

check_forbidden_params <- function(x, pnames, filename = NULL)
{
  if (length(x) == 0L) return(invisible())
  if (any(unique(unlist(lapply(x, all.names))) %in% c("&&", "||"))) {
    for (ex in x) {
      if (any(all.names(ex) %in% c("&&", "||")))
        irace.error("Please use '&' and '|' instead of '&&' and '|' in: ", deparse(ex), " .\n")
    }
  }
  if (all(all.vars(x) %in% pnames)) return(invisible())
  for (ex in x) {
    v <- setdiff(all.vars(ex), pnames)
    if (length(v)) {
      v <- paste0(v, collapse=", ")
      if (is.null(filename)) {
        irace.error("Forbidden expression '", deparse(ex), "' contains unknown parameter(s): ", v)
      } else if (is.na(filename)) {
        irace.error("Expression '", deparse(ex), "' after [forbidden] contains unknown parameter(s): ", v)
      } else {
        irace.error("Expression '", deparse(ex), "' after [forbidden] in '", filename, "' contains unknown parameter(s): ", v)
      }
    }
  }
}


# ---------------------------------------------------------------------------
# Ordering of the parameters according to conditions hierarchy
# *  The conditions hierarchy is an acyclic directed graph.
#    Function treeLevel() computes an order on vertex s.t:
#    level(A) > level(B)  <=>  There is an arc A ---> B
#    (A depends on B to be activated)
# *  If a cycle is detected, execution is stopped
# *  If a parameter depends on another one not defined, execution is stopped
param_level <- function(paramName, varsTree, rootParam = paramName)
{
  # The last parameter is used to record the root parameter of the
  # recursive call in order to detect the presence of cycles.
  vars <- varsTree[[paramName]]
  if (length(vars) == 0L) return(1L) # This parameter does not have conditions

  # This parameter has some conditions
  # Recursive call: level <- MAX( level(m) : m in children )
  maxChildLevel <- 0L
  for (child in vars) {
    # The following line detects cycles
    if (child == rootParam)
      irace.error("Cycle detected in subordinate parameters! ",
                  "Check definition of conditions and/or dependent domains.\n",
                  "One parameter of this cycle is '", rootParam, "'")
      
    # The following line detects a missing definition
    if (child %not_in% names(varsTree))
      irace.error("A parameter definition is missing! ",
                  "Check definition of parameters.\n",
                  "Parameter '", paramName,
                  "' depends on '", child, "' which is not defined.")
        
    level <- param_level(child, varsTree, rootParam)
    if (level > maxChildLevel)
      maxChildLevel <- level
  }
  maxChildLevel + 1L
}


transform_domain <- function(transf, domain, type)
{
  # We do not support transformation of dependent parameters, yet
  # TODO: think about dependent domain transformation
  if (is.expression(domain))
    stop("Parameter domain transformations are not yet available for",
         " dependent parameter domains.")

  lower <- domain[1L]
  upper <- domain[2L]

  if (transf == "log") {
    # Reject log if domain contains zero or negative values
    if (any(domain <= 0))
      stop("Domain (", lower, ", ", upper, ") of parameter of type 'log' contains non-positive values")
    
    trLower <- log(lower)
    # +1 to adjust before floor()
    trUpper <- if (type == "i") log(upper + 1) else log(upper)
    
    irace.assert(is.finite(trLower))
    irace.assert(is.finite(trUpper))
    attr(transf, "lower") <- trLower
    attr(transf, "upper") <- trUpper
    return(transf)
  }
  stop("unrecognized transformation type '", transf, "'")
}

named_stop <- function(name, ...)
  stop(errorCondition(paste0(..., collapse=""), class = name, call = sys.call(-1)))

param_parse_condition <- function(x, type)
{
  if (is.language(x))
    x <- as.expression(x)
  else if (is.character(x)) {
    x <- tryCatch(str2expression(x), error = function(e) {
      msg <- sub("<text>:[0-9]+:[0-9]+: ", "", conditionMessage(e), perl=TRUE)
      msg <- sub("\n1:", "\n ", msg, perl=TRUE)
      stop("Invalid condition '", x, "': ", msg, "\n")
    })
  }
  if (!is.expression(x))
      stop("Invalid condition '", x, "'")
  if (any(all.names(x) %in% c("&&", "||")))
    stop("Please use '&' and '|' instead of '&&' and '|' in: ", deparse(x[[1L]]), " .\n")
  x
}

valid_real_bound <- function(x, digits)
{
  if (is.na(x) || x == 0) return(TRUE)
  rx <- round(x, digits = digits)
  ((abs(rx - x) <= .irace_tolerance * max(1, abs(x))) && digits >= -log10(abs(x)))
}

parse_exp_domain <- function(domain)
{
  # Convert to expression if string
  domain <- sapply(domain, USE.NAMES=FALSE, function(x) if (is.character(x)) str2expression(x) else x)
  # If is an expression or language without variables, then evaluate to convert it to numeric.
  domain <- sapply(domain, USE.NAMES=FALSE, function(x) if (is.numeric(x) || length(all.vars(x, unique=FALSE))) x else eval(x))
  domain
}

Parameter <- function(name, type, domain, label, condition, transf,
                      digits = if (type=='r') 15L else NULL)
{
  cls <- switch(type, i="ParamInt", r="ParamReal", c="ParamCat", o="ParamOrd",
    stop("Unknown parameter type '", type, "'."))

  if (!isTRUE(condition)) {
    condition <- param_parse_condition(condition)
  }
  
  if (type %in% c("i", "r")) {
    exp_domain <- parse_exp_domain(domain)
    domain <- sapply(exp_domain, USE.NAMES=FALSE, function(x) if (is.numeric(x)) x else NA)
    if (type == "r") {
      # digits >= 15 is almost infinite-precision so we do not complain.
      digits <- as.integer(digits)
      if (digits < 15L &&
          (!valid_real_bound(domain[1L], digits)
            || !valid_real_bound(domain[2L], digits))) { 
        for (i in seq.int(digits+1L, 15L)) {
          if (valid_real_bound(domain[1L], i) && valid_real_bound(domain[2L], i))
            break
        }
        stop("Domain bounds (", domain[1L], ", ", domain[2L],
             ") of parameter '", name, "' of type 'real' must be representable within the given 'digits=",
             digits, "'; you would need at least 'digits=", i, "' or adjust the domain")
      }
      if (anyNA(domain))
        domain <- as.expression(exp_domain)
    } else { # type == "i"
      if (any(!is.wholenumber(domain[!is.na(domain)])))
        named_stop("invalid_domain", "for parameter '", name, "' of type 'integer', values must be integers")
      if (anyNA(domain)) 
        domain <- as.expression(sapply(exp_domain, USE.NAMES=FALSE, function(x) if (is.numeric(x)) as.integer(x) else x))
      else
        domain <- as.integer(domain)
    }
    if (!is.expression(domain) && domain[1L] >= domain[2L])
      named_stop("invalid_range", "lower bound must be smaller than upper bound in numeric range")
    
  } else { # type %in% c("c", "o")
    if (anyDuplicated(domain)) {
      dups <- duplicated(domain)
      stop("duplicated values (", paste0('\"', domain[dups], "\"", collapse = ', '), ") for parameter '", name, "'")
    }
    if (type == "o") {
      tmp <- suppressWarnings(as.numeric(domain))
      if (!anyNA(tmp) && !identical(order(tmp), seq_along(tmp)))
        stop("the domain of parameter '", name, "' appears to be a discretization of a numerical range, but the values are not in increasing order: ",
          paste0(domain, collapse = ', '))
    }
  }
  
  if (transf != "")
    transf <- transform_domain(transf, domain, type)

  isFixed <- (type == "c" || type == "o") && (length(domain) == 1L)

  if (type == "r") {
    param <- list(name = name, type = type, domain = domain, label = label, is_dependent = is.expression(domain),
      isFixed = isFixed, transform = transf, condition = condition, digits = digits)
  } else {
    param <- list(name = name, type = type, domain = domain, label = label, is_dependent = is.expression(domain),
      isFixed = isFixed, transform = transf, condition = condition)
  }
  class(param) <- c(cls, "Parameter", class(param))
  param
}

#' Create a parameter space to be tuned.
#'
#' @description
#'  - `param_cat()` creates a categorical parameter.
#'  - `param_ord()` creates an ordinal parameter.
#'  - `param_real()` creates a real-valued parameter.
#'  - `param_int()` creates an integer parameter.
#' 
#' @param ... one or more parameters created by `param_int()`, `param_real()`, `param_ord()`, or `param_cat()`.
#' @param forbidden (`expression()|character(1)`)\cr String or list of expressions that define forbidden parameter configurations.
#' @inheritParams readParameters
#'
#' @return (`ParameterSpace`)
#' @name parameters
#' @examples
#' digits <- 4L
#' parametersNew(
#'    param_cat(name = "algorithm", values = c("as", "mmas", "eas", "ras", "acs"), label = "--"),
#'    param_ord(name = "localsearch", values = c("0", "1", "2", "3"), label = "--localsearch "),
#'    param_real(name = "alpha", lower = 0.0, upper=5.0, label = "--alpha ", digits = digits),
#'    param_real(name = "beta", lower = 0.0, upper = 10.0, label = "--beta ", digits = digits),
#'    param_real(name = "rho", lower = 0.01, upper = 1.00, label = "--rho ", digits = digits),
#'    param_int(name = "ants", lower = 5, upper = 100, transf = "log", label = "--ants "),
#'    param_real(name = "q0", label = "--q0 ", lower=0.0, upper=1.0,
#'               condition = expression(algorithm == "acs")),
#'    param_int(name = "rasrank", label = "--rasranks ", lower=1, upper=quote(min(ants, 10)),
#'              condition = 'algorithm == "ras"'),
#'    param_int(name = "elitistants", label = "--elitistants ", lower=1, upper=expression(ants),
#'              condition = 'algorithm == "eas"'),
#'    param_int(name = "nnls", label = "--nnls ", lower = 5, upper = 50,
#'              condition = expression(localsearch %in% c(1,2,3))),
#'    param_cat(name = "dlb",  label = "--dlb ", values = c(0,1),
#'              condition = "localsearch %in% c(1,2,3)"),
#'    forbidden = "(alpha == 0) & (beta == 0)")
#'
#' @export
parametersNew <- function(..., forbidden = NULL, debugLevel = 0L)
  ParameterSpace$new(..., forbidden = forbidden, verbose = debugLevel)

ParameterSpace <- R6::R6Class("ParameterSpace", cloneable = TRUE, lock_class = TRUE, lock_objects = TRUE, public = list(
  .params = NULL,
  conditions = NULL,
  depends = NULL,
  domains = NULL,
  forbidden = NULL,
  hierarchy = NULL,
  isFixed = NULL,
  names = NULL,
  names_fixed = NULL,
  names_variable = NULL,
  names_numeric = NULL,
  nbParameters = NULL,
  nbFixed = NULL,
  nbVariable = NULL,
  switches = NULL,
  types = NULL,
  initialize = function(..., forbidden = NULL, verbose = 0L) {
    .params <- list(...)
    names(.params) <- sapply(.params, "[[", "name")
    self$names <- names(.params)
    self$types <- sapply(.params, "[[", "type")
    self$switches <- sapply(.params, "[[", "label")
    # This has to be a list because each element is a vector.
    self$domains <- lapply(.params, "[[", "domain")
    self$conditions <- lapply(.params, "[[", "condition")
    self$isFixed <- sapply(.params, "[[", "isFixed")
    self$names_fixed <- self$names[self$isFixed]
    self$names_variable <- self$names[!self$isFixed]
    self$names_numeric <- self$names[self$types %in% c("r", "i")]
    self$nbParameters <- length(.params)
    self$nbFixed <- sum(self$isFixed)
    self$nbVariable <- sum(!self$isFixed)
    
    # Obtain the variables in each condition
    self$depends <- lapply(self$domains, all.vars)
    # Check that domain dependencies are OK.
    check_domain_dependencies(.params, self$depends, self$types)
    # Merge dependencies and conditions
    self$depends <- Map(union, self$depends, lapply(self$conditions, all.vars))
    # Sort parameters in 'conditions' in the proper order according to conditions
    hierarchyLevel <- sapply(names(.params), param_level, varsTree = self$depends)
    self$hierarchy <- hierarchyLevel
    self$conditions <- self$conditions[order(hierarchyLevel)]
    names(self$hierarchy) <- names(.params)
    stopifnot(length(self$conditions) == length(.params))

    if (!is.null(forbidden)) {
      if (is.language(forbidden))
        forbidden <- as.expression(forbidden)
      else if (is.character(forbidden))
        forbidden <- str2expression(forbidden)
      if (!is.expression(forbidden))
        stop ("Invalid forbidden expression.")
      check_forbidden_params(forbidden, names(.params))
      # FIXME: Instead of a list, we should generate a single expression that is
      # the logical-OR of all elements of the list.
      # First we would need to handle the "is.na(x) | !(x)" case here.
      # Maybe: sapply(forbiddenExps, function(x) substitute(is.na(x) | !(x), list(x=x)))
      # x <- parse(text=paste0("(", paste0(forbiddenExps,collapse=")||("), ")"))
      self$forbidden <- sapply(forbidden, compile_forbidden)
    }

    # Print the hierarchy vector:
    if (verbose >= 1L) {
      cat ("# --- Parameters Hierarchy ---\n")
      print(data.frame(Parameter = paste0(names(self$hierarchy)),
        Level = self$hierarchy,
        "Depends on" = sapply(names(self$hierarchy), function(x) paste0("", x, collapse=", ")),
        row.names=NULL))
      cat("# ------------------------\n")
    }
    self$.params <- .params
  },
  get = function(x) if (missing(x)) self$.params else self$.params[[x]],
  get_ordered = function() self$.params[names(self$conditions)],

  forbid_configurations = function(x) {
    # FIXME: This copies the whole list. Avoid the copy and just append.
    self$forbidden <- c(self$forbidden, 
      buildForbiddenExp(configurations = x[, self$names, drop=FALSE]))
  },
  as_character = function() {
    names_len <- max(nchar(names(self$.params)))
    label_len <- max(nchar(sapply(self$.params, "[[", "label"))) + 2L
    output <- ""
    digits <- c()
    for (p in self$.params) {
      type <- p[["type"]]
      domain <- p[["domain"]]
      if (is.expression(domain)) {
        domain <- sapply(domain, USE.NAMES=FALSE, function(x) {
          if (is.numeric(x)) {
            if (type == "r")
              x <- formatC(x, digits = p[["digits"]], format="f", drop0trailing=TRUE)
          } else {
            x <- paste0("\"", deparse(x, width.cutoff = 500L), "\"")
          }
          x
        })
      }
      domain <- paste0('(', paste0(domain, collapse=","), ')')
      condition <- p[["condition"]]
      condition <- if (isTRUE(condition)) "" else paste0(" | ", condition)
      transf <- p[["transform"]]
      if (!is.null(transf) && transf != "") type <- paste0(type, ",", transf)
      label <- paste0('"', p[["label"]], '"')
      output <- paste0(output, sprintf('%*s %*s %s %-15s%s\n', -names_len, p[["name"]], -label_len, label, type, domain, condition))
      if (type == "r")
        digits <- c(digits, p[["digits"]])
    }
    if (!is.null(self$forbidden)) {
      output <- paste0(output, "\n[forbidden]\n",
        paste0(collapse="\n", sapply(self$forbidden, attr, "source", USE.NAMES=FALSE)),
        "\n")
    }
    digits <- unique(digits)
    n <- length(digits)
    if (n > 1L)
      stop("Different digits per parameter is not supported yet")
    if (n == 1L)
      output <- paste0(output, "\n[global]\ndigits = ", digits[[1L]], "\n")
    output
  }),
)

#' @param name  Parameter name (must be alphanumeric).
#' @param values  (`character()`) \cr Domain as a vector of strings.
#' @param label Label associated to the parameter. Often used to encode a command-line switch that activates the parameter.
#' @param condition (`expression(1)|character(1)`) \cr Expression that defines when the parameter is active according to the value of other parameters.
#' @rdname parameters
#' @export
param_cat <- function(name = name, values, label = "", condition = TRUE)
  Parameter(name = name, type = "c", domain = values, label = label, condition = condition, transf = "")

#' @rdname parameters
#' @export
param_ord <- function(name, values, label = "", condition = TRUE)
  Parameter(name = name, type = "o", domain = values, label = label, condition = condition, transf = "")

#' @param lower,upper  Lower and upper limits of the valid domain.
#' @param transf (`character(1)`) \cr If `"log"`, then the parameter is sampled in a logarithmic scale.
#' @inheritParams readParameters
#' @rdname parameters
#' @export
param_real <- function(name, lower, upper, label = "", condition = TRUE, transf = "", digits = 15L)
  Parameter(name = name, type = "r", domain = c(lower, upper), label = label, condition = condition, transf = transf, digits = digits)

#' @param transf (`character(1)`) \cr If `"log"`, then the parameter is sampled in a logarithmic scale.
#' @rdname parameters
#' @export
param_int <- function(name, lower, upper, label = "", condition = TRUE, transf = "")
  Parameter(name = name, type = "i", domain = c(lower, upper), label = label, condition = condition, transf = transf)


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

sample_model <- function(param, n, model, domain)
  UseMethod("sample_model")

sample_unif <- function(param, n, domain)
  UseMethod("sample_unif")

param_quantile <- function(param, probs, domain)
  UseMethod("param_quantile")

#' @exportS3Method
sample_model.ParamCat <- function(param, n, model, domain = param[["domain"]])
  sample(domain, size = n, replace = TRUE, prob = model)

#' @exportS3Method
sample_unif.ParamCat <- function(param, n, domain = param[["domain"]])
  sample(domain, n, replace=TRUE)

#' @exportS3Method
param_quantile.ParamCat <- function(param, probs, domain = param[["domain"]])
{
  n <- length(domain)
  z <- integer_round(as.numeric(probs) * n + 1L, 1L, n)
  domain[z]
}

#' @exportS3Method
sample_model.ParamOrd <- function(param, n, model, domain = param[["domain"]])
  domain[floor(sample_numeric_norm(n, mean = model[[2L]], sd = model[[1L]],
    lower = 1L, upper = length(domain), transf = ""))]

#' @exportS3Method
sample_unif.ParamOrd <- function(param, n, domain = param[["domain"]])
  sample(domain, n, replace=TRUE)

#' @exportS3Method
param_quantile.ParamOrd <- param_quantile.ParamCat

#' @exportS3Method
sample_model.ParamInt <- function(param, n, model, domain = param[["domain"]])
{
  # Dependent domains could be not available because of inactivity of parameters
  # on which they are depedent. In this case, the dependent parameter becomes 
  # not active and we return NA.
  if (anyNA(domain))
    return(NA_integer_)
  lower <- domain[1L]
  upper <- domain[2L]
  transf <- param[["transform"]]
  mean <- model[[2L]]
  if (is.na(mean))
    # +1 for correct rounding before floor()
    value <- sample_numeric_unif(n, lower, 1L + upper, transf)
  else
    # + 0.5 because negative domains are log-transformed to positive domains.
    value <- sample_numeric_norm(n, mean + 0.5, sd = model[[1L]], lower = lower,
      # +1 for correct rounding before floor()
      upper = 1L + upper, transf = transf)
  # We use original upper, not the +1L for 'i'.
  integer_round(value, lower, upper)
}

#' @exportS3Method
sample_unif.ParamInt <- function(param, n, domain = param[["domain"]])
{
  # Dependent domains could be not available because of inactivity of parameters
  # on which they are depedent. In this case, the dependent parameter becomes 
  # not active and we return NA.
  if (anyNA(domain))
    return(NA_integer_)
  lower <- domain[1L]
  upper <- domain[2L]
  # +1 for correct rounding before floor()
  value <- sample_numeric_unif(n, lower, 1L + upper, transf = param[["transform"]])
  # We use original upperBound, not the +1L for 'i'.
  integer_round(value, lower, upper)
}

#' @exportS3Method
param_quantile.ParamInt <- function(param, probs, domain = param[["domain"]])
{
  lower <- domain[1L]
  upper <- domain[2L]
  probs <- as.numeric(probs)
  transf <- param[["transform"]]
  if (transf == "log") {
    # +1 for correct rounding before floor()
    probs <- transform_from_log(probs, transf, lower, upper + 1L)
  } else {
    probs <- probs * (upper + 1L - lower) + lower
  }
  integer_round(probs, lower, upper)
}
#' @exportS3Method
sample_model.ParamReal <- function(param, n, model, domain = param[["domain"]])
{
  # Dependent domains could be not available because of inactivity of parameters
  # on which they are depedent. In this case, the dependent parameter becomes 
  # not active and we return NA.
  if (anyNA(domain)) return(NA)
  lower <- domain[[1L]]
  upper <- domain[[2L]]
  transf <- param[["transform"]]
  mean <- model[[2L]]
  if (is.na(mean))
    x <- sample_numeric_unif(n, lower, upper, transf)
  else
    x <- sample_numeric_norm(n, mean, sd = model[[1L]], lower, upper, transf)
  x <- round(x, digits = param[["digits"]])
  irace.assert(all(x >= lower) && all(x <= upper))
  x
}

#' @exportS3Method
sample_unif.ParamReal <- function(param, n, domain = param[["domain"]])
{
  # Dependent domains could be not available because of inactivity of parameters
  # on which they are depedent. In this case, the dependent parameter becomes 
  # not active and we return NA.
  if (anyNA(domain)) return(NA)
  lower <- domain[[1L]]
  upper <- domain[[2L]]
  x <- sample_numeric_unif(n, lower, upper, transf = param[["transform"]])
  x <- round(x, digits = param[["digits"]])
  irace.assert(all(x >= lower) && all(x <= upper))
  x
}

#' @exportS3Method
param_quantile.ParamReal <- function(param, probs, domain = param[["domain"]])
{
  lower <- domain[1L]
  upper <- domain[2L]
  probs <- as.numeric(probs)
  digits <- param[["digits"]]
  transf <- param[["transform"]]
  if (transf == "log")
    return(round(transform_from_log(probs, transf, lower, upper), digits))
  probs <- probs * (upper - lower) + lower
  clamp(round(probs, digits), lower, upper)
}

#' Print parameter space in the textual format accepted by irace.
#' 
#' @param parameters (`list()`) \cr Data structure containing the parameter
#'   space definition. The data structure has to similar to the one returned by the
#'   function [`readParameters`].
#'
#'
#' @return `character()`
#' 
#' @seealso [readParameters()]
#' @examples
#' parameters_table <- '
#'  # name       switch           type  values               [conditions (using R syntax)]
#'  algorithm    "--"             c     (as,mmas,eas,ras,acs)
#'  localsearch  "--localsearch " c     (0, 1, 2, 3)
#'  alpha        "--alpha "       r     (0.00, 5.00)
#'  beta         "--beta "        r     (0.00, 10.00)
#'  rho          "--rho  "        r     (0.01, 1.00)
#'  ants         "--ants "        i,log (5, 100)
#'  q0           "--q0 "          r     (0.0, 1.0)           | algorithm == "acs"
#'  q0dep       "--q0 "           r     (0.0, q0)            | algorithm != "acs"
#'  rasrank      "--rasranks "    i     (1, "min(ants, 10)") | algorithm == "ras"
#'  elitistants  "--elitistants " i     (1, ants)            | algorithm == "eas"
#'  nnls         "--nnls "        i     (5, 50)              | localsearch %in% c(1,2,3)
#'  dlb          "--dlb "         c     (0, 1)               | localsearch %in% c(1,2,3)
#'  
#'  [forbidden]
#'  (alpha == 0.0) & (beta == 0.0)
#' '
#' parameters <- readParameters(text=parameters_table)
#' printParameters(parameters)
#' @export
printParameters <- function(parameters)
  cat(parameters$as_character())

#' @export
print.ParameterSpace <- function(x, digits = 15L, ...)
{
  str(x$.params, digits.d = digits)
  cat("Forbidden:\n")
  print(x$forbidden, digits = 15L)
}

## digits <- 4L
## x <- parametersNew(param_cat(name = "algorithm", values = c("as", "mmas", "eas", "ras", "acs"), label = "--"),
##                    param_ord(name = "localsearch", values = c("0", "1", "2", "3"), label = "--localsearch "),
##                    param_real(name = "alpha", lower = 0.0, upper=5.0, label = "--alpha ", digits = digits),
##                    param_real(name = "beta", lower = 0.0, upper = 10.0, label = "--beta ", digits = digits),
##                    param_real(name = "rho", lower = 0.01, upper = 1.00, label = "--rho ", digits = digits),
##                    param_int(name = "ants", lower = 5, upper = 100, transf = "log", label = "--ants "),
##                    param_real(name = "q0", label = "--q0 ", lower=0.0, upper=1.0, condition = expression(algorithm == "acs")),
##                    param_int(name = "rasrank", label = "--rasranks ", lower=1, upper=quote(min(ants, 10)), condition = 'algorithm == "ras"'),
##                    param_int(name = "elitistants", label = "--elitistants ", lower=1, upper=expression(ants), condition = 'algorithm == "eas"'),
##                    param_int(name = "nnls", label = "--nnls ", lower = 5, upper = 50, condition = expression(localsearch %in% c(1,2,3))),
##                    param_cat(name = "dlb",  label = "--dlb ", values = c(0,1), condition = "localsearch %in% c(1,2,3)"),
##                    param_cat(name = "fixed1",  label = "--fixed1 ", values = "1"),
##                    param_cat(name = "fixed2",  label = "--fixed2 ", values = "0"),
##                    forbidden = "(alpha == 0) & (beta == 0)")


## printParameters(x)
## print(x)

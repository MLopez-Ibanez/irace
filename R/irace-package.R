#' The irace package: \packageTitle{irace}
#'
#' \packageDescription{irace}
#'
#' @name irace-package
#' @import stats matrixStats withr data.table
#' @importFrom utils str
#' @importFrom R6 R6Class
#' @importFrom graphics abline axis boxplot par plot points strwidth bxp grid
#' @importFrom spacefillr generate_sobol_set
#'
#' @details  License: GPL (>= 2)
#'
#' @author Maintainers: Manuel López-Ibáñez and Leslie Pérez Cáceres
#'         \email{irace-package@googlegroups.com}
#'
#' @keywords package optimize tuning automatic configuration
#'
#' @references
#'   Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Leslie Pérez Cáceres,
#'   Thomas Stützle, and Mauro Birattari. The irace package: Iterated
#'   Racing for Automatic Algorithm Configuration. \emph{Operations Research
#'   Perspectives}, 2016. \doi{10.1016/j.orp.2016.09.002}
#'
#'   Manuel López-Ibáñez, Jérémie Dubois-Lacoste, Thomas Stützle, and Mauro
#'   Birattari. \emph{The irace package, Iterated Race for Automatic
#'   Algorithm Configuration}. Technical Report TR/IRIDIA/2011-004, IRIDIA,
#'   Université Libre de Bruxelles, Belgium, 2011.
#'
#'   Manuel López-Ibáñez and Thomas Stützle. The Automatic Design of
#'   Multi-Objective Ant Colony Optimization Algorithms. \emph{IEEE Transactions
#'   on Evolutionary Computation}, 2012.
#' @seealso
#'  [irace()] for examples and `vignette(package = "irace")` for the user-guide.
"_PACKAGE"


# This silences CRAN NOTE:
#   Namespace in Imports field not imported from: 'codetools'
#     All declared Imports should be used.
.ignore_unused_imports <- function() { # nocov start
  codetools::findGlobals
} # nocov end

## Functions ##########################################################
f_rosenbrock <- function (x) {
  d  <- length(x)
  z  <- x + 1
  hz <- z[1:(d - 1)]
  tz <- z[2:d]
  s  <- sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
  return(s)
}

f_rastrigin <- function (x) {
  sum(x * x - 10 * cos(2 * pi * x) + 10)
}

## target runner ###########################################################
target.runner <- function(experiment, scenario)
{
  debugLevel    <- scenario$debugLevel
  configuration.id  <- experiment$id.configuration
  instance.id   <- experiment$id.instance
  seed          <- experiment$seed
  configuration <- experiment$configuration
  instance      <- experiment$instance

  D <- 3
  par <- runif(D, min = -1, max = 1)
  fn <- function(x) {
    weight <- instance
    return(weight * f_rastrigin(x) + (1 - weight) * f_rosenbrock(x))
  }
  res <- optim(par,fn, method = "SANN", 
               control = list(maxit = 10,
                 tmax = as.numeric(configuration[["tmax"]]), 
                 temp = as.numeric(configuration[["temp"]])))
  result <- list(cost = res$value, call = toString(experiment))
  return(result)
}

set.seed(2)
weights <- rnorm(200, mean = 0.9, sd = 0.02)

## Run function ########################################################
sann.irace <- function(...)
{
  args <- list(...)
  require("irace")

  parameters.table <- '
   tmax "" i (1, 5000)
   temp "" r (0, 100)
   '  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(targetRunner = target.runner,
                   maxExperiments = 1000, seed = 1234567)
  scenario <- c(scenario, args)

  scenario <- checkScenario (scenario)

  irace(scenario = scenario, parameters = parameters)
}

sann.irace(instances = weights)

sann.irace(deterministic = TRUE, instances = weights[1:7])

## FIXME: This needs to be tested on Windows.
## The following code can be used to test this function.
test.path.rel2abs <- function()
{
  # Try to set wd; otherwise fail silently.
  cwd <- getwd()
  if (is.null(cwd)) return(TRUE)
  on.exit(setwd(cwd))
  try (setwd("/tmp"))
  if (getwd() != "/tmp") return(TRUE)
  
  testcases <- read.table(text='
"."                         "/tmp"  "/tmp"
".."                        "/tmp"  "/"
"../"                       "/tmp"  "/"
"../."                      "/tmp"  "/"
"../.."                     "/tmp"  "/"
"../../"                    "/tmp"  "/"
"../../x.r"                 "/tmp"  "/x.r"
"../leslie/"                "/tmp"  "/leslie"
"../leslie/x.r"             "/tmp"  "/leslie/x.r"
"../x.r"                    "/tmp"  "/x.r"
"..irace"                   "/tmp"  "/tmp/..irace"
"./"                        "/tmp"  "/tmp"
"./."                       "/tmp"  "/tmp"
"./"                        "/tmp/"  "/tmp"
"./."                       "/tmp/"  "/tmp"
"././x.r"                   "/tmp"  "/tmp/x.r"
"./irace/../x.r"            "/tmp"  "/tmp/x.r"
"./x.r"                     "/tmp"  "/tmp/x.r"
".x.R"                      "/tmp"  "/tmp/.x.R"
"/./x.r"                    "/tmp"  "/x.r"
"/home"                     "/tmp"  "/home"
"/home/leslie/././x.r"      "/tmp"  "/home/leslie/x.r"
"/home/leslie/~/x.r"        "/tmp"  "/home/leslie/~/x.r"
"/~/x.r"                    "/tmp"  "/~/x.r"
"e:/home/leslie/x.r"        "/tmp"  "e:/home/leslie/x.r"
"leslie/leslie/../../irace" "/tmp"  "/tmp/irace"
"x.r"                       "/tmp"  "/tmp/x.r"
"~/irace/../x.r"            "/tmp"  "~/x.r"
"~/x.r"                     "/tmp"  "~/x.r"
"../../../data"             "./"    "/data"
"../../../data"             "/tmp/a/b/c/" "/tmp/data"
"..//a"                     ".//"   "/a"
', stringsAsFactors=FALSE)
  for(i in 1:nrow(testcases)) {
    orig <- testcases[i,1]
    cwd <-  testcases[i,2]
    res <- irace:::path.rel2abs(testcases[i,1], cwd)
    exp <- path.expand(testcases[i,3])
    if (res == exp) {
      cat("[OK] (", orig, ", ", cwd, ") -> ", res, "\n", sep="")
    } else {
      stop("[FAILED] (", orig, ", ", cwd, ") -> ", res, " but expected: ", exp, "\n")
    }
  }
}
test.path.rel2abs()

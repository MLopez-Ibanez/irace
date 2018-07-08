library(irace)

# Reproducible results
seed <- sample(2^30, 1)
cat("Seed: ", seed, "\n")
set.seed(seed)

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
  res <- optim(par, fn, method = "SANN", 
               control = list(maxit = 10,
                 tmax = as.numeric(configuration[["tmax"]]), 
                 temp = as.numeric(configuration[["temp"]])))
  result <- list(cost = res$value, call = toString(experiment))
  return(result)
}

## target runner ###########################################################
target.runner.reject <- function(experiment, scenario)
{
  if (runif(1) <= 0.05) return (list(cost = -Inf, call = toString(experiment)))
  return (target.runner(experiment, scenario))
}


weights <- rnorm(200, mean = 0.9, sd = 0.02)

## Run function ########################################################
sann.irace <- function(...)
{
  args <- list(...)

  parameters.table <- '
   tmax "" i (1, 5000)
   temp "" r (0, 100)
   '  
  parameters <- readParameters(text = parameters.table)

  scenario <- list(targetRunner = target.runner,
                   maxExperiments = 1000, seed = 1234567)
  scenario <- modifyList(scenario, args)

  scenario <- checkScenario (scenario)

  confs <- irace(scenario = scenario, parameters = parameters)
  best.conf <- getFinalElites(logFile = scenario$logFile, n = 1,
                              drop.metadata = TRUE)
  stopifnot(identical(removeConfigurationsMetaData(confs[1, , drop = FALSE]),
                      best.conf))
}

sann.irace(instances = weights, parallel = 2, targetRunner = target.runner.reject)

sann.irace(instances = weights, parallel = 2)

sann.irace(deterministic = TRUE, instances = weights[1:7])


test.path.rel2abs <- function()
{
  # Try to set wd; otherwise fail silently.
  old.cwd <- getwd()
  if (is.null(old.cwd)) return(TRUE)
  on.exit(setwd(old.cwd), add = TRUE)
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
    exp <- gsub("\\", "/", path.expand(testcases[i,3]), fixed = TRUE)
    if (res == exp) {
      cat("[OK] (", orig, ", ", cwd, ") -> ", res, "\n", sep="")
    } else {
      stop("[FAILED] (", orig, ", ", cwd, ") -> ", res, " but expected: ", exp, "\n")
    }
  }
}
test.path.rel2abs()

testwindows.path.rel2abs <- function()
{
  testcases <- read.table(text='
.                         N:\\\\tmp  N:/tmp
..                        N:\\\\tmp  N:/
..\\\\                       N:\\\\tmp  N:/
..\\\\.                      N:\\\\tmp  N:/
..\\\\..                     N:\\\\tmp  N:/
..\\\\..\\\\                    N:\\\\tmp  N:/
..\\\\..\\\\x.r                 N:\\\\tmp  N:/x.r
..\\\\leslie\\\\                N:\\\\tmp  N:/leslie
..\\\\leslie\\\\x.r             N:\\\\tmp  N:/leslie/x.r
..\\\\x.r                    N:\\\\tmp  N:/x.r
..irace                   N:\\\\tmp  N:/tmp/..irace
.\\\\                        N:\\\\tmp  N:/tmp
.\\\\.                       N:\\\\tmp  N:/tmp
.\\\\                        N:\\\\tmp\\\\ N:/tmp
.\\\\.                       N:\\\\tmp\\\\  N:/tmp
.\\\\.\\\\x.r                   N:\\\\tmp  N:/tmp/x.r
.\\\\irace\\\\..\\\\x.r            N:\\\\tmp  N:/tmp/x.r
.\\\\x.r                     N:\\\\tmp  N:/tmp/x.r
.x.R                      N:\\\\tmp  N:/tmp/.x.R
.                         N:\\tmp  N:/tmp
..                        N:\\tmp  N:/
..\\                       N:\\tmp  N:/
..\\.                      N:\\tmp  N:/
..\\..                     N:\\tmp  N:/
..\\..\\                    N:\\tmp  N:/
..\\..\\x.r                 N:\\tmp  N:/x.r
..\\leslie\\                N:\\tmp  N:/leslie
..\\leslie\\x.r             N:\\tmp  N:/leslie/x.r
..\\x.r                    N:\\tmp  N:/x.r
..irace                   N:\\tmp  N:/tmp/..irace
.\\                        N:\\tmp  N:/tmp
.\\.                       N:\\tmp  N:/tmp
.\\                        N:\\tmp\\ N:/tmp
.\\.                       N:\\tmp\\  N:/tmp
.\\.\\x.r                   N:\\tmp  N:/tmp/x.r
.\\irace\\..\\x.r            N:\\tmp  N:/tmp/x.r
.\\x.r                     N:\\tmp  N:/tmp/x.r
.x.R                      N:\\tmp  N:/tmp/.x.R
.                         N:  N:/
..                        N:  N:/
..\\\\                       N:  N:/
..\\\\.                      N:  N:/
..\\\\..                     N:  N:/
..\\\\..\\\\                    N:  N:/
..\\\\..\\\\x.r                 N:  N:/x.r
..\\\\leslie\\\\                N:  N:/leslie
..\\\\leslie\\\\x.r             N:  N:/leslie/x.r
..\\\\x.r                    N:  N:/x.r
..\\                       N:  N:/
..\\.                      N:  N:/
..\\..                     N:  N:/
..\\..\\                    N:  N:/
..\\..\\x.r                 N:  N:/x.r
..\\leslie\\                N:  N:/leslie
..\\leslie\\x.r             N:  N:/leslie/x.r
..\\x.r                    N:  N:/x.r
..irace                   N:  N:/..irace
.\\\\                        N:  N:/
.\\\\.                       N:  N:/
.\\\\                        N:\\\\ N:/
.\\\\.                       N:\\\\  N:/
.\\\\.\\\\x.r                   N:  N:/x.r
.\\\\irace\\\\..\\\\x.r            N:  N:/x.r
.\\\\x.r                     N:  N:/x.r
.\\                        N:  N:/
.\\.                       N:  N:/
.\\                        N:\\ N:/
.\\.                       N:\\  N:/
.\\.\\x.r                   N:  N:/x.r
.\\irace\\..\\x.r            N:  N:/x.r
.\\x.r                     N:  N:/x.r
.x.R                      N:  N:/.x.R
.                         N:/tmp  N:/tmp
..                        N:/tmp  N:/
../                       N:/tmp  N:/
../.                      N:/tmp  N:/
../..                     N:/tmp  N:/
../../                    N:/tmp  N:/
../../x.r                 N:/tmp  N:/x.r
../leslie/                N:/tmp  N:/leslie
../leslie/x.r             N:/tmp  N:/leslie/x.r
../x.r                    N:/tmp  N:/x.r
..irace                   N:/tmp  N:/tmp/..irace
./                        N:/tmp  N:/tmp
./.                       N:/tmp  N:/tmp
./                        N:/tmp/ N:/tmp
./.                       N:/tmp/  N:/tmp
././x.r                   N:/tmp  N:/tmp/x.r
./irace/../x.r            N:/tmp  N:/tmp/x.r
./x.r                     N:/tmp  N:/tmp/x.r
.x.R                      N:/tmp  N:/tmp/.x.R
D:/./x.r                  N:/tmp  D:/x.r
D:\\\\.\\\\x.r                  N:/tmp  D:/x.r
D:\\.\\x.r                  N:/tmp  D:/x.r
D:                        N:/tmp  D:/
D:\\\\                       N:/tmp  D:/
D:/                       N:/tmp  D:/
D:/leslie/././x.r         N:/tmp  D:/leslie/x.r
D:/leslie/~/x.r        N:/tmp  D:/leslie/~/x.r
e:/home/leslie/x.r        /tmp  e:/home/leslie/x.r
leslie/leslie/../../irace N:/tmp  N:/tmp/irace
x.r                       N:/tmp  N:/tmp/x.r
~/irace/../x.r            N:/tmp  ~/x.r
~/x.r                     N:/tmp  ~/x.r
', stringsAsFactors=FALSE)
  for(i in 1:nrow(testcases)) {
    orig <- testcases[i,1]
    cwd <-  testcases[i,2]
    res <- irace:::path.rel2abs(testcases[i,1], cwd)
    exp <- gsub("\\", "/", path.expand(testcases[i,3]), fixed = TRUE)
    if (res == exp) {
      cat("[OK] ", i, ": path.rel2abs(\"", orig, "\", \"", cwd, "\") -> ", res, "\n", sep="")
    } else {
      stop("[FAILED] ", i, ": path.rel2abs(\"", orig, "\", \"", cwd, "\") -> ", res, " but expected: ", exp, "\n")
    }
  }
}
testwindows.path.rel2abs()

test.checkForbidden <- function(param.file)
{
  params <- irace:::readParameters(param.file)
  confs <- irace:::readConfigurationsFile("configurations.txt", params)
  forbidden <- irace:::readForbiddenFile("forbidden.txt")
  exp.confs <- irace:::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        6     "x1"   3.5   "low"
NA        NA   "x3"   4.5   "low"
', parameters = params)
  confs <- irace:::checkForbidden(confs, forbidden)
  rownames(confs) <- rownames(exp.confs) <- NULL
  stopifnot(identical(confs, exp.confs))
}
test.checkForbidden("parameters.txt")
test.checkForbidden("logparameters.txt")


test.instances <- function()
{
  # Test that a function can be given as a string.
  scenario <- list(targetRunner = "target.runner",
                   maxExperiments = 1000, seed = 1234567,
                   firstTest = 5,
                   deterministic = TRUE,
                   trainInstancesFile = "train-instances.txt")
  scenario <- checkScenario (scenario)
}
test.instances()


test.similarConfigurations <- function()
{
  parameters <- irace:::readParameters(text = '
n1 "" r (0,1)
n2 "" r (0,1)
n3 "" r (0,1)
c1 "" c ("a","b")
c2 "" c ("a","b")
c3 "" c ("a","b")
')
  confs <- read.table(header = TRUE, stringsAsFactors = FALSE, text = '
.ID. n1 n2 n3 c1 c2 c3
1    0.5  0.5  0.5      "a" "a" "a"
2    NA  0.5  0.5       "a" "a" "a"
3    0.5  0.5  0.5      "a" "a" "b"
4    0.5  0.501  0.5    "a" "a" "a"
5    0.5  0.5    0.499  "a" "a" "a"
6    0.5  0.5  0.5      "a" "a" "a"
7    0.5  0.5  0.5      "a"  NA "a"
8    0.5  0.1  0.5      "a" "a" "a"
9    0.5  0.49  0.5     "a" "a" "a"
10   0.5  0.5  0.5      "a" "a"  NA
11   0.5  0.5  0.5      "a" "a"  NA
12    NA  0.5  0.5      "a" "a" "a"
')
  stopifnot(identical(
    irace:::similarConfigurations(confs[1:10,], parameters, threshold = 0.001),
    as.integer(c(1,6))))
  stopifnot(identical(
    irace:::similarConfigurations(confs[1:10,], parameters, threshold = 0.01),
    as.integer(c(1,4,5,6))))
  stopifnot(identical(
    irace:::similarConfigurations(confs[1:10,], parameters, threshold = 0.1),
    as.integer(c(1,4,5,6,9))))

  stopifnot(identical(
    irace:::similarConfigurations(confs, parameters, threshold = 0.001),
    # FIXME: The output should be already sorted
    as.integer(c(1,2,6,12,10,11))))
}


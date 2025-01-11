library(irace)
library(fs)
library(withr)

###### Create sann.rda
cat('**** Creating sann.rda\n')
## We generate 50 instances (in this case, weights):
weights <- rnorm(50, mean = 0.9, sd = 0.02)

## On this set of instances, we are interested in optimizing two
## parameters of the SANN algorithm: tmax and temp. We setup the
## parameter space as follows:
parameters_table <- '
tmax "" i (1, 5000)
temp "" r (0, 100)
'

## We use the irace function readParameters to read this table:
parameters <- readParameters(text = parameters_table)

## Next, we define the function that will evaluate each candidate
## configuration on a single instance. For simplicity, we restrict to
## three-dimensional functions and we set the maximum number of
## iterations of SANN to 5000.
target_runner <- function(experiment, scenario)
{
  instance <- experiment$instance
  configuration <- experiment$configuration
  D <- 3L
  par <- runif(D, min=-1, max=1)
  fn <- function(x) {
    # Functions to be optimized:
    f_rosenbrock <- function (x) {
      z <- x + 1
      hz <- z[-length(z)]
      tz <- z[-1L]
      sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
    }
    f_rastrigin <- function (x)
      sum(x * x - 10 * cos(2 * pi * x) + 10)

    (instance * f_rastrigin(x) + (1 - instance) * f_rosenbrock(x))
  }
  res <- stats::optim(par,fn, method="SANN",
               control=list(maxit=5000
                 , tmax = as.numeric(configuration[["tmax"]])
                 , temp = as.numeric(configuration[["temp"]])
                 ))
  ## New output interface in irace 2.0. This list may also contain:
  ## - 'time' if irace is called with 'maxTime'
  ## - 'error' is a string used to report an error
  ## - 'outputRaw' is a string used to report the raw output of calls to
  ##   an external program or function.
  ## - 'call' is a string used to report how target.runner called the
  ##   external program or function.
  list(cost = res$value)
}

## We define a configuration scenario by setting targetRunner to the
## function define above, instances to the first 50 random weights, and
## a maximum budget of 1000 calls to targetRunner.
scenario <- list(targetRunner = target_runner,
                 instances = weights,
                 maxExperiments = 1000,
                 logFile = "./sann.rda",
                 execDir = "./",
                 parameters = parameters)

## We are now ready to launch irace. We do it by means of the irace
## function. The function will print information about its
## progress. This may require a few minutes, so it is not run by default.
irace(scenario = scenario)

iraceResults <- read_logfile("sann.rda")
iraceResults$scenario$execDir <- "./"
iraceResults$scenario$logFile <- "./sann.rda"
save(iraceResults, file="sann.rda", version = 3L)

### ACOTSP example

download_uncompress <- function(url, exdir, flat = FALSE) {
  exts <- c(".tar.bz2", ".tar.gz", ".tgz", ".tar.xz", ".zip")
  fileext <- exts[endsWith(url, exts)]
  tf <- local_tempfile(fileext = fileext)
  download.file(url, destfile = tf)
  if (fileext == ".zip") {
    unzip(tf, exdir = exdir, junkpaths = flat)
  } else {
    untar(tf, exdir = exdir, restore_times = FALSE, verbose = TRUE)
    if (flat) {
      files <- untar(tf, list = TRUE)
      withr::with_dir(exdir, {
        for (p in files) {
          if (fs::is_file(p)) {
            fs::file_move(p, ".")
          }
        }
        paths <- fs::dir_ls(".", type = "directory")
        for (p in paths) {
          n <- length(fs::dir_ls(p, recurse = TRUE, type = "file"))
          if (n == 0L) fs::dir_delete(p)
        }
      })
    }
  }
}

invoke_make <- function(args) {
  system2("make", args = args)
}

# Problem instances
setup_tsp_rue_2000 <- function() {
  exdir <- fs::path_abs("acotsp")
  if (!fs::file_exists(exdir)) {
    download_uncompress("https://iridia.ulb.ac.be/supp/IridiaSupp2016-003/scenarios/acotsp/instances.tar.gz",
      exdir = exdir)
  }
}

setup_acotsp <- function() {
  exdir <- fs::path_abs("acotsp")
  download_uncompress("https://github.com/MLopez-Ibanez/ACOTSPQAP/archive/refs/heads/master.zip", exdir = exdir, flat= TRUE)
  invoke_make(c("-C", exdir, "acotsp"))
}

setup_tsp_rue_2000()
setup_acotsp()

iracebin <- system.file(package="irace", "bin/irace")
if (0 != file.access(iracebin, mode=1))
  stop("Error: ", iracebin, " is not executable or not found!")
system(paste0("nice -n 19 ", iracebin, " --parallel 2 | tee irace-acotsp-stdout.txt 2>&1"))

# Create log-ablation.Rdata
cat('**** Running ablation("irace-acotsp.Rdata")\n')
ablation("irace-acotsp.Rdata", parallel = 2L)

iraceResults <- read_logfile("irace-acotsp.Rdata")

# Change paths
# FIXME: Use irace::scenario_update_paths()
to_change <- c("logFile", "trainInstancesDir", "trainInstancesFile",
               "testInstancesDir", "testInstancesFile", "parameterFile",
               "targetRunner")
iraceResults$scenario[to_change] <-
  lapply(iraceResults$scenario[to_change],
         function(x) paste0("./", basename(x)))

iraceResults$scenario$execDir <- "./"
iraceResults$scenario$instances <-
  paste0(iraceResults$scenario$trainInstancesDir, "/",
         basename(iraceResults$scenario$instances))
iraceResults$scenario[["testInstances"]] <-
  paste0(iraceResults$scenario$testInstancesDir, "/",
         basename(iraceResults$scenario$testInstances))

save(iraceResults, file = "irace-acotsp.Rdata", version = 3L)

###############################
### Small example file
###############################
# Experiment
res <- get_instanceID_seed_pairs(iraceResults, index = 1L, instances = TRUE)
stopifnot(length(res[["instanceID"]]) == 1L)
stopifnot(length(res[["instance"]]) == 1L)
experiment <- list (
  id_configuration = 1,
  id_instance      = res[["instanceID"]],
  seed             = res[["seed"]],
  configuration    = getConfigurationById(iraceResults, 1L, drop.metadata = TRUE),
  instance         = res[["instance"]],
  switches         = iraceResults$scenario$parameters$switches)

# Output
# FIXME: Create this just using functions from the irace package.
output <- list(
  list(cost=iraceResults$experiments[1L,1L], time=as.numeric(iraceResults$experimentLog[1L,"time"])),
  list(cost=iraceResults$experiments[1L,2L], time=as.numeric(iraceResults$experimentLog[2L,"time"])))

# save in the folder
save(experiment, output, file="examples.Rdata", version = 3L)

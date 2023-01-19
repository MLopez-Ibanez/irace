context("irace")

withr::with_output_sink("test-repair.Rout", {

repair_irace <- function(targetRunner, repair)
{
  weights <- rnorm(200, mean = 0.9, sd = 0.02)
  parameters <- readParameters(text = '
   p1 "" r (0,1)
   p2 "" r (0,1)
   p3 "" r (0,1)
   dummy "" c ("d1", "d2")
   ')
  scenario <- list(targetRunner = targetRunner,
                   repairConfiguration = repair,
                   instances = weights,
                   maxExperiments=180,
                   seed = 1234567)
  scenario <- checkScenario (scenario)
  irace:::checkTargetFiles(scenario = scenario, parameters = parameters)
  confs <- irace(scenario = scenario, parameters = parameters)
  final_ids <- as.character(sort(confs$.ID.[1:scenario$testNbElites]))
  expect_gt(nrow(confs), 0L)
}
target_sum2one <- function(experiment, scenario)
{
  configuration <- experiment$configuration
  p1 <-  configuration[["p1"]]
  p2 <-  configuration[["p2"]]
  p3 <-  configuration[["p3"]]
  stopifnot(isTRUE(all.equal(p1+p2+p3, 1.0)))
  list(cost = -p1, call = toString(experiment))
}

repair_sum2one <- function(configuration, parameters)
{
  isreal <- names(which(parameters$types[colnames(configuration)] == "r"))
  digits <- parameters$digits[isreal]
  c_real <- unlist(configuration[isreal])
  c_real <- c_real / sum(c_real)
  c_real[-1] <- round(c_real[-1], digits[-1])
  c_real[1] <- 1 - sum(c_real[-1])
  configuration[isreal] <- c_real
  return(configuration)
}

target_order <- function(experiment, scenario)
{
  configuration <- experiment$configuration
  p1 <-  configuration[["p1"]]
  p2 <-  configuration[["p2"]]
  p3 <-  configuration[["p3"]]
  stopifnot(p1 <= p2 && p2 <= p3)
  list(cost = -p1, call = toString(experiment))
}

repair_order <- function(configuration, parameters)
{
 columns <- c("p1","p2","p3")
 #cat("Before"); print(configuration)
 configuration[columns] <- sort(unlist(configuration[columns], use.names=FALSE))
 #cat("After"); print(configuration)
 return(configuration)
}

test_that("repair: sum to one", {
  generate.set.seed()
  repair_irace(target_sum2one, repair_sum2one)
})

test_that("repair: increasing order", {
  generate.set.seed()
  repair_irace(target_order, repair_order)
})

}) # withr::with_output_sink()

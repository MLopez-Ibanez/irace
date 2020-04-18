context("bugs")

withr::with_output_sink("test-bugs.Rout", {

run_irace_from_rds <- function(rds_filename)
{
  # FIXME: For some unknown reason, this test doesn't work in R 3.6
  skip_if(grepl("^3.6", getRversion()))
  skip_if(grepl("^4", getRversion()))

  scenario <- readRDS(rds_filename)$scenario
  parameters <- readRDS(rds_filename)$parameters

  scenario$targetRunner <- function(experiment, scenario, filename = rds_filename) {
    saved <- readRDS(filename)
    row <- which(saved$instancesList[, "instance"] == experiment[["id.instance"]]
                 & saved$instancesList[, "seed"] == experiment[["seed"]])

    cost <- saved$experiments[row, experiment[["id.configuration"]] ]
    if (length(cost) == 0 || is.na(cost)) {
      print(row)
      print(experiment)
    }
    expect_false(is.na(cost))
    return(list(cost = cost))
  }
  confs <- irace(scenario = scenario, parameters = parameters)
  expect_gt(nrow(confs), 0L)
}

test_that("bug_large_new_instances", {
  skip_on_cran()
  # FIXME: For some unknown reason, this test doesn't work in R 3.6
  skip_if(grepl("^3.6", getRversion()))
  skip_if(grepl("^4", getRversion()))
  # FIXME: Convert this test to use run_irace_from_rds()
  load("bug_large_new_instances.Rdata", verbose = TRUE)

  scenario$targetRunner <- function(experiment, scenario) {
    saved_instances_list <- dynGet("saved_instances_list", inherits = TRUE)
    saved_experiments <- dynGet("saved_experiments", inherits = TRUE)
    row <- which(saved_instances_list[, "instance"] == experiment[["id.instance"]]
                 & saved_instances_list[, "seed"] == experiment[["seed"]])
    cost <- saved_experiments[row, experiment[["id.configuration"]] ]
    if (length(cost) == 0 || is.na(cost)) {
      print(row)
      print(experiment)
    }
    expect_false(is.na(cost))
    return(list(cost = cost))
  }
  confs <- irace(scenario = scenario, parameters = parameters)
  expect_gt(nrow(confs), 0L)
})

test_that("target.runner as string", {

  target.runner.local <- function(experiment, scenario) return(list(cost=1L))

  expect_true(irace:::is.function.name("target.runner.local"))

  # Test that a function can be given as a string.
  scenario <- list(targetRunner = "target.runner.local",
                   instances = 1:10, maxExperiments = 1000)
  scenario <- checkScenario (scenario)

  expect_equal(scenario$targetRunner, target.runner.local)
  expect_is(scenario$targetRunner, "function")
})

target.runner.global <- function(experiment, scenario) return(list(cost=1L))

test_that("target.runner as string (global)", {

  expect_true(irace:::is.function.name("target.runner.global"))

  # Test that a function can be given as a string.
  scenario <- list(targetRunner = "target.runner.global",
                   instances = 1:10, maxExperiments = 1000)
  scenario <- checkScenario (scenario)

  expect_equal(scenario$targetRunner, target.runner.global)
  expect_is(scenario$targetRunner, "function")
})


test_that("ordered assert", {
  library(irace)
  parameters <- readParameters(text='
x "" o (a,b,c,d)
')
  confs <- irace:::sampleUniform(parameters, 1, 0)
  confs$.ID. <- 1
  model <- irace:::initialiseModel(parameters, confs, 0)
  confs <- irace:::sampleModel(parameters, confs, model, 1, 0)
  expect_true(confs$x %in% parameters$domain$x)
})

test_that("maxim_bug", {
  skip_on_cran()
  run_irace_from_rds("saved_maxim_bug.rds")
})

test_that("maxim_bug2", {
  skip_on_cran()
  run_irace_from_rds("saved_maxim_bug2.rds")
})

test_that("non-normal", {
  skip_on_cran()
  df <- as.matrix(read.table(text='
0.007858276 0.007934570 0.007949829
0.009384155 0.009521484 0.009719849
0.011520386 0.012100220 0.012176514
0.214721680 0.219146729 0.219512939
0.005462646 0.005462646 0.005523682
0.005035400 0.005035400 0.005035400
0.011001587 0.011367798 0.011962891
0.153244019 0.155212402 1.157546997
0.008636475 0.008789062 0.008666992
0.008239746 0.008499146 0.008560181
0.010848999 0.011077881 0.011703491
0.191589355 0.191879272 0.191589355
0.011245728 0.011245728 0.011245728
0.006454468 0.006454468 0.006561279
0.010330200 0.010894775 0.011032104
0.160491943 0.164169312 0.167068481', header=FALSE))
  ## colnames(df) <- 1:3
  ## #df <- log(df)
  ## library(ggplot2)
  ## library(tidyr)
  ## df_long <- as.data.frame(df)
  ## df_long$run <- 1:16

  ## df_long <- df_long %>% gather("config", "y", as.character(1:3))
  ## ggplot(df_long, aes(df=y)) +
  ##   geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=0.01)+
  ##   geom_density(alpha=.2, fill="#FF6666") +
  ##   facet_grid(config ~ .)

  ## ggplot(df_long, aes(x=config, y=y)) + geom_boxplot() + geom_jitter()
  ## aggregate(y ~ config, data = df_long, FUN = function(x) c(mean = mean(x), sd = sd(x), median=median(x),
  ##                                                           shapiro.test = shapiro.test(x)$p.value) )

  ## t.test(x[,1], x[,2],paired=TRUE)
  ## t.test(x[,1], x[,3],paired=TRUE)

  ## wilcox.test(x[,1], x[,2],paired=TRUE)
  ## wilcox.test(x[,1], x[,3],paired=TRUE)

  ## print(all(x[,1] <= x[,2]))
  ## print(all(x[,1] <= x[,3]))
  parameters <- readParameters(text='p "" c (1,2,3,4,5)')
  scenario <- list(instances = 1:16, maxExperiments = 50,
                   testType="t-test")
  scenario$targetRunner <- function(experiment, scenario, this_df = df) {
    if (experiment[["id.configuration"]] > 3)
      cost <- as.numeric(experiment[["id.configuration"]])
    else
      cost <- this_df[as.numeric(experiment[["id.instance"]]),
                      as.numeric(experiment[["id.configuration"]]) ]
    return(list(cost=cost))
  }
  confs <- irace(scenario = scenario, parameters = parameters)
  print(confs)
  expect_equal(confs$.ID., 1L)
})

}) # withr::with_output_sink()

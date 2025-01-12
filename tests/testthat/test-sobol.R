withr::with_output_sink("test-sobol.Rout", {

  test_that("bug with conditional dependent", {
    parameters  <- parametersNew(param_cat(name = "algorithm", values = c("as", "mmas", "ras", "acs")),
      param_real(name = "alpha", lower = 0.0, upper=5.0),
      param_real(name = "beta", lower = 0.0, upper = 10.0),
      param_int(name = "ants", lower = 2, upper = 100),
      param_real(name = "q0", lower=0.0, upper=1.0, condition = expression(algorithm == "acs")),
      param_int(name = "rasrank", lower=1, upper=quote(min(ants, 10)), condition = 'algorithm == "ras"'),
      param_int(name = "eants", lower=0, upper=expression(rasrank)),
      param_cat(name = "dlb",  values = c(0,1), condition = "localsearch == 1"),
      param_int(name = "nnls", lower = 5, upper = 50, condition = expression(dlb == 1)),
      param_ord(name = "localsearch", values = c("0", "1")),
      param_cat(name = "fixed",  values = "0"))

    confs <- irace:::sampleSobol(parameters, 1000L)
    expect_equal(nrow(confs), 1000L)
    expect_valid_configurations(confs, parameters)
  })

  test_that("bug with dependent fixed", {
    params <- readParameters(text='
ROOT		"ROOT="		c		("SimpleAlgorithm")
ROOT_SimpleAlgorithm.constructive		"ROOT_SimpleAlgorithm.constructive="		c		("FasterInvertedConstructive", "SlowConstructive")		| ROOT %in% c("SimpleAlgorithm")
ROOT_SimpleAlgorithm.constructive_FasterInvertedConstructive.sumThis		"ROOT_SimpleAlgorithm.constructive_FasterInvertedConstructive.sumThis="		r		(-50.0, 50.0)		| ROOT_SimpleAlgorithm.constructive %in% c("FasterInvertedConstructive")
ROOT_SimpleAlgorithm.constructive_SlowConstructive.sumThis		"ROOT_SimpleAlgorithm.constructive_SlowConstructive.sumThis="		i		(-10, 10)		| ROOT_SimpleAlgorithm.constructive %in% c("SlowConstructive")
ROOT_SimpleAlgorithm.improver		"ROOT_SimpleAlgorithm.improver="		c		("FlippyFlopImprover")		| ROOT %in% c("SimpleAlgorithm")
ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.enabled		"ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.enabled="		c		("true", "false")		| ROOT_SimpleAlgorithm.improver %in% c("FlippyFlopImprover")
ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.sleepy		"ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.sleepy="		c		("8", "6", "12", "11", "7", "5", "4", "10", "1", "9", "2", "3", "13")		| ROOT_SimpleAlgorithm.improver %in% c("FlippyFlopImprover")
')
    confs <- irace:::sampleSobol(params, 10L)
    expect_equal(nrow(confs), 10L)
    expect_valid_configurations(confs, params)
  })
  test_that("bug with dependent fixed #2", {
    params <- readParameters(text='
ROOT		"ROOT="		c		("SimpleAlgorithm", "ComplexAlgorithms")
ROOT_SimpleAlgorithm.constructive		"ROOT_SimpleAlgorithm.constructive="		c		("FasterInvertedConstructive", "SlowConstructive")		| ROOT %in% c("SimpleAlgorithm")
ROOT_SimpleAlgorithm.constructive_FasterInvertedConstructive.sumThis		"ROOT_SimpleAlgorithm.constructive_FasterInvertedConstructive.sumThis="		r		(-50.0, 50.0)		| ROOT_SimpleAlgorithm.constructive %in% c("FasterInvertedConstructive")
ROOT_SimpleAlgorithm.constructive_SlowConstructive.sumThis		"ROOT_SimpleAlgorithm.constructive_SlowConstructive.sumThis="		i		(-10, 10)		| ROOT_SimpleAlgorithm.constructive %in% c("SlowConstructive")
ROOT_SimpleAlgorithm.improver		"ROOT_SimpleAlgorithm.improver="		c		("FlippyFlopImprover")		| ROOT %in% c("SimpleAlgorithm")
ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.enabled		"ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.enabled="		c		("true", "false")		| ROOT_SimpleAlgorithm.improver %in% c("FlippyFlopImprover")
ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.sleepy		"ROOT_SimpleAlgorithm.improver_FlippyFlopImprover.sleepy="		c		("8", "6", "12", "11", "7", "5", "4", "10", "1", "9", "2", "3", "13")		| ROOT_SimpleAlgorithm.improver %in% c("FlippyFlopImprover")
')
    confs <- irace:::sampleSobol(params, 10)
    expect_equal(nrow(confs), 10)
    expect_valid_configurations(confs, params)
  })

})

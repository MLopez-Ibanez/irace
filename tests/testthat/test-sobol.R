withr::with_output_sink("test-sobol.Rout", {

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
    confs <- irace:::sampleSobol(params, 10)
    expect_equal(nrow(confs), 10)
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
  })

})

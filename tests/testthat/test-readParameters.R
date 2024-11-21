withr::with_output_sink("test-readParameters.Rout", {

  test_that("error checking", {

    ref <- parametersNew(param_real(name = "tmp", lower = 0, upper=1,
      label = "", digits = 5), forbidden = "tmp == 0")

    expect_identical(ref, readParameters(text='
tmp "" r (0, 1)

[global]
digits = 5
[forbidden]
tmp == 0
'))

    expect_identical(ref, readParameters(text='
tmp "" r (0, 1)
[forbidden]
tmp == 0
[global]
digits = 5
'))

    expect_identical(ref, readParameters(text='
tmp "" r (0, 1)

[forbidden]
tmp == 0

[global]

digits = 5
'))

expect_error(readParameters(text = '
rest_t "--restart_temp_ratio="  r,log (0.00001, 0.9)
[global]
digits = 4
'), "Domain bounds")

expect_no_error(readParameters(text = '
rest_t "--restart_temp_ratio="  r,log (0.00001, 0.9)
[global]
digits = 5
'))

expect_error(readParameters(text = '
temp "" r (0, 10)
t-max "" i (1, 50)
'), "name must be alphanumeric")

expect_error(readParameters(text = '
temp "" r (0, 10)
[forbidden]
tmp == 0
'), "contains unknown parameter(s): tmp", fixed = TRUE)

expect_error(readParameters(text = '
temp "" r (0, 10)
temp "" i (1, 50)
'), "Duplicated parameter name")

expect_error(readParameters(text = '
temp "" c ("a", "b", "a")
'), "duplicated values")

expect_error(readParameters(text = '
temp --temp i (0, 10)
'), "Parameter label (switch) must be a double-quoted string", fixed = TRUE)

expect_error(readParameters(text = '
temp "--temp" i,lag (0, 10)
'), "Parameter type must be a single character in .'c','i','r','o'.")

expect_error(readParameters(text = '
temp "--temp" i c(0, 10)
'), "Allowed values must be a list within parenthesis at line 2")

expect_error(readParameters(text = 'param1 "--param1 " c "a,b,c"'),
             "Allowed values must be a list within parenthesis at line 1")

expect_error(readParameters(text = 'param1 "--param1 " i (1,2,3 )'),
             "Incorrect numeric range")
expect_error(readParameters(text = 'param1 "--param1 " i ( 1,10) |'),
             "Expected condition before '|'")
expect_error(readParameters(text = 'param1 "--param1 " i ( 1, 10) param1 < param2'),
             "Expected condition after '|'")

expect_error(readParameters(text = '
param1 "--param1 " i (0,2)
param2 "--param2 " r (0,1) | param1 <> 1'),
"Invalid condition 'param1 <> 1': ")

expect_error(readParameters(text = '
param1 "--param1 " i (0,2)
param2 "--param2 " r (0,2) | param1 < param2'),
"Cycle detected.+param2")

expect_error(readParameters(text = '
param1 "--param1 " i (0,2)
param2 "--param2 " r (0,param2)'),
"Cycle detected.+param2")

expect_error(readParameters(text = '
param1 "--param1 " i (0,2)
param2 "--param2 " r (0,2) | param1 < param3'),
"Parameter 'param2' depends on 'param3' which is not defined")

expect_error(readParameters(text = '
param1 "--param1 " i (0,2)
param2 "--param2 " r (0,param3)'),
"'param3' cannot be found in the scenario parameters")

expect_error(readParameters(text = '
param1 "--param1 " c (a,b)
param2 "--param2 " r (0,param1)'),
"Domain of parameter 'param2' depends on non-numerical parameters: param1")

expect_error(readParameters(text = 'param1 "--param1 " i (1.1,2.0)'),
"For parameter 'param1' of type 'i' values must be integers")

expect_error(readParameters(text = 'param1 "--param1 " r (0.01, 0.001)'),
"Lower bound must be smaller than upper bound in numeric range")

expect_error(readParameters(text = 'param1 "--param1 " i (1, 1)'),
"Lower bound must be smaller than upper bound in numeric range")

expect_error(readParameters(text = '
#

#
'), "No parameter definition found")

test_that("ordinal out of order", {
expect_error(readParameters(text = '
param "" o (0, 2, 1)
'), "the values are not in increasing order")
})

expect_error(readParameters(text = 'param1 "--param1 " r,log (0, 100)'),
             "of parameter of type 'log' contains non-positive values")

expect_error(readParameters(text = 'param1 "--param1 " r,log (0.0001, 0.99999)', digits=3),
             "must be representable within the given 'digits=3'; you would need at least 'digits=5'")

expect_error(param_cat(name = "algorithm", values = c("as", "mmas", "eas", "ras", "acs"), label = "--", condition = "a == 0 && b == 2"),
             "Please use '&' and '|' instead of '&&' and '|' in: a == 0 && b == 2", fixed = TRUE)

expect_error(param_cat(name = "algorithm", values = c("as", "mmas", "eas", "ras", "acs"), label = "--", condition = quote(a == 0 || b == 2)),
             "Please use '&' and '|' instead of '&&' and '|' in: a == 0 || b == 2", fixed = TRUE)

expect_error(param_cat(name = "algorithm", values = c("as", "mmas", "eas", "ras", "acs"), label = "--", condition = 0),
             "Invalid condition", fixed = TRUE)

expect_error(parametersNew(param_real(name = "alpha", lower = 0.0, upper=5.0),
                           param_real(name = "beta", lower = 0.0, upper = 10.0),
                           forbidden = expression((alpha == 0) || (beta == 0))),
             "Please use '&' and '|' instead of '&&' and '|' in: (alpha == 0) || (beta == 0)", fixed = TRUE)

expect_error(parametersNew(param_real(name = "alpha", lower = 0.0, upper=5.0),
                           param_real(name = "beta", lower = 0.0, upper = 10.0),
                           forbidden = quote((alpha == 0) && (beta == 0))),
             "Please use '&' and '|' instead of '&&' and '|' in: (alpha == 0) && (beta == 0)", fixed = TRUE)

expect_error(parametersNew(param_real(name = "alpha", lower = 0.0, upper=5.0),
                           param_real(name = "beta", lower = 0.0, upper = 10.0),
                           forbidden = TRUE),
             "Invalid forbidden expression", fixed = TRUE)

expect_identical(param_real(name = "p", lower = "-1", upper = "10")$domain, c(-1,10))
expect_identical(param_int(name = "p", lower = "-1", upper = "10")$domain, c(-1L,10L))
expect_identical(param_int(name = "p", lower = -1, upper = 10)$domain, c(-1L,10L))
expect_identical(param_int(name = "p", lower = "-1", upper = expression(a))$domain[[1L]], -1L)
expect_identical(
  param_int(name = "p", lower = "-1", upper= "a"),
  param_int(name = "p", lower = "-1", upper= expression(a)))
expect_identical(
  param_int(name = "p", lower = -1, upper= "a"),
  param_int(name = "p", lower = "-1", upper= "a"))

})
})

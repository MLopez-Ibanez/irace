withr::with_output_sink("test-readconfs.Rout", {

params <- readParameters("parameters.txt")

expect_error_readconfs <- function(params, table, exp)
  expect_error(irace::readConfigurationsFile(text=table, parameters = params), exp)

test_that("no error", {
  x <- irace::readConfigurationsFile(text='
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
NA       NA   "x3"   4.5   "low"', parameters=params)
  expect_equal(nrow(x), 3L)
  expect_equal(ncol(x), 6L) # It adds the fixed column

  expect_identical(
    readConfigurationsFile(text='
param1 mode real mutation
5      x2   4.0  low
1      x2   4.0  low', parameters=params),
readConfigurationsFile(text='
param1 param2 mode real mutation
5      NA     x2   4.0  low
1      NA     x2   4.0  low', parameters=params))
})

test_that("wrong fixed", {
  expect_error_readconfs(params, '
param1 param2 mode   real mutation fixed
5       NA    "x2"   4.0  "low"    fixed
1       NA    "x2"   4.0  "low"    fixed
NA      NA    "x3"   4.5  "low"    wrong', "is not among the valid values")
})

test_that("checkDuplicates", {
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
NA       NA    "x3"   4.5   "low"
', "Duplicated")

  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', "Duplicated")

})

test_that("parameter values", {
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.0   "low"
11      NA    "x2"   4.0   "low"
', "is not within the valid range")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5001 "low"
', "is not within the valid range")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1.1     NA    "x2"   4.5   "low"
', "is not an integer")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x2"   4.0   "low"
1       NA    "x2"   4.5   "lower"
', "is not among the valid values")
  expect_error_readconfs(params, '
param1 param2 mode   real mutation
5       NA    "x3"   4.0   "low"
1       NA    "x2"   4.5   "low"
', "is not enabled")
})

test_that("parameter names", {
  expect_error_readconfs(params, '
param1 param0 mode   real mutation
5        NA    "x2"   4.0   "low"
1        NA    "x2"   4.0   "low"
', "do not match")
  expect_error_readconfs(params, '
param1 mode   real mutation
5       "x1"   3.0   "low"
1       "x2"   4.0   "low"
', 'Configuration number 1.*param2.*is missing')
  expect_error_readconfs(params, '
param1 param2 mode   real mutation param3
5        NA    "x2"   4.0   "low"  NA
1        NA    "x2"   4.0   "low"  NA
', "do not match")
})

test_that("inactive variable parameters may be omitted", {
  for (definition in c('i (1, 10)', 'r (0, 1)', 'c (x, y)', 'o (x, y)')) {
    params <- readParameters(text=paste0('
param1 "" i (1, 2)
param2 "" c (a, b, c)
param3 "" ', definition, ' | !(param2 %in% c("a", "b"))'))
    full <- readConfigurationsFile(text='
param1 param2 param3
1 a NA
2 b NA
1 b NA
2 a NA', parameters=params)
    compact <- 'param1 param2\n1 a\n2 b\n1 b\n2 a'
    expect_identical(readConfigurationsFile(text=compact, parameters=params), full)
    expect_identical(irace:::fix_configurations(full[c("param2", "param1")], params), full)

    path <- withr::local_tempfile()
    writeLines(compact, path)
    expect_identical(readConfigurationsFile(path, parameters=params), full)

    confs_txt <- 'param1 param2\n1 a\n2 c'
    expect_error_readconfs(params, confs_txt, 'Configuration number 2.*param3.*is missing')
    writeLines(confs_txt, path)
    expect_error(readConfigurationsFile(path, parameters=params),
                 'Configuration number 2 from file .*param3.*is missing')
    expect_error_readconfs(params, 'param2\na', 'param1.*is missing')
    expect_error_readconfs(params, 'param1 param2\n1 a\n1 a', 'Duplicated')
  }
})

test_that("omitted parameters respect chained conditions and dependent domains", {
  params <- readParameters(text='
dependent "" r (0, parent)
child     "" c (x, y) | parent > 0
parent    "" r (0, 1) | mode == "on"
mode      "" c (on, off)')
  full <- readConfigurationsFile(text='
mode parent child dependent
off  NA     NA    NA', parameters=params)
  expect_identical(readConfigurationsFile(text='mode\noff', parameters=params), full)
  expect_error_readconfs(params, 'mode\non', 'parent.*is missing')
  expect_error_readconfs(params, 'mode parent child\noff NA NA\non 0.5 x',
                         'Configuration number 2.*dependent.*is missing')
})

test_that("omitted fixed parameters are resolved in dependency order", {
  params <- readParameters(text='
child  "" c (yes) | parent == "yes"
parent "" c (yes) | mode == "on"
mode   "" c (on, off)')
  expect_identical(
    readConfigurationsFile(text='mode\noff\non', parameters=params),
    readConfigurationsFile(text='mode parent child\noff NA NA\non yes yes', parameters=params))
})


test_that("conditional fixed", {

  params <- readParameters(text='
param1 "" c ("fixed")
param2 "" c ("cond_fixed") | param3 == "on"
param3 "" c ("on", "off")
')

  x <- irace::readConfigurationsFile(text='
param1 param3
fixed  off
', parameters=params)

  expect_equal(x[["param2"]], NA_character_)

  x <- irace::readConfigurationsFile(text='
param1 param2 param3
fixed  NA     off
', parameters=params)

  x <- irace::readConfigurationsFile(text='
param1 param3
fixed  on
', parameters=params)

  expect_equal(x[["param2"]], "cond_fixed")

  expect_error_readconfs(params, '
param1 param2      param3
fixed  cond_fixed     off
', "is not enabled because of condition")
})

}) # withr::with_output_sink

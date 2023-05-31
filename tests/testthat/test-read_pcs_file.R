withr::with_output_sink("test-read_pcs_file.Rout", {
test_that("read_pcs_file", {
  pcs_table <- '
# name       domain
algorithm    {as,mmas,eas,ras,acs}[as]
localsearch  {0, 1, 2, 3}[0]
alpha        [0.00, 5.00][1]
beta         [0.00, 10.00][1]
rho          [0.01, 1.00][0.95]l
ants         [1, 100][10]il
q0           [0.0, 1.0][0]
rasrank      [1, 100][1]i
elitistants  [1, 750][1]i
nnls         [5, 50][5]i
dlb          {0, 1}[1]
Conditionals:
q0 | algorithm in {acs}
rasrank | algorithm in {ras}
elitistants | algorithm in {eas}
nnls | localsearch in {1,2,3}
dlb | localsearch in {1,2,3}
 {alpha=0, beta= 0.0} 
{ants=1,algorithm=eas}
 {ants=1,algorithm="ras"}'
  
  parameters_table <- '
# name       domain
algorithm "algorithm" c (as,mmas,eas,ras,acs)
localsearch "localsearch" c (0, 1, 2, 3)
alpha "alpha" r (0.00, 5.00)
beta "beta" r (0.00, 10.00)
rho "rho" r,log (0.01, 1.00)
ants "ants" i,log (1, 100)
q0 "q0" r (0.0, 1.0) | algorithm == "acs"
rasrank "rasrank" i (1, 100) | algorithm == "ras"
elitistants "elitistants" i (1, 750) | algorithm == "eas"
nnls "nnls" i (5, 50) | localsearch %in% c("1","2","3")
dlb "dlb" c (0, 1) | localsearch %in% c("1","2","3")

[forbidden]
(alpha == 0) & (beta == 0.0)
(ants == 1) & (algorithm == "eas")
(ants == 1) & (algorithm == "ras")
'
  expect_equal(parameters_table, read_pcs_file(text=pcs_table))
})
})


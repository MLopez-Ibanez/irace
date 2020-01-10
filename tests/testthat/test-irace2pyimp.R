context("irace2pyimp") # Avoid bug in testthat 2.0.0
test_that("irace2pyimp", {
  skip_on_cran()
  
  check_irace2pyimp_files <- function(outdir, origdir)
  {
    # Print it for debugging if something goes wrong
    cat("outdir: ", outdir, "\n")
    
    outfiles <- c("scenario.txt", "params.pcs", "runhistory.json", "traj_aclib2.json", "instances.txt", "features.csv")
    for (file in outfiles) {
      outfile <- file.path(outdir, file)
      origfile <- file.path(origdir, file)
      if (file.exists(paste0(origfile, ".gz")))
        origfile <- paste0(origfile, ".gz")
      else
        expect_true(file.exists(origfile))
      expect_true(file.exists(outfile))
      expect_equal(readLines(origfile), readLines(outfile))
    }
  }
 
  # acotsp example 1: convert irace.Rdata to PyImp's input format, without normalisation
  cat(". Testing acotsp_1","\n")
  outdir <- file.path("/home/manu/work/irace/nguyen/tests/testthat/", "irace2pyimp/acotsp_1")
  outdir <- tempdir()
  logfile <- file.path(system.file(package="irace"), "examples", "irace2pyimp",
                       "acotsp", "irace.Rdata")
  featuresfile <- file.path(system.file(package="irace"), "examples",
                            "irace2pyimp", "acotsp", "features.csv")

  irace2pyimp(file = logfile, instanceFeatureFile = featuresfile, outdir = outdir)
  check_irace2pyimp_files(outdir, "./irace2pyimp/acotsp_1/")

  # acotsp example 4: convert irace.Rdata to PyImp's input format, with normalisation based on feature-vector values
  cat(". Testing acotsp_4","\n")
  outdir <- tempdir()
  logfile <- file.path(system.file(package="irace"), "examples", "irace2pyimp",
                       "acotsp", "irace.Rdata")
  featuresfile <- file.path(system.file(package="irace"), "examples",
                            "irace2pyimp", "acotsp", "features.csv")

  irace2pyimp(file = logfile, outdir = outdir,
              normalise='feature', instanceFeatureFile = featuresfile)
  check_irace2pyimp_files(outdir, "./irace2pyimp/acotsp_4/")

  # 002-TemplateDesign example: using irace2pyimp with filter conditions and normalisation:
  # - filter tuning data such that only configurations with n_templates_middle<=40 are used, then convert irace.Rdata to PIMP's input format, 
  # - the cost values of all configurations are normalised on an instance-basis
  cat(". Testing 002-TemplateDesign","\n")
  outdir <- "/home/manu/work/irace/nguyen/tests/testthat/irace2pyimp/002-TemplateDesign" 
  outdir <- tempdir()
  logfile <- file.path(system.file(package="irace"), "examples", "irace2pyimp",
                       "002-TemplateDesign", "irace.Rdata")

  irace2pyimp(file = logfile, outdir = outdir, normalise = "instance",
              filterConditions = "n_templates_middle<=40")

  check_irace2pyimp_files(outdir, "./irace2pyimp/002-TemplateDesign")
})

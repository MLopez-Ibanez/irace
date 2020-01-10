library("jsonlite")
# Read parameters from JSON file
irace.params <- fromJSON(txt = "irace_options.json",
                              simplifyDataFrame = TRUE)

is.na.nowarn <- function(x)
  return(suppressWarnings(is.na(x)))

is.null.or.na.or.empty <- function(x)
  return(is.null(x) | is.na.nowarn(x) | (x == ""))

format.number.or.string <- function(x)
{
  if (is.na.nowarn(x))
    return("NA")

  y <- suppressWarnings(as.numeric(x))
  if (is.na.nowarn(y))
    return(sprintf('"%s"', x))
  else
    return(y)
}
to.plain.text <- function(x)
  return(gsub("\\\\code\\{([^}]+)\\}", "\\1", x))
  
     
ordered.sections <- c("General options",
                      "Elitist \\irace",
                      "Internal \\irace options",
                      "Target algorithm parameters",
                      "Target algorithm execution",
                      "Initial configurations",
                      "Training instances",
                      "Tuning budget",
                      "Statistical test",
                      "Adaptive capping",
                      "Recovery",
                      "Testing")
sections <- unique(irace.params[,"section"])
# All options must have a section.
stopifnot(setequal(ordered.sections, sections))

# Generate Roxygen style comment.
options.comment.filename <- "./irace_options_comment.R" 
marker.beg <- "# __IRACE_OPTIONS__BEGIN__"
marker.end <- "# __IRACE_OPTIONS__END__"
man.text <- marker.beg
man.text <- c(man.text, "#' \\itemize{")


for (section in ordered.sections) {
  man.text <- c(man.text,
                sprintf("#'  \\item %s:", gsub("\\\\([^ ]+)", "\\\\code{\\1}", section)),
                "#'    \\describe{")
  parameters <- irace.params[irace.params[, "section"] == section, ]
  parameters <- parameters[substring(parameters[, "name"], 1, 1) != ".", , drop = FALSE]
  parameters <- parameters[!is.null.or.na.or.empty(parameters[, "man"])
                           | !is.null.or.na.or.empty(parameters[, "description"]), , drop = FALSE]
  if (nrow(parameters) < 1) next
  sec.text <- apply(parameters, 1, function(x) {
    paste0("#'      \\item{\\code{", x["name"], "}}{",
           ifelse(is.null.or.na.or.empty(x["man"]),
                                         x["description"], x["man"]),
                  " (Default: \\code{",
           format.number.or.string(x["default"]),
           "})}")})

  man.text <- c(man.text, sec.text, "#'    }")
}
man.text <- c(man.text, "#' }", marker.end)
writeLines(man.text, con = options.comment.filename)

# FIXME: Implement this in pure R, and remove use of 'sed'
system2("sed",
        args = c("-i", "-e",
                 sprintf("\"/$lead/,/$tail/{ /$lead/{r %s
                         };; d }\"", options.comment.filename),
                 "../R/readConfiguration.R"),
        env = c(sprintf("lead='^%s$';", marker.beg),
                sprintf("tail='^%s$';", marker.end)))
  
 

# Generate file with replacement for vignettes

gendefparameter <- function(x)
{
  return(sprintf(" \\defparameter%s{%s}{%s}{%s}%%\n%s\n",
                 ifelse(x["short"] == "", "",
                        # we have to cut the -
                        paste0("[", substring(x["short"], 2), "]")),
                 # Option names starting with "." are special: They should only appear in the vignette and there is no associated R option."
                 ifelse(substring(x["name"], 0, 1) == ".",
                        paste0("-{}-", substring(x["long"], 3)), x["name"]),
                 # we have to cut the --
                 substring(x["long"], 3),
                 ifelse(is.na.nowarn(x["default"]), "", x["default"]),
                 x["vignettes"]))
}

vin.text <-"\n"
for (section in ordered.sections) {
  vin.text <- c(vin.text,
                paste0("\n\\subsection[",
                       gsub("\\", "", section, fixed = TRUE),
                       "]{", section, "}"),
                "\\begin{description}")
  parameters <- irace.params[irace.params[, "section"] == section, ]
  parameters <- parameters[!is.na(parameters[, "vignettes"]) & !is.null(parameters[,"vignettes"]) & parameters[,"vignettes"]!="",]
  if (nrow(parameters) < 1) next
  sec.text <- apply(parameters, 1, gendefparameter)

  vin.text <- c(vin.text, sec.text, "\\end{description}")
}
writeLines(vin.text, con = "../vignettes/section/irace-options.tex")

# Generate template of scenario.
out.text <-
'###################################################### -*- mode: r -*- #####
## Scenario setup for Iterated Race (irace).
############################################################################

## To use the default value of a parameter of iRace, simply do not set
## the parameter (comment it out in this file, and do not give any
## value on the command line).
'
for (i in seq_len(nrow(irace.params))) {
  p <- irace.params[i, ]
  if (substring(p["name"], 1, 1) == ".") next
  if (p["description"] == "") next
  if (p["name"] %in% c("scenarioFile")) next 
  out.text <- c(out.text,
                strwrap(to.plain.text(p["description"]), width = 75,  prefix = "## "),
                sprintf("# %s = %s\n", p["name"],
                        format.number.or.string(p["default"])))
}
out.text <- c(out.text,
'## END of scenario file
############################################################################
')

writeLines(out.text, con = "../inst/templates/scenario.txt.tmpl")


# Generate R code for options
irace.params <- irace.params[, c("name", "type", "short", "long", "default", "description")]
irace.params$description <- to.plain.text(irace.params$description)
rownames (irace.params) <- irace.params[,"name"]
irace.params.names <- rownames(irace.params)[substring(rownames(irace.params), 1, 1) != "."]

r.text <- sprintf('## This file was generated by scripts/generate-options.R
# Non-variable options (such as --help and --version) have names starting with "."
# Variables that do not have a command-line option have description == ""
# Types are b(oolean), i(nteger), s(tring), r(eal), p(ath), x (R object or no value)
# FIXME: Add special type for R functions.
# FIXME: For i and r add their range.
.irace.params.def <- %s
.irace.params.names <- %s
## FIXME: If these values are special perhaps they should be saved in $state ?
.irace.params.recover <- c("instances", "seed", "testInstances",
                           # We need this because this data may mutate
                           "targetRunnerData", "elitist", "deterministic")
',
paste0(deparse(irace.params), collapse = "\n"),
paste0(deparse(irace.params.names), collapse = "\n"))

writeLines(r.text, con = "../R/irace-options.R")

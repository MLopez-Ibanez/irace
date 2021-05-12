old_opts = options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

old_opts = lapply(old_opts, function(x) if (is.null(x)) FALSE else x)


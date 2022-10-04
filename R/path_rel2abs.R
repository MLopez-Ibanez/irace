#' Converts a relative path to an absolute path. It tries really hard to create
#' canonical paths.
#' 
#' @param path (`character(1)`) Character string representing a relative path.
#' @param cwd (`character(1)`) Current working directory.
#'
#' @return (`character(1)`) Character string representing the absolute path
#' 
#' @examples
#' path_rel2abs("..")
#' @export
path_rel2abs <- function (path, cwd = getwd())
{
  # Keep doing gsub as long as x keeps changing.
  gsub.all <- function(pattern, repl, x, ...) {
    repeat {
      newx <- gsub(pattern, repl, x, ...)
      if (newx == x) return(newx)
      x <- newx
    }
  }
  # FIXME: Why NA and not FALSE?
  irace_normalize_path <- function(path)
    suppressWarnings(normalizePath(path, winslash = "/", mustWork = NA))
      
  if (is.null.or.na(path)) {
    return (NULL)
  } else if (path == "") {
    return ("")
  }
  
  # Using .Platform$file.sep is too fragile. Better just use "/" everywhere.
  s <- "/"

  # Possibly expand ~/path to /home/user/path.
  path <- path.expand(path)
  # Remove winslashes if given.
  path <- gsub("\\", s, path, fixed = TRUE)

  # Detect a Windows drive
  windrive.regex <- "^[A-Za-z]:"
  windrive <- ""
  if (grepl(paste0(windrive.regex, "($|", s, ")"), path)) {
    m <- regexpr(windrive.regex, path)
    windrive <- regmatches(path, m)
    path <- sub(windrive.regex, "", path)
  }

  # Change "/./" to "/" to get a canonical form 
  path <- gsub.all(paste0(s, ".", s), s, path, fixed = TRUE)
  # Change "//" to "/" to get a canonical form 
  path <- gsub(paste0(s, s, "+"), s, path)
  # Change "/.$" to "/" to get a canonical form 
  path <- sub(paste0(s, "\\.$"), s, path)
  # Drop final "/"
  path <- sub(paste0(s, "$"), "", path)
  if (path == "") path <- s

  # Prefix the current cwd to the path if it doesn't start with
  # / \\ or whatever separator.
  if (path == "." || !startsWith(path, s)) {
    # There is no need to normalize cwd if it was returned by getwd()
    if (!missing(cwd)) {
      # Recurse to get absolute cwd
      cwd <- path_rel2abs(cwd)
    }

    # Speed-up the most common cases.
    # If it is just "."
    if (path == ".") return (irace_normalize_path(cwd))
        
    # If it does not contain separators at all and does not start with ".."
    if (!startsWith(path, "..") && !grepl(s, path)) {
      # it may be a command in the path.
      sys_path <- suppressWarnings(Sys.which(path))
      if (nchar(sys_path) > 0
          && file.access(sys_path, mode = 1) == 0
          && !file.info(sys_path)$isdir) {
        return(irace_normalize_path(as.vector(sys_path)))
      }
    }
  
    # Remove "./" from the start of path.
    path <- sub(paste0("^\\.", s), "", path)
    # Make it absolute but avoid doubling s
    if (substring(cwd, nchar(cwd)) == s) path <- paste0(cwd, path)
    else path <- paste0(cwd, s, path)
    # If it is just a path without ".." inside
    if (!grepl(paste0(s,"\\.\\."), path)) {
      return (irace_normalize_path(path))
    }
    # Detect a Windows drive
    if (grepl(paste0(windrive.regex, "($|", s, ")"), path)) {
      m <- regexpr(windrive.regex, path)
      windrive <- regmatches(path, m)
      path <- sub(windrive.regex, "", path)
    }
  }
  # else

  # Change "/x/.." to "/" to get a canonical form 
  prevdir.regex <- paste0(s, "[^", s,"]+", s, "\\.\\.")
  repeat {
    # We need to do it one by one so "a/b/c/../../../" is not converted to "a/b/../"
    tmp <- sub(paste0(prevdir.regex, s), s, path)
    if (tmp == path) break
    path <- tmp
  }
  # Handle "/something/..$" to "/" that is, when ".." is the last thing in the path.
  path <- sub(paste0(prevdir.regex, "$"), s, path)

  # Handle "^/../../.." to "/" that is, going up at the root just returns the root.
  repeat {
    # We need to do it one by one so "a/b/c/../../../" is not converted to "a/b/../"
    tmp <- sub(paste0("^", s, "\\.\\.", s), s, path)
    if (tmp == path) break
    path <- tmp
  }
  # Handle "^/..$" to "/" that is, when ".." is the last thing in the path.
  path <- sub(paste0("^", s, "\\.\\.$"), s, path)
  # Add back Windows drive, if any.
  path <- paste0(windrive, path)

  # We use normalizePath, which will further simplify the path if
  # the path exists.
  irace_normalize_path(path)
}

#' Converts a relative path to an absolute path.
#'
#' If the path passed corresponds to an executable, it tries to find its path
#' using [Sys.which()].  Expansion of `'~'` in Windows follows the definition
#' of [fs::path_expand()] rather than [base::path.expand()]. This function
#' tries really hard to create canonical paths.
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
  # We need to do this even with the path returned by `getwd()`.
  cwd <- fs::path_norm(fs::path_expand(cwd))

  if (fs::is_absolute_path(path)) {
    # Possibly expand ~/path to /home/user/path.
    path <- fs::path_expand(path)
  } else if (!startsWith(path, ".")) {
    # it may be a command in the path.
    sys_path <- suppressWarnings(Sys.which(path))
    if (nzchar(sys_path)
      && fs::file_access(sys_path, "execute")
      && fs::is_file(sys_path)) {
      path <- as.vector(sys_path)
    }
  }
  as.character(fs::path_abs(path, start = cwd))
}

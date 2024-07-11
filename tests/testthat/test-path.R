test_path_rel2abs <- function(testcases)
{
  for (i in 1:nrow(testcases)) {
    orig <- testcases[i,1L]
    cwd <-  testcases[i,2L]
    res <- path_rel2abs(testcases[i,1L], cwd)
    if (testcases[i,3L] == "Sys.which") {
      exp <- fs::path_abs(Sys.which(testcases[i,1L]))
    } else {
      exp <- gsub("\\", "/", fs::path_expand(testcases[i,3L]), fixed = TRUE)
    }
    if (res == exp) {
      #cat("[OK] ", i, ": path_rel2abs(\"", orig, "\", \"", cwd, "\") -> ", res, "\n", sep="")
    } else {
      cat(sep="", "[FAILED] ", i, ": path_rel2abs(\"", orig, "\", \"", cwd, "\") -> ", res, " but expected: ", exp, "\n")
    }
    expect_match(res, exp, fixed = TRUE)
  }
}

test_that("test path_rel2abs", {
  # Try to set wd; otherwise fail silently.
  old.cwd <- getwd()
  skip_if(is.null(old.cwd))
  withr::defer(setwd(old.cwd))
  tryCatch(setwd("/tmp"), error = function(e) { skip(e) })
  
  testcases <- read.table(text='
"."                         "/tmp"  "/tmp"
".."                        "/tmp"  "/"
"../"                       "/tmp"  "/"
"../."                      "/tmp"  "/"
"../.."                     "/tmp"  "/"
"../../"                    "/tmp"  "/"
"../../x.r"                 "/tmp"  "/x.r"
"../leslie/"                "/tmp"  "/leslie"
"../leslie/x.r"             "/tmp"  "/leslie/x.r"
"../x.r"                    "/tmp"  "/x.r"
"..irace"                   "/tmp"  "/tmp/..irace"
"./"                        "/tmp"  "/tmp"
"./."                       "/tmp"  "/tmp"
"./"                        "/tmp/"  "/tmp"
"./."                       "/tmp/"  "/tmp"
"././x.r"                   "/tmp"  "/tmp/x.r"
"./irace/../x.r"            "/tmp"  "/tmp/x.r"
"./x.r"                     "/tmp"  "/tmp/x.r"
".x.R"                      "/tmp"  "/tmp/.x.R"
"/./x.r"                    "/tmp"  "/x.r"
"/home"                     "/tmp"  "/home"
"/home/leslie/././x.r"      "/tmp"  "/home/leslie/x.r"
"/home/leslie/~/x.r"        "/tmp"  "/home/leslie/~/x.r"
"/~/x.r"                    "/tmp"  "/~/x.r"
"leslie/leslie/../../irace" "/tmp"  "/tmp/irace"
"x.r"                       "/tmp"  "/tmp/x.r"
"~/irace/../x.r"            "/tmp"  "~/x.r"
"~/x.r"                     "/tmp"  "~/x.r"
"../../../data"             "./"    "/data"
"../../../data"             "/tmp/a/b/c/" "/tmp/data"
"..//a"                     ".//"   "/a"
', stringsAsFactors=FALSE)
  test_path_rel2abs(testcases)
})

test_that("test path_rel2abs without /tmp", {

  testcases <- read.table(text='
.                         N:\\\\tmp  N:/tmp
..                        N:\\\\tmp  N:/
..\\\\                       N:\\\\tmp  N:/
..\\\\.                      N:\\\\tmp  N:/
..\\\\..                     N:\\\\tmp  N:/
..\\\\..\\\\                    N:\\\\tmp  N:/
..\\\\..\\\\x.r                 N:\\\\tmp  N:/x.r
..\\\\leslie\\\\                N:\\\\tmp  N:/leslie
..\\\\leslie\\\\x.r             N:\\\\tmp  N:/leslie/x.r
..\\\\x.r                    N:\\\\tmp  N:/x.r
..irace                   N:\\\\tmp  N:/tmp/..irace
.\\\\                        N:\\\\tmp  N:/tmp
.\\\\.                       N:\\\\tmp  N:/tmp
.\\\\                        N:\\\\tmp\\\\ N:/tmp
.\\\\.                       N:\\\\tmp\\\\  N:/tmp
.\\\\.\\\\x.r                   N:\\\\tmp  N:/tmp/x.r
.\\\\x.r                     N:\\\\tmp  N:/tmp/x.r
.\\\\irace\\\\..\\\\x.r            N:\\\\tmp  N:/tmp/x.r
.x.R                      N:\\\\tmp  N:/tmp/.x.R
.                         N:\\tmp  N:/tmp
..                        N:\\tmp  N:/
..\\                       N:\\tmp  N:/
..\\.                      N:\\tmp  N:/
..\\..                     N:\\tmp  N:/
..\\..\\                    N:\\tmp  N:/
..\\..\\x.r                 N:\\tmp  N:/x.r
..\\leslie\\                N:\\tmp  N:/leslie
..\\leslie\\x.r             N:\\tmp  N:/leslie/x.r
..\\x.r                    N:\\tmp  N:/x.r
..irace                   N:\\tmp  N:/tmp/..irace
.\\                        N:\\tmp  N:/tmp
.\\.                       N:\\tmp  N:/tmp
.\\                        N:\\tmp\\ N:/tmp
.\\.                       N:\\tmp\\  N:/tmp
.\\.\\x.r                   N:\\tmp  N:/tmp/x.r
.\\irace\\..\\x.r            N:\\tmp  N:/tmp/x.r
.\\x.r                     N:\\tmp  N:/tmp/x.r
.x.R                      N:\\tmp  N:/tmp/.x.R
.                         N:  N:/
..                        N:  N:/
..\\\\                       N:  N:/
..\\\\.                      N:  N:/
..\\\\..                     N:  N:/
..\\\\..\\\\                    N:  N:/
..\\\\..\\\\x.r                 N:  N:/x.r
..\\\\leslie\\\\                N:  N:/leslie
..\\\\leslie\\\\x.r             N:  N:/leslie/x.r
..\\\\x.r                    N:  N:/x.r
..\\                       N:  N:/
..\\.                      N:  N:/
..\\..                     N:  N:/
..\\..\\                    N:  N:/
..\\..\\x.r                 N:  N:/x.r
..\\leslie\\                N:  N:/leslie
..\\leslie\\x.r             N:  N:/leslie/x.r
..\\x.r                    N:  N:/x.r
..irace                   N:  N:/..irace
.\\\\                        N:  N:/
.\\\\.                       N:  N:/
.\\\\                        N:\\\\ N:/
.\\\\.                       N:\\\\  N:/
.\\\\.\\\\x.r                   N:  N:/x.r
.\\\\irace\\\\..\\\\x.r            N:  N:/x.r
.\\\\x.r                     N:  N:/x.r
.\\                        N:  N:/
.\\.                       N:  N:/
.\\                        N:\\ N:/
.\\.                       N:\\  N:/
.\\.\\x.r                   N:  N:/x.r
.\\irace\\..\\x.r            N:  N:/x.r
.\\x.r                     N:  N:/x.r
.x.R                      N:  N:/.x.R
.                         N:/tmp  N:/tmp
..                        N:/tmp  N:/
../                       N:/tmp  N:/
../.                      N:/tmp  N:/
../..                     N:/tmp  N:/
../../                    N:/tmp  N:/
../../x.r                 N:/tmp  N:/x.r
../leslie/                N:/tmp  N:/leslie
../leslie/x.r             N:/tmp  N:/leslie/x.r
../x.r                    N:/tmp  N:/x.r
..irace                   N:/tmp  N:/tmp/..irace
./                        N:/tmp  N:/tmp
./.                       N:/tmp  N:/tmp
./                        N:/tmp/ N:/tmp
./.                       N:/tmp/  N:/tmp
././x.r                   N:/tmp  N:/tmp/x.r
./irace/../x.r            N:/tmp  N:/tmp/x.r
./x.r                     N:/tmp  N:/tmp/x.r
.x.R                      N:/tmp  N:/tmp/.x.R
D:/./x.r                  N:/tmp  D:/x.r
D:\\\\.\\\\x.r            N:/tmp  D:/x.r
D:\\.\\x.r                N:/tmp  D:/x.r
D:                        N:/tmp  D:/
D:\\\\                    N:/tmp  D:/
D:/                       N:/tmp  D:/
D:/leslie/././x.r         N:/tmp  D:/leslie/x.r
D:/leslie/~/x.r           N:/tmp  D:/leslie/~/x.r
e:/home/leslie/x.r        /tmp  E:/home/leslie/x.r
leslie/leslie/../../irace N:/tmp  N:/tmp/irace
x.r                       N:/tmp  N:/tmp/x.r
~/irace/../x.r            N:/tmp  ~/x.r
~/x.r                     N:/tmp  ~/x.r
"R"                       "/tmp/" "Sys.which"
', stringsAsFactors=FALSE)
  test_path_rel2abs(testcases)
})

test_that("test path_rel2abs with symlink", {
  # Try to set wd; otherwise fail silently.
  old.cwd <- getwd()
  skip_if(is.null(old.cwd))
  withr::defer(setwd(old.cwd))
  tryCatch({
    tmp <- withr::local_tempdir()
    setwd(tmp)
    fs::dir_create("a")
    fs::file_create("a/b")
    fs::link_create(fs::path_abs("a"), "c")
  }, error = function(e) { skip(e) })
  testcases <- data.frame(p = "c/b", wd = ".", res = file.path(tmp, "c/b"),
    stringsAsFactors=FALSE)
  test_path_rel2abs(testcases)
})

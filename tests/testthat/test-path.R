context("path.rel2abs")

test_that("test.path.rel2abs", {
  # Try to set wd; otherwise fail silently.
  old.cwd <- getwd()
  if (is.null(old.cwd)) return(TRUE)
  on.exit(setwd(old.cwd), add = TRUE)
  try (setwd("/tmp"))
  if (getwd() != "/tmp") return(TRUE)
  
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
"e:/home/leslie/x.r"        "/tmp"  "e:/home/leslie/x.r"
"leslie/leslie/../../irace" "/tmp"  "/tmp/irace"
"x.r"                       "/tmp"  "/tmp/x.r"
"~/irace/../x.r"            "/tmp"  "~/x.r"
"~/x.r"                     "/tmp"  "~/x.r"
"../../../data"             "./"    "/data"
"../../../data"             "/tmp/a/b/c/" "/tmp/data"
"..//a"                     ".//"   "/a"
', stringsAsFactors=FALSE)
  for(i in 1:nrow(testcases)) {
    orig <- testcases[i,1]
    cwd <-  testcases[i,2]
    res <- irace:::path.rel2abs(testcases[i,1], cwd)
    exp <- gsub("\\", "/", path.expand(testcases[i,3]), fixed = TRUE)
    if (res == exp) {
      # cat("[OK] (", orig, ", ", cwd, ") -> ", res, "\n", sep="")
    } else {
      cat("[FAILED] (", orig, ", ", cwd, ") -> ", res, " but expected: ", exp, "\n")
    }
    expect_match(res, exp, fixed = TRUE)
  }
})

test_that("test.path.rel2abs for windows", {

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
.\\\\irace\\\\..\\\\x.r            N:\\\\tmp  N:/tmp/x.r
.\\\\x.r                     N:\\\\tmp  N:/tmp/x.r
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
D:\\\\.\\\\x.r                  N:/tmp  D:/x.r
D:\\.\\x.r                  N:/tmp  D:/x.r
D:                        N:/tmp  D:/
D:\\\\                       N:/tmp  D:/
D:/                       N:/tmp  D:/
D:/leslie/././x.r         N:/tmp  D:/leslie/x.r
D:/leslie/~/x.r        N:/tmp  D:/leslie/~/x.r
e:/home/leslie/x.r        /tmp  e:/home/leslie/x.r
leslie/leslie/../../irace N:/tmp  N:/tmp/irace
x.r                       N:/tmp  N:/tmp/x.r
~/irace/../x.r            N:/tmp  ~/x.r
~/x.r                     N:/tmp  ~/x.r
', stringsAsFactors=FALSE)
  for(i in 1:nrow(testcases)) {
    orig <- testcases[i,1]
    cwd <-  testcases[i,2]
    res <- irace:::path.rel2abs(testcases[i,1], cwd)
    exp <- gsub("\\", "/", path.expand(testcases[i,3]), fixed = TRUE)
    if (res == exp) {
      #cat("[OK] ", i, ": path.rel2abs(\"", orig, "\", \"", cwd, "\") -> ", res, "\n", sep="")
    } else {
      cat("[FAILED] ", i, ": path.rel2abs(\"", orig, "\", \"", cwd, "\") -> ", res, " but expected: ", exp, "\n")
    }
    expect_match(res, exp, fixed = TRUE)
  }
})


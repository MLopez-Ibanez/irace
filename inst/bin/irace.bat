@echo off
:: Windows has problems with spaces in directory names. Thus, it is better to
:: install R somewhere like "C:\R". You also need to add both R and the
:: directory where irace.bat is located to the PATH.
:: To edit the PATH in Windows, search for "Environment variables" in the
:: control panel, then edit PATH, then
:: add a string similar to "C:\R_PATH\bin;C:\IRACE_HOME\bin", where R_PATH
:: should be R's installation path and IRACE_HOME is irace's installation path
:: (the output given by running 'system.file(package="irace")' within R).
R.exe --vanilla --slave -e "library(irace);irace.cmdline()" --args %*

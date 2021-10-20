::##############################################################################
:: In Windows, we cannot execute directly .py files so we use this script as
:: the targetRunner that calls the actual .py file.
:: Contributed by Levi Ribeiro<azalos13@gmail.com>
::
:: This script is run in the execution directory (execDir, --exec-dir).
::
:: PARAMETERS:
:: %%1 is the candidate configuration number
:: %%2 is the instance ID
:: %%3 is the seed
:: %%4 is the instance name
:: The rest are parameters to the target-algorithm
::
:: RETURN VALUE:
:: This script should print one numerical value: the cost that must be minimized.
:: Exit with 0 if no error, with 1 in case of error
::##############################################################################
@echo off
:: If python is in the PATH environment variable, you can call it directly.
SET "pythonexe=python.exe"
:: Otherwise, you need to provide its absolute location
:: SET "pythonexe=C:\bin\python.exe"

:: This script is run in the execution directory (execDir, --exec-dir), which often does not coincide with the directory containing target-runner.py. Either a relative or absolute path should be provided.
SET "targetrunnerpy=..\target-runner.py"
set parameters=%*
%pythonexe% %targetrunnerpy%  %parameters%

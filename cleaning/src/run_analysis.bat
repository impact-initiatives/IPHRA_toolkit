@echo off
FOR /F "skip=2 tokens=2,*" %%A IN ('reg.exe query "HKlm\Software\R-core\r" /v "InstallPath"') DO set "InstallPath=%%B"

"%InstallPath%\bin\R.exe" CMD BATCH --vanilla --quiet run_analysis.R

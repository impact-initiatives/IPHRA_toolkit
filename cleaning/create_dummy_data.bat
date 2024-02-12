@echo off
FOR /F "skip=2 tokens=2,*" %%A IN ('reg.exe query "HKlm\Software\R-core\r" /v "InstallPath"') DO set "InstallPath=%%B"

cmd /c "echo START SCRIPT...&echo(&pause"

echo source("src/create_dummy_data.R") | "%InstallPath%\bin\R.exe" --ess --vanilla 

cmd /c "echo ...SCRIPT FINISHED&echo(&pause"
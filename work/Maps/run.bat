@echo off
echo This script copies the maps into the data dir.
call ..\setenv.bat
if not exist %WORKDATADIR% (mkdir %WORKDATADIR%)
if not exist %WORKDATADIR%\maps (mkdir %WORKDATADIR%\maps)
copy json\*.json %WORKDATADIR%\maps /Y

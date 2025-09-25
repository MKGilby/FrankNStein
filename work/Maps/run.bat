@echo off
echo This script copies the maps into the data dir.
call ..\setenv.bat
copy json\*.json %WORKDATADIR%\maps /Y


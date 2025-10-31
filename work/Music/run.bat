@echo off
echo This script copies the maps into the data dir.
call ..\setenv.bat
if not exist %WORKDATADIR% (mkdir %WORKDATADIR%)
if not exist %WORKDATADIR%\music (mkdir %WORKDATADIR%\music)
copy data\*.mo3 %WORKDATADIR%\music /Y

@echo off
echo This script copies the maps into the data dir.
copy json\*.json %WORKDATADIR%\maps /Y


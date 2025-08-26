@echo off
echo This script converts the old format graphics to PNGs.

call ..\setenv.bat
%WORKTOOLSDIR%\mkconv2 convert.mc2
if not exist %WORKDATADIR%\ (mkdir %WORKDATADIR%)
for %%i in (*.png) do %WORKTOOLSDIR%\pngout %%i %WORKDATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

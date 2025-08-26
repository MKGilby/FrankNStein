@echo off
echo This script converts the logo to PNGs.

call ..\setenv.bat
%WORKTOOLSDIR%\mkconv2 convert.mc2
copy data\spectrum.png .
for %%i in (*.png) do %WORKTOOLSDIR%\pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

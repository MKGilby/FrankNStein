@echo off
echo This script converts the old format graphics to PNGs.

..\..\tools\mkconv2 convert.mc2
if not exist ..\..\data\ (mkdir ..\..\data)
for %%i in (*.png) do ..\..\tools\pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

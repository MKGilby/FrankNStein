@echo off
echo This script converts the logo to PNGs.

..\..\tools\mkconv2 convert.mc2
copy data\spectrum.png .
for %%i in (*.png) do ..\..\tools\pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

@echo off
echo This script converts the old format graphics to PNGs.

..\..\tools\mkconv2 convert.mc2
for %%i in (*.png) do ..\..\tools\pngout %%i ..\..\data\decorations\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

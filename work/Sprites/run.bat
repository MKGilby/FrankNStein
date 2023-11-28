@echo off
echo This script converts the sprites to spritesheets.

SkeletonPrep
..\..\tools\mkconv2 convert.mc2
del skeletonsprite.png
for %%i in (*.png) do ..\..\tools\pngout %%i ..\..\data\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

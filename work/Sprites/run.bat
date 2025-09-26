@echo off
echo This script converts the sprites to spritesheets.

call ..\setenv.bat

%WORKTOOLSDIR%\SkeletonPrep
%WORKTOOLSDIR%\mkconv2 convert.mc2
del skeletonsprite.png
for %%i in (*.png) do %WORKTOOLSDIR%\pngout %%i %WORKDATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png
del *.log

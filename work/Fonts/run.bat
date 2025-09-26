@echo off
echo This script creates font file from font image.
call ..\setenv.bat
%WORKTOOLSDIR%\fontbuild2 data\cs_font.png cs_font.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-@.#2f',:!?#80#81#82#83)(" -colorkey
%WORKTOOLSDIR%\fontbuild2 data\meterfont.png meterfont.png -charset "0123456789:" -colorkey
for %%i in (*.png) do %WORKTOOLSDIR%\pngout %%i %WORKDATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

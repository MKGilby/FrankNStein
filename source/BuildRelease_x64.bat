@echo off
call ..\work\setenv.bat
if not exist ..\release\ (mkdir ..\release)
if not exist ..\release\x64 (mkdir ..\release\x64)
if not exist lib\ (mkdir lib) else (del /Q lib\*)
%FPCDIR%\fpc.exe -Twin64 -Px86_64 -l- -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib -Fuunits -Fuunits\SDL2 -Fu. -FUlib -FE..\release\x64 -oFrankNStein.exe FrankNStein.lpr
RMDIR "lib" /S /Q

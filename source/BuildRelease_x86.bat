@echo off
if not exist lib\ (mkdir lib) else (del /Q lib\*)
if not exist ..\release\ (mkdir ..\release)
if not exist ..\release\x86 (mkdir ..\release\x86)
copy ..\copyright.txt ..\release\x86
copy ..\license.txt ..\release\x86
copy ..\README-SDL.txt ..\release\x86
call ..\work\setenv.bat
%FPCDIR%\fpc.exe -l- -Twin32 -Pi386 -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib -Fuunits -Fuunits\SDL2 -Fu. -FUlib\ -FE..\release\x86 -oFrankNStein.exe FrankNStein.lpr
RMDIR "lib" /S /Q

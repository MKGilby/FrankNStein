@echo off
if not exist lib\ (mkdir lib)
if not exist lib\x86_64-win64\ (mkdir lib\x86_64-win64) else (del /Q lib\x86_64-win64\*)
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -Twin32 -Pi386 -MDelphi -Scghi -CX -O3 -XX -l -vewnhibq -vm6058,5024 -Filib\i386-win32 -Fuunits -Fuunits\SDL2 -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\freetype\lib\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fu. -FUlib\x86_64-win64 -FE..\_release -oFrankNStein_x86.exe FrankNStein.lpr

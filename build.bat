@echo off
rem This script builds the release version of FrankNStein Refurbished.
rem - Graphics and maps
cd work
call build_all.bat
rem - Datafile and binaries
cd ..\source
call BuildDatafile.bat
call BuildRelease_x64.bat
call BuildRelease_x86.bat

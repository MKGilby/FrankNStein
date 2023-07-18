@echo off
if not exist ..\data\ (mkdir ..\data)
if not exist ..\data\decorations\ (mkdir ..\data\decorations)
cd decorations
call run.bat
cd ..\maps
call run.bat
cd ..\fonts
call run.bat
cd ..\logo
call run.bat
cd ..

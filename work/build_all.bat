@echo off
if not exist ..\data\ (mkdir ..\data)
cd decorations
call run.bat
cd ..\maps
call run.bat
cd ..\fonts
call run.bat
cd ..\logo
call run.bat
cd ..\sprites
call run.bat
cd ..

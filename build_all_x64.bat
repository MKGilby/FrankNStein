@echo off
call work\setenv.bat
cd %TOOLSDIR%
call BuildTools.bat
cd ..\work
call BuildData.bat
cd ..\source
call BuildRelease_x64.bat
call BuildDatafile.bat
echo .
echo Build complete.
echo Before you can play, You must manually add
echo x64 version of SDL2.dll and bass.dll to release\x64 folder.
echo .
echo Press any key
pause >nul


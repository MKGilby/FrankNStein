call ..\work\setenv.bat
if exist ..\release\x64 (
  if exist ..\release\x64\FrankNStein.data del ..\release\x64\FrankNStein.data
  %SOURCETOOLSDIR%\mad4 ..\release\x64\FrankNStein.data ..\data * -1 -r -n
)

if exist ..\release\x86 (
  if exist ..\release\x86\FrankNStein.data del ..\release\x86\FrankNStein.data
  %SOURCETOOLSDIR%\mad4 ..\release\x86\FrankNStein.data ..\data * -1 -r -n
)

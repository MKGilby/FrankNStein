@echo off
echo Creating backup...
"c:\program files\winrar\winrar" a FrankNStein @backup.txt -r
timestamp FrankNStein.rar /d
move *.rar ..\..\_Backups

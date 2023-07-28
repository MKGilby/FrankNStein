@echo off
echo This script creates font file from font image.
rem ..\..\tools\fontbuild2 data\font.png fontface.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@-.!,()'" -colorkey -sort
rem TextureFont
..\..\tools\fontbuild2 data\cs_font.bdc cs_font.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-@.#2f',:!?" -colorkey
rem ..\..\tools\pngout font.png ..\..\data\font.png /Y /kanMZ,fnTZ,anIM
..\..\tools\pngout cs_font.png ..\..\data\cs_font.png /Y /kanMZ,fnTZ,anIM
rem del fontface.png
rem del font.png
del cs_font.png

@echo off
echo This script creates font file from font image.
..\..\tools\fontbuild2 data\font.png fontface.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@-.!,()'" -colorkey -sort
TextureFont
..\..\tools\pngout font.png ..\..\data\font.png /Y /kanMZ,fnTZ,anIM
del fontface.png
del font.png

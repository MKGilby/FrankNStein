@echo off
echo This script creates font file from font image.
..\..\tools\fontbuild2 data\cs_font.png cs_font.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-@.#2f',:!?#80#81#82#83)(" -colorkey
..\..\tools\pngout cs_font.png ..\..\data\cs_font.png /Y /kanMZ,fnTZ,anIM
del cs_font.png

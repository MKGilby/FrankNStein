@echo off
echo This script converts the human readable maps into binary format.
MapConverter
copy maps.bin ..\..\data /Y
del maps.bin

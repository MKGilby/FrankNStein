# Frank N Stein Refurbished
Remake of Frank'N'Stein and Frank'N'Stein Rebooted by Colin Stewart (from ZX Spectum 48K)

## Programming language
FreePascal (Lazarus 4.2 with FPC 3.2.2 Windows x64 or x86 version or cross-compiler to Win32 or Win64)
[Lazarus homepage](https://www.lazarus-ide.org/)

## Source codes
SDL2 pascal headers (source\units\sdl2) is licensed under MPL or zlib license.
GitHub for SDL2 pascal headers: [PascalGameDevelopment/SDL2-for-Pascal](https://github.com/PascalGameDevelopment/SDL2-for-Pascal)

The rest of source code is licensed under GNU GPL v3 (or later).

## Tools
PNGOut tool is by Ken Silverman [His homepage](http://advsys.net/ken)

MKConv2, MAD4, FontBuild2 and SkeletonPrep tools are made by me.

## Music and sound effects
Majority of music and all sound effects are made by Mike Fraley (rockfistus)

The title music is composed by me.

## Graphics and fonts
Graphics is created by me and licensed under CC BY-NC 4.0

Main font are drawn by me based on the original font by Colin Stewart.

## Compiling in windows environment
1. Go into folder "work" and edit setenv.bat, set FPCDIR to point to the folder
containing your fpc.exe
2. Go into folder "tools\source" and run BuildTools.bat
3. Go into folder "work" and run BuildData.bat
4. Go into folder "source" and run BuildRelease_x64.bat (or x86 as you wish.)
   You need Lazarus cross compiler libraries to be installed
   to compile x64 on x86 systems and vice-versa.
5. Download and extract the latest SDL2.dll into \release\x64 or x86
   (be aware of bitness!). The latest DLLs can be found on the [SDL releases page](https://github.com/libsdl-org/SDL/releases).
   Scroll down to the latest 2.xx version, click assets and download file.
   At the time of writing of this document the latest SDL2 version is 2.32.8.

## Compiled binaries from current build with datafiles and DLLs
[x64](https://mksztsz.hu/tmpfiles/FrankNStein_Refurbished_0.0.0.18.zip "Download x64 version") or
[x86](https://mksztsz.hu/tmpfiles/FrankNStein_Refurbished_x86_0.0.0.18.zip "Download x86 version").

## What's new

### 2025.09.05 - Build 18
- Power meter now measures power, as in original. Later I will add Rebooted
  style meter too.

### 2025.08.30 - Build 17
- Reworked fixed decoration handling. Now it's a full screen overlay picture
  depending on the type of the map. I construct it with mkconv2 from pieces,
  but you can draw one 256x192 png with transparent background.
- Removed some unneeded graphics files.

### 2025.08.26 - Build 16
- Added tool sources and build script for them.
- Reworked VMU (dropped PlayerRegistryUnit).
- Slot selector updates after coming back from playing.

### 2025.08.21 - Build 15
- Added electro-meter gfx. No needle yet.
- Updated used units.
- "Press .. to start" flashing is synced to music. (At least on my machine.)
- Slot selector "remembers" last used slot.
- Small reworks here and there.

### 2024.05.16 - Build 14
- Prof is animated again in SlotSelector.
- Picked up pieces are floating to their place.

### 2024.05.15 - Build 13
- Map data is stored in JSON format, so no preprocessing needed for them.
- Monster data is now stores speed in pixels/sec, and horizontal coordinates are
  in pixels instead of blocks.

### 2024.05.03 - Build 12
- Added third monster animation. (Double wheel at bottom of the first map.)
- Updated mkconv2 tool.
- Updated used units.
- Animations are added from JSON files during preparation.

### 2024.04.18 - Build 11
- Fixed snake animation.
- Updated mkconv2 tool.
- Updated used units.
- Animations are added from XML files during preparation.

### 2024.01.13 - Build 10
- Adjusted monster speed.
- Added second monster: snake. Needs a small adjustment with the head.

### 2024.01.08 - Build 9
- Added first monster: syringe.

### 2023.12.29 - Build 8
- Added '(' and ')' characters to font.
- Changed Lazarus version on opening screen.
- "Remake" word is changed to "Refurbication" on opening screen.
- Prof slides facing forward, not backwards.

### 2023.12.29 - Build 7
- Animation is time based now, not frame based.
- The next skeleton piece flashes.

### 2023.12.08 - Build 6
- Ice is slippery, can't stop on that.
- You can slide down poles.
- Added a nice speccy color strip to the botton right corner of non-ingame screens.

### 2023.12.06 - Build 5
- You can move and jump with controller.
- Spring descends with the prof when you stay on it.
- Spring kicks you up visually.

### 2023.12.05 - Build 4
- You can move left-right.
- You can fall down.
- You can jump when standing exactly on the springs.
- You can pick up skeleton pieces.

### 2023.11.30 - Build 3
- Skeleton pieces appears on first map.
- Slightly rearranged SlotSelector.
- Added helping text to SlotSelector. (Different when a controller is detected)
- Added different text to StartScreen when a controller is detected.
- Skeleton piece now have a 8x8 darkening box behind it (in addition to the red outline)
- ICE tiles are 7 pixel height instead of 8.
- Skeleton's spine is now 2 pixels width to be symmetric. The skull seems too big now, but I can live with it.

### 2023.11.29 - Build 2
- The first map appears.

### 2023.11.14
- Name changed to Frank'N'Stein Refurbished.

### 2023.10.29
- Top platform is changed to metal as in original.

### 2023.07.28
- Added background bricks.
- Added lightbulb, bookshelf and life number picture frame.
- Added top platform made from half height stones.
- All these things appear when a save slot is selected.

### 2023.07.27
- Added '!' and '?' chars to font.

### 2023.07.26
- Added slot selector with animated Professor.
- Added progress and last played date to slot selector.

### 2023.07.25
- Added VMU code.

### 2023.07.21
- Added controller support.
- Updated start screen with authors and used tools.

### 2023.07.18
- Didn't like font, ripped original from Re-Booted. Modified it a bit by filling
  the holes with light grey pixels.
- Logo screen added.

### 2023.07.17
- Created logo.

### 2023.07.14
- Decided game resolution. (256*192 as original)
- Created font.


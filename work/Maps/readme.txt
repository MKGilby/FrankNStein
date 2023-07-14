
  Frank N Stein Resurrected - Map Converter tool Readme.txt - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of Frank N Stein Resurrected.

  Frank N Stein Resurrected is free software: you can redistribute it
  and/or modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  Frank N Stein Resurrected is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  Frank N Stein Resurrected. If not, see <https://www.gnu.org/licenses/>.



About maps
----------

Game maps are present in human readable format in the folder 'old' and are
checked and converted to binary format into the folder 'new' by MapConverter.
It also collects the binary files into one file to use in the game.

Use 'run.bat' to build 'maps.bin' into the 'data' directory. 


MPT format
----------

The file is a text file, first line is always MAPT.

The rest of the lines always start with a two char type identifier:

  WI - Width of the map in blocks, in this game it's 32.
  HE - Height of the map in blocks, in this game it's 22.
  AU - Author of the map.
  MN - Map name.
  GM - The game the map is made for.
  MP - One map line, there should be HE count of these lines.
       If the line doesn't contain enough chars for the entire map line, it
       is filled with spaces (empty blocks).
       (meaning of characters is explained later)
  EX - Extra data. There can be more than one EX line, the meaning of each line:
       - 1st line: The index of wall block to use to draw walls.
       - 2nd-6th line: Monster data (explained later)
       - rest: Decoration data (explained later)
  XX - End of the map file. Lines after this won't be processed.

Characters in MP lines:
  [=] - Metal floor at the top of the map.
  -   - Wall.
  1-7 - Skeleton pieces in order of collecting.
  :   - Spring. Allows to jump.
  \>  - Stairs to the right.
  </  - Stairs to the left.
  ^|v - Sliding pole. Allows to slide down.
  ()  - Ice. Can't stop on it.
  {$} - Mud. Slows down.
  !   - Zapper. Zaps the prof, but reduces the charge in the animating device,
        effectively giving some more time to collect the skeleton pieces.
  J   - Teleport. Teleports you two blocks to the direction you are facing.

Monster data in EX lines:
  There's either six number specifying monster data separated by commas
  or one dot signing no monster data. The six numbers are:
    row   - Map row of the monster.
    left  - Map column where the monster will turn back when moving to the left.
    right - Map column where the monster will turn back when moving to the right.
    start - Start column of the monster.
    speed - Speed and start direction of the monster.
            The bigger is abs(speed) the bigger is the speed.
            If speed<0 then the monster will start moving to the left,
            if speed>0 then the monster will start moving to the right.
            If speed=0 then the monster will stand still.
    image - Animation index of the monster.

Decoration data in EX lines:
  There are three values in each line, separated by commas:
    type - Decoration type. Currently there are two types: window, door.
    left - Left of the decoration in blocks.
    top  - Top of the decoration in blocks.


BIN format
----------

The following variable types are used:
            Name           Length(byte)  Description
  string:   stringlength   1             Length of the following string data
            stringdata     *             The actual string
  byte:     data           1
  coord:    left           1             In blocks (0..31)
            top            1             In blocks (0..21)
  bits(n):  data           n             Contents described in content description
  coordlen: left           1             In blocks (0..31)
            top            1             In blocks (0..21)
            length         1             In blocks (1..32)

BIN file content:
  Name         Type     Description
  FourCC                Contains 'FNSR'
  Author       string
  MapName      string
  TypeAndWall  bits(1)  Map type (t) and wall image index (w). Map type can be:
                          00 - Constructing (contains piece data)
                          01 - Interim original (doesn't contains piece data)
                          10 - Interim re-booted (doesn't contains piece data)
                          11 - Congratulations (doesn't contains piece data)
                        7             0
                        |             |
                        t t w w w w w w
  WallCount    byte     Count of wall data (can be 0)
  <
    WallData   coordlen
  > WallCount  times
  BlockCount1  byte     Count of simple blocks (spring, zapper, jumper, ice, mud) (can be 0)
  <
    BlockType  byte     1-spring, 2-ice, 3-zapper, 4-mud, 5-jumper
    Position   coord
  > BlockCount1 times
  BlockCount2  byte     Count of advanced blocks (stairsL/R, pole) (can be 0)
  <
    BlockType  byte     6-pole, 7-stairsR, 8-stairsL
    BlockData  coordlen
  > BlockCount2 times
  [                     Only exists if map type is Constructing
    Head       coord
    TorsoL     coord
    TorsoR     coord
    HipL       coord
    HipR       coord
    LegL       coord
    LegR       coord
  ]
  MonsterCount byte     Count of monster data (can be 0)
  <
    Speed      bits(1)  Direction(d) and speed(s). Values can be:
                          d=0 - Starts moving to the left.
                          d=1 - Starts moving to the right.
                          s=0..4 - 0-Stand still, 1->4 slowest to fastest.
                        7             0
                        |             |
                        0 0 0 0 d s s s
    StartPos   coord
    Left       byte
    Right      byte
    AnimIndex  byte     Animation index
  > MonsterCount times
  DecorCount   byte     Count of decoration data (can be 0)
  <
    Type       byte     0-window, 1-door
    Position   coord
  > DecorCount times

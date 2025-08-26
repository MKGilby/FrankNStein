{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSShared;

{$mode Delphi}{$H+}

interface

uses MediaManagerUnit, sdl2, FNSVMU, FNSSpring, FNSJsonMap;

const
  LOGICALWINDOWWIDTH=256;
  LOGICALWINDOWHEIGHT=192;
  WINDOWWIDTH=LOGICALWINDOWWIDTH*4;
  WINDOWHEIGHT=LOGICALWINDOWHEIGHT*4;
  DEFAULTCOLORS:array[0..2,0..2] of byte=(
     (24,36,48),   // Main background color
     (48,24,36),   // Inactive slot background color
     (64,36,54)    // Active slot background color
  );
  MAPWIDTHINBLOCKS=32;
  MAPHEIGHTINBLOCKS=22;
  MAPTYPECONSTRUCTING=0;
  MAPTYPEINTERIMORIGINAL=1;
  MAPTYPEINTERIMREBOOTED=2;
  MAPTYPECONGRATULATIONS=3;

  TILE_EMPTY=0;
  TILE_WALL=1;
  TILE_SPRING=2;
  TILE_ICE=3;
  TILE_ZAPPER=4;
  TILE_MUD=5;
  TILE_JUMPER=6;
  TILE_POLE=7;
  TILE_PIECE=128;

  // Split the game loop elapsed time into this tiny fragments (in seconds).
  // Anything longer than this lets the player fall through walls.
  MAXTIMESLICE=1/64;
  // If one game loop uses more than this time, consider it lag and skip it.
  MINLAG=1;

  RES_TERMINATE=-1;
  RES_NONE=0;
  RES_SUCCESS=1;


var
  MM:TMediaManager;
  Controller:PSDL_GameController;
  VMU:TVMU;
  MapList:TMapList;
  Springs:TSprings;

procedure LoadAssets;
procedure FreeAssets;

implementation

uses Logger;

procedure LoadFont(name:string;r,g,b:integer);
begin
  MM.Load('cs_font.png',Name,MM_DONTKEEPIMAGE);
  MM.Fonts[Name].SpaceSpace:=7;
  MM.Fonts[Name].SetRecolorExcludeChars(#128#129#130#131);
  MM.Fonts[Name].SetColor(r,g,b);
end;

procedure LoadAssets;
begin
  Log.LogStatus('Loading assets...');
  MM:=TMediaManager.Create;
  Log.LogStatus('  Fonts...');
  LoadFont('White',255,255,255);
  LoadFont('Blue',40,128,240);
  LoadFont('Green',40,240,128);
  LoadFont('Yellow',240,128,40);
  LoadFont('Pink',240,40,128);
  LoadFont('Purple',128,40,240);
  LoadFont('Lime',128,240,40);
  Log.LogStatus('  Logo...');
  MM.Load('logo.png','Logo',MM_CREATETEXTUREONLY);
  MM.Load('spectrum.png','Speccy',MM_CREATETEXTUREONLY);
  Log.LogStatus('  Sprites...');
  MM.Load('sprites.png','Sprites');
  MM.Load('msprites.png','MaskedSprites',MM_CREATEMASKFORANIMATIONFRAMES);
  Log.LogStatus('  Decorations...');
  MM.Load('backwall.png','Stones');
  MM.Load('decorations.png','Decorations');
  MM.Load('device.png','Device');
  MM.Load('meter.png','Meter');
  MM.Load('tiles.png','Tiles');
  Log.LogStatus('  Music...');
  MM.LoadMusic('music\rb_theme.mo3','Main');
  Log.LogStatus('  Maps metadata...');
  MapList:=TMapList.Create;
  MapList.Load;
  Log.LogStatus('Loading VMU...');
  VMU:=TVMU.Create;
  VMU.MapCount:=MapList.Count;
  Log.LogStatus('Creating common classes...');
  Springs:=TSprings.Create;
end;

procedure FreeAssets;
begin
  Log.LogStatus('Common classes...');
  if Assigned(Springs) then Springs.Free;
  Log.LogStatus('Freeing VMU...');
  if Assigned(VMU) then VMU.Free;
  Log.LogStatus('Freeing assets...');
  if Assigned(MapList) then MapList.Free;
  if Assigned(MM) then MM.Free;
end;



end.


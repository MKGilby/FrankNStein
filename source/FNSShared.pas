{
  Frank N Stein Refurbished - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of Frank N Stein Refurbished.

  Frank N Stein Refurbished is free software: you can redistribute it
  and/or modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  Frank N Stein Refurbished is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  Frank N Stein Refurbished. If not, see <https://www.gnu.org/licenses/>.
}

unit FNSShared;

{$mode ObjFPC}{$H+}

interface

uses MediaManagerUnit, sdl2, FNSVMU, FNSMap;

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
  MAXSLOTS=3;
  MAPTYPECONSTRUCTING=0;
  MAPTYPEINTERIMORIGINAL=1;
  MAPTYPEINTERIMREBOOTED=2;
  MAPTYPECONGRATULATIONS=3;

var
  MM:TMediaManager;
  Controller:PSDL_GameController;
  VMU:TVMU;
  Maps:TMapList;

procedure LoadAssets;
procedure FreeAssets;

implementation

uses Logger;

procedure LoadFont(name:string;r,g,b:integer);
begin
  MM.Load('cs_font.png',Name);
  MM.Fonts[Name].SpaceSpace:=7;
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
  MM.Load('logo.png','Logo');
  Log.LogStatus('  Sprites...');
  MM.Load('sprites.png','Sprites');
  Log.LogStatus('  Decorations...');
  MM.Load('backwall.png','Stones');
  MM.Load('decorations.png','Decorations');
  MM.Load('device.png','Device');
  MM.Load('meter.png','Meter');
  Log.LogStatus('  Music...');
  MM.LoadMusic('music\rb_theme.mo3','Main');
  Log.LogStatus('  Maps...');
  Maps:=TMapList.Create('maps.bin');
  Log.LogStatus('Loading VMU...');
  VMU:=TVMU.Create;
  VMU.MapCount:=Maps.Count;
end;

procedure FreeAssets;
begin
  Log.LogStatus('Freeing VMU...');
  if Assigned(VMU) then VMU.Free;
  Log.LogStatus('Freeing assets...');
  if Assigned(Maps) then Maps.Free;
  if Assigned(MM) then MM.Free;
end;



end.


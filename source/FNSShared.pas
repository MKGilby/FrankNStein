{
  Frank N Stein Resurrected - Copyright 2023 MKSZTSZ
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
}

unit FNSShared;

{$mode ObjFPC}{$H+}

interface

uses MediaManagerUnit, sdl2;

const
  LOGICALWINDOWWIDTH=256;
  LOGICALWINDOWHEIGHT=192;
  WINDOWWIDTH=LOGICALWINDOWWIDTH*4;
  WINDOWHEIGHT=LOGICALWINDOWHEIGHT*4;

var
  MM:TMediaManager;
  Controller:PSDL_GameController;

procedure LoadAssets;
procedure FreeAssets;

implementation

procedure LoadFont(name:string;r,g,b:integer);
begin
  MM.Load('cs_font.png',Name);
  MM.Fonts[Name].SpaceSpace:=7;
  MM.Fonts[Name].SetColor(r,g,b);
end;

procedure LoadAssets;
begin
  MM:=TMediaManager.Create;
  LoadFont('White',255,255,255);
  LoadFont('Blue',40,128,240);
  LoadFont('Green',40,240,128);
  LoadFont('Yellow',240,128,40);
  LoadFont('Pink',240,40,128);
  LoadFont('Purple',128,40,240);
  MM.Load('logo.png','Logo',MM_CREATETEXTUREWHENNOANIMATIONDATA);
  MM.Load('resurrected.png','LogoRes',MM_CREATETEXTUREWHENNOANIMATIONDATA);
  MM.Load('sprites.png','Sprites');
  MM.LoadMusic('music\rb_theme.mo3','Main');
end;

procedure FreeAssets;
begin
  MM.Free;
end;



end.


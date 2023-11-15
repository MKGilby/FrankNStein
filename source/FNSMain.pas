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

unit FNSMain;

{$mode ObjFPC}{$H+}
{$define LimitFPS}

interface

uses
  SysUtils, mk_sdl2, FNSStartScreen, FNSSlotSelector, FNSMapSelect;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fMainWindow:TWindow;
    fStartScreen:TStartScreen;
    fSlotSelector:TSlotSelector;
    fMapSelect:TMapSelect;
  end;

implementation

uses sdl2, MKToolbox, Logger, MKStream, FNSShared, MAD4MidLevelUnit, MKAudio,
  Animation2Unit;

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
{$ifndef DEBUG}var MAD4:TMAD4MidLevel;{$endif}
begin
  randomize;
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
  MKStreamOpener.AddDirectory('..\data',0);
{$ELSE}
  // Set logging level
  Log.SetLogLevel(llStatus);
  MKStreamOpener.AddDirectory('.',0);
  MAD4:=TMAD4MidLevel.Create(ExtractFilePath(paramstr(0))+'\FrankNStein.data');
  MKStreamOpener.AddOtherSource(MAD4,100);
{$ENDIF}
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_GAMECONTROLLER);

  fMainWindow:=TWindow.CreateCustomSized(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    LOGICALWINDOWWIDTH,
    LOGICALWINDOWHEIGHT,
    Format('Frank N Stein Refurbished V%s (%s)',[iVersion,replace(iBuildDate,'/','.')]));

  Controller:=FindController;

  Init_Audio;

  SetFPS(60);

  LoadAssets;

  fStartScreen:=TStartScreen.Create;
  fSlotSelector:=TSlotSelector.Create;
  fMapSelect:=TMapSelect.Create(0);
end;

destructor TMain.Destroy;
begin
  if Assigned(fMapSelect) then fMapSelect.Free;
  if Assigned(fSlotSelector) then fSlotSelector.Free;
  if Assigned(fStartScreen) then fStartScreen.Free;
  FreeAssets;
  if Assigned(fMainWindow) then fMainWindow.Free;
  inherited Destroy;
end;

procedure TMain.Run;
var res:integer;
begin
  VMU.SetSlotUsed(0);
//  MM.Musics.ItemByName['Main']._music.Play;
  repeat
    res:=fStartScreen.Run;
    if res>-1 then
      repeat
        res:=fSlotSelector.Run;
        if res>-1 then fMapSelect.Run;
      until res=-1;
  until res=-1;
//  MM.Musics.ItemByName['Main']._music.Stop;
end;

end.


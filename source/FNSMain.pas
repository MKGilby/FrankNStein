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

unit FNSMain;

{$mode ObjFPC}{$H+}
{$define LimitFPS}

interface

uses
  SysUtils, mk_sdl2;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fMainWindow:TWindow;
  end;

implementation

uses sdl2, MKToolbox, Logger, MKStream, FNSShared;

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
begin
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
{$ELSE}
  // Set logging level
  Log.SetLogLevel(llStatus);
{$ENDIF}

  MKStreamOpener.AddDirectory('.',0);
  fMainWindow:=TWindow.CreateDoubleSized(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    Format('Frank N Stein Resurrected V%s (%s)',[iVersion,replace(iBuildDate,'/','.')]));

  SetFPS(60);

end;

destructor TMain.Destroy;
begin
  if Assigned(fMainWindow) then fMainWindow.Free;
  inherited Destroy;
end;

procedure TMain.Run;
var quit:boolean;
begin
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    HandleMessages;
    quit:=Terminate;
    if keys[SDL_SCANCODE_ESCAPE] then quit:=true;
  until quit;
end;

end.


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

unit FNSMapSelect;

{$mode ObjFPC}{$H+}
{$define LimitFPS}

interface

uses FNSMapStatic;

type

  { TMapSelect }

  TMapSelect=class
    constructor Create(iStartMapNo:integer);
    destructor Destroy; override;
    function Run:integer;
  private
    fCurrentMapNo:integer;
    fMapStatic:TMapStatic;
  end;

implementation

uses mk_sdl2, sdl2;

{ TMapSelect }

constructor TMapSelect.Create(iStartMapNo:integer);
begin
  fCurrentMapNo:=iStartMapNo;
  fMapStatic:=TMapStatic.Create(fCurrentMapNo);
end;

destructor TMapSelect.Destroy;
begin
  if Assigned(fMapStatic) then fMapStatic.Free;
  inherited Destroy;
end;

function TMapSelect.Run:integer;
begin
  Result:=0;
  ClearKeys;
  repeat
    fMapStatic.Draw;

    {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    HandleMessages;
    if keys[SDL_SCANCODE_ESCAPE] then Result:=-1;
    if keys[SDL_SCANCODE_RETURN] or keys[SDL_SCANCODE_SPACE] then Result:=1;
    if controllerbuttons[SDL_CONTROLLER_BUTTON_A] then Result:=1;
    if controllerbuttons[SDL_CONTROLLER_BUTTON_B] then Result:=-1;
    if Terminate then Result:=-1;
  until Result<>0;
  ClearControllerButtons;
end;

end.


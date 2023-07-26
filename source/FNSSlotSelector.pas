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

unit FNSSlotSelector;

{$mode ObjFPC}{$H+}
{$define LimitFPS}

interface

uses
  SysUtils, Animation2Unit;

type

  { TSlot }

  TSlot=class
    constructor Create(iSlot,iLeft:integer);
    destructor Destroy; override;
    procedure Draw;
  private
    fProf:TAnimation;
    fSlot:integer;
    fLeft:integer;
    fCompletedMapCount:integer;
    fIsUsed:boolean;
    fLastDate,fLastTime:string;
  public
    Active:boolean;
  end;

  { TSlotSelector }

  TSlotSelector=class
    constructor Create;
    destructor Destroy; override;
    function Run:integer;
  private
    fSlots:array[0..2] of TSlot;
  end;

implementation

uses FNSShared, mk_sdl2, sdl2;

const
  SLOTWIDTH=76;
  SLOTSPACE=(LOGICALWINDOWWIDTH-(SLOTWIDTH*3)) div 4;
  SLOTTOP=44+16;
  SLOTHEIGHT=128;
  SLOTLINEHEIGHT=8;
  SLOTMARGIN=6;

{ TSlot }

constructor TSlot.Create(iSlot, iLeft: integer);
begin
  fSlot:=iSlot;
  fLeft:=iLeft;
  fProf:=MM.Animations.ItemByName['ProfRight'].SpawnAnimation;
  fIsUsed:=VMU.IsSlotUsed(fSlot);
  if fIsUsed then begin
    fLastDate:=VMU.GetSlotLastUseDate(fSlot);
    fLastTime:=VMU.GetSlotLastUseTime(fSlot);
  end else begin
    fLastDate:='';
    fLastTime:='';
  end;
  fCompletedMapCount:=VMU.GetCompletedMapCount(fSlot);
end;

destructor TSlot.Destroy;
begin
  if Assigned(fProf) then fProf.Free;
  inherited Destroy;
end;

procedure TSlot.Draw;

  procedure Print(pFont,pText:string;pX,pLine,pAlign:integer); inline;
  begin
    MM.Fonts[pFont].OutText(pText,fLeft+pX,SLOTTOP+SLOTMARGIN+SLOTLINEHEIGHT*pLine,pAlign);
  end;

begin
  Rectangle(fLeft,SLOTTOP,SLOTWIDTH,SLOTHEIGHT,0,0,0);
  if Active then begin
    Rectangle(fLeft+1,SLOTTOP+1,SLOTWIDTH-2,SLOTHEIGHT-2,128,128,128);
    Bar(fLeft+2,SLOTTOP+2,SLOTWIDTH-4,SLOTHEIGHT-4,DEFAULTCOLORS[2,0],DEFAULTCOLORS[2,1],DEFAULTCOLORS[2,2]);
  end else begin
    Rectangle(fLeft+1,SLOTTOP+1,SLOTWIDTH-2,SLOTHEIGHT-2,64,64,64);
    Bar(fLeft+2,SLOTTOP+2,SLOTWIDTH-4,SLOTHEIGHT-4,DEFAULTCOLORS[1,0],DEFAULTCOLORS[1,1],DEFAULTCOLORS[1,2]);
  end;
  Print('Blue',Format('SLOT %d',[fSlot+1]),SLOTWIDTH div 2,0,1);
  if fIsUsed then begin
    Print('Purple','MAPS:',SLOTMARGIN,2,0);
    // We write mapcount-1 because the congratulations map is not counted.
    Print('Yellow',Format('%d/%d',[fCompletedMapCount,Maps.Count-1]),SLOTWIDTH-SLOTMARGIN,3,2);
    Print('Purple','LAST',SLOTMARGIN,5,0);
    Print('Purple','PLAYED:',SLOTWIDTH-SLOTMARGIN,6,2);
    Print('Yellow',fLastDate,SLOTWIDTH div 2,7,1);
    Print('Yellow',fLastTime,SLOTWIDTH div 2,8,1);
    fProf.PutFrame(fLeft+SLOTWIDTH div 2-5,SLOTTOP+SLOTHEIGHT-6-16);
    if Active then fProf.Animate;
  end else begin
    Print('Pink','EMPTY',SLOTWIDTH div 2,7,1);
  end;
end;

{ TSlotSelector }

constructor TSlotSelector.Create;
var i:integer;
begin
  for i:=0 to 2 do fSlots[i]:=TSlot.Create(i,SLOTSPACE*(i+1)+SLOTWIDTH*i);
end;

destructor TSlotSelector.Destroy;
var i:integer;
begin
  for i:=0 to 2 do
    if Assigned(fSlots[i]) then fSlots[i].Free;
  inherited Destroy;
end;

function TSlotSelector.Run:integer;
var i:integer;
begin
  Result:=0;
  fSlots[Result].Active:=true;
  ClearKeys;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,DEFAULTCOLORS[0,0],DEFAULTCOLORS[0,1],DEFAULTCOLORS[0,2],255);
    SDL_RenderClear(PrimaryWindow.Renderer);

//    MM.Fonts['White'].OutText('SELECT SAVE SLOT!',LOGICALWINDOWWIDTH div 2,178,1);
    MM.Fonts['White'].OutText('SELECT SAVE SLOT!',LOGICALWINDOWWIDTH div 2,44,1);
    PutTexture(57,8,MM.Textures.ItemByName['Logo']);
    PutTexture(155,28,MM.Textures.ItemByName['LogoRes']);
    for i:=0 to 2 do fSlots[i].Draw;
    Flip;
    HandleMessages;
    if (keys[SDL_SCANCODE_LEFT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT])
        and (Result>0) then begin
      fSlots[Result].Active:=false;
      dec(Result);
      fSlots[Result].Active:=true;
      keys[SDL_SCANCODE_LEFT]:=false;
      controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT]:=false;
    end;
    if (keys[SDL_SCANCODE_RIGHT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT])
        and (Result<MAXSLOTS-1) then begin
      fSlots[Result].Active:=false;
      inc(Result);
      fSlots[Result].Active:=true;
      keys[SDL_SCANCODE_RIGHT]:=false;
      controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT]:=false;
    end;
  until keys[SDL_SCANCODE_RETURN] or
        keys[SDL_SCANCODE_SPACE] or
        keys[SDL_SCANCODE_ESCAPE] or
        controllerbuttons[SDL_CONTROLLER_BUTTON_A] or
        controllerbuttons[SDL_CONTROLLER_BUTTON_B];
  if controllerbuttons[SDL_CONTROLLER_BUTTON_B] or keys[SDL_SCANCODE_ESCAPE] then Result:=-1;
  ClearControllerButtons;
end;

end.


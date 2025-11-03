{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSSlotSelector;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Animation2Unit;

type

  { TSlot }

  TSlot=class
    constructor Create(iSlot,iLeft:integer);
    destructor Destroy; override;
    procedure Draw;
    procedure UpdateData;
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

uses FNSShared, FNSVMU, mk_sdl2, sdl2, MKToolbox;

const
  SLOTWIDTH=76;
  SLOTSPACE=(LOGICALWINDOWWIDTH-(SLOTWIDTH*3)) div 4;
  SLOTTOP=42+16;
  SLOTHEIGHT=112;
  SLOTLINEHEIGHT=8;
  SLOTMARGIN=6;

{ TSlot }

constructor TSlot.Create(iSlot, iLeft: integer);
begin
  fSlot:=iSlot;
  fLeft:=iLeft;
  fProf:=MM.Animations.ItemByName['ProfRight'].SpawnAnimation;
  UpdateData;
end;

destructor TSlot.Destroy;
begin
  if Assigned(fProf) then fProf.Free;
  inherited Destroy;
end;

procedure TSlot.Draw;

  procedure Print(pFont,pText:string;pX,pLine,pAlign:integer);
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
    Print('White','MAPS:',SLOTMARGIN,2,0);
    Print('Yellow',Format('%d/%d',[fCompletedMapCount,MapList.Count]),SLOTWIDTH-SLOTMARGIN,3,2);
    Print('White','LAST',SLOTMARGIN,5,0);
    Print('White','PLAYED:',SLOTWIDTH-SLOTMARGIN,6,2);
    Print('Yellow',fLastDate,SLOTWIDTH div 2,7,1);
    Print('Yellow',fLastTime,SLOTWIDTH div 2,8,1);
    fProf.PutFrame(fLeft+SLOTWIDTH div 2-5,SLOTTOP+SLOTHEIGHT-6-16);
    if Active then fProf.Animate;
  end else begin
    Print('Pink','EMPTY',SLOTWIDTH div 2,7,1);
  end;
end;

procedure TSlot.UpdateData;
begin
  fIsUsed:=VMU.Slots[fSlot].IsUsed;
  if fIsUsed then begin
    fLastDate:=StringReplace(DateToStr(VMU.Slots[fSlot].LastUsed,FS),'.','_',[rfReplaceAll]);
    fLastTime:=TimeToStr(VMU.Slots[fSlot].LastUsed,FS);
  end else begin
    fLastDate:='';
    fLastTime:='';
  end;
  fCompletedMapCount:=VMU.Slots[fSlot].CompletedMapCount;
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
var i,sl:integer;
begin
  Result:=RES_NONE;
  sl:=VMU.Config.LastUsedSlot;
  fSlots[sl].Active:=true;
  ClearKeys;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,DEFAULTCOLORS[0,0],DEFAULTCOLORS[0,1],DEFAULTCOLORS[0,2],255);
    SDL_RenderClear(PrimaryWindow.Renderer);

    PutTexture(256-24,192-48,MM.Textures.ItemByName['Speccy']);

    MM.Fonts['White'].OutText('SELECT SAVE SLOT',LOGICALWINDOWWIDTH div 2,42,1);
    if not Assigned(Controller) then begin
      MM.Fonts['Purple'].OutText('USE '#130' AND '#131' TO SELECT SLOT',LOGICALWINDOWWIDTH div 2,LOGICALWINDOWHEIGHT-20,1);
      MM.Fonts['Purple'].OutText('PRESS '#128' TO CONTINUE',LOGICALWINDOWWIDTH div 2,LOGICALWINDOWHEIGHT-10,1);
    end else begin
      MM.Fonts.OutText(#5'USE '#6'ARROWS'#5' TO SELECT SLOT',LOGICALWINDOWWIDTH div 2,LOGICALWINDOWHEIGHT-20,1);
      MM.Fonts.OutText(#5'PRESS '#6'SPACE'#5' TO CONTINUE',LOGICALWINDOWWIDTH div 2,LOGICALWINDOWHEIGHT-10,1);
    end;
    PutTexture(57,8,MM.Textures.ItemByName['Logo']);
    for i:=0 to MAXSLOTS-1 do fSlots[i].Draw;
    FlipNoLimit;
    HandleMessages;
    if (keys[SDL_SCANCODE_LEFT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT])
        and (sl>0) then begin
      fSlots[sl].Active:=false;
      dec(sl);
      fSlots[sl].Active:=true;
      keys[SDL_SCANCODE_LEFT]:=false;
      controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT]:=false;
    end;
    if (keys[SDL_SCANCODE_RIGHT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT])
        and (sl<MAXSLOTS-1) then begin
      fSlots[sl].Active:=false;
      inc(sl);
      fSlots[sl].Active:=true;
      keys[SDL_SCANCODE_RIGHT]:=false;
      controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT]:=false;
    end;
    if keys[SDL_SCANCODE_RETURN] or keys[SDL_SCANCODE_SPACE] or
       controllerbuttons[SDL_CONTROLLER_BUTTON_A] then begin
      VMU.Config.LastUsedSlot:=sl;
      Result:=RES_SUCCESS;
    end;
    if keys[SDL_SCANCODE_ESCAPE] or Terminate or
       controllerbuttons[SDL_CONTROLLER_BUTTON_B] then Result:=RES_TERMINATE;
  until Result<>RES_NONE;
  ClearControllerButtons;
end;

end.


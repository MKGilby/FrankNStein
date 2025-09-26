{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSMapSelect;

{$mode Delphi}{$H+}
{define LimitFPS}

interface

uses SysUtils, FNSPlay1Map, mk_sdl2;

type

  { TMapSelect }

  TMapSelect=class
    constructor Create(iStartMapNo:integer);
    destructor Destroy; override;
    function Run:integer;
  private
    fCurrentMapNo:integer;
    fPlay1Map:TPlay1Map;
    fDarkLayer:TStaticTexture;
  end;

implementation

uses sdl2, FNSShared, MKToolbox, ARGBImageUnit;

{ TMapSelect }

constructor TMapSelect.Create(iStartMapNo:integer);
var tmp:TARGBImage;
begin
  fCurrentMapNo:=iStartMapNo;
  fPlay1Map:=TPlay1Map.Create(fCurrentMapNo);
  tmp:=TARGBImage.Create(LOGICALWINDOWWIDTH,LOGICALWINDOWHEIGHT-24);
  try
    tmp.Clear($80000000);
    fDarkLayer:=TStaticTexture.Create(tmp);
  finally
    tmp.Free;
  end;
end;

destructor TMapSelect.Destroy;
begin
  fDarkLayer.Free;
  fPlay1Map.Free;
  inherited Destroy;
end;

function TMapSelect.Run:integer;
begin
  Result:=RES_NONE;
  ClearKeys;
  ClearControllerButtons;
  repeat
    fPlay1Map.Draw;
    PutTexture(0,0,fDarkLayer);
    bar(0,LOGICALWINDOWHEIGHT-24,LOGICALWINDOWWIDTH,24,0,0,0);
    PutTexturePart(32,80,0,0,LOGICALWINDOWWIDTH-32*2,32,fDarkLayer);
    MM.Fonts['White'].OutText(MapList.MapNames[fCurrentMapNo],128,88,1);
    MM.Fonts.OutText(#1'BY '#3+MapList.Authors[fCurrentMapNo],128,96,1);
    if fCurrentMapNo>0 then begin
      PutTexturePart(4,88,0,0,16,16,fDarkLayer);
      MM.Fonts.OutText(#130,8,92,0);
    end;
    if fCurrentMapNo<MapList.Count-1 then begin
      PutTexturePart(236,88,0,0,16,16,fDarkLayer);
      MM.Fonts.OutText(#131,240,92,0);
    end;
    if Assigned(Controller) then begin
      MM.Fonts['Purple'].OutText(#128' PLAY',0,184,0);
      MM.Fonts['Purple'].OutText(#129' BACK',LOGICALWINDOWWIDTH,184,2);
    end else begin
      MM.Fonts.OutText(#5'USE '#6'ARROWS'#5' TO SELECT MAP',LOGICALWINDOWWIDTH div 2,LOGICALWINDOWHEIGHT-24,1);
      MM.Fonts.OutText(#5'PRESS '#6'SPACE'#5' TO CONTINUE',LOGICALWINDOWWIDTH div 2,LOGICALWINDOWHEIGHT-16,1);
      MM.Fonts.OutText(#6'ESCAPE'#5' TO GO BACK',LOGICALWINDOWWIDTH div 2,LOGICALWINDOWHEIGHT-8,1);
    end;

    {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    HandleMessages;
    if (controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT] or keys[SDL_SCANCODE_LEFT])
       and (fCurrentMapNo>0) then begin
      fPlay1Map.Free;
      dec(fCurrentMapNo);
      fPlay1Map:=TPlay1Map.Create(fCurrentMapNo);
      keys[SDL_SCANCODE_LEFT]:=false;
      controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT]:=false;
    end;
    if (controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT] or keys[SDL_SCANCODE_RIGHT])
       and (fCurrentMapNo<MapList.Count-1) then begin
      fPlay1Map.Free;
      inc(fCurrentMapNo);
      fPlay1Map:=TPlay1Map.Create(fCurrentMapNo);
      keys[SDL_SCANCODE_RIGHT]:=false;
      controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT]:=false;
    end;
    if keys[SDL_SCANCODE_RETURN] or keys[SDL_SCANCODE_SPACE] or
       controllerbuttons[SDL_CONTROLLER_BUTTON_A] then Result:=fCurrentMapNo+1;
    if keys[SDL_SCANCODE_ESCAPE] or
       controllerbuttons[SDL_CONTROLLER_BUTTON_B] then Result:=RES_BACK;
    if Terminate then Result:=RES_TERMINATE;
  until Result<>RES_NONE;
  ClearKeys;
  ClearControllerButtons;
end;

end.


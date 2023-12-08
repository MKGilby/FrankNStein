{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSStartScreen;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TStartScreen }

  TStartScreen=class
    function Run:integer;
  end;

implementation

uses SDL2, mk_sdl2, FNSShared;

{ TStartScreen }

function TStartScreen.Run: integer;
const TEXTTOP=110;
begin
  Result:=0;
  ClearKeys;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,DEFAULTCOLORS[0,0],DEFAULTCOLORS[0,1],DEFAULTCOLORS[0,2],255);
    SDL_RenderClear(PrimaryWindow.Renderer);

    PutTexture(57,8,MM.Textures.ItemByName['Logo']);
    PutTexture(256-24,192-48,MM.Textures.ItemByName['Speccy']);
    if GetTickCount64 mod 1000<500 then
      if Assigned(Controller) then
        MM.Fonts['White'].OutText('PRESS '#128' TO START',LOGICALWINDOWWIDTH div 2,64,1)
      else
        MM.Fonts['White'].OutText('PRESS SPACE TO START',LOGICALWINDOWWIDTH div 2,64,1);
    MM.Fonts['Blue'].OutText('FRANK N. STEIN RE-BOOTED',LOGICALWINDOWWIDTH div 2,TEXTTOP,1);
    MM.Fonts['Blue'].OutText('@1984-2011 COLIN STEWART',LOGICALWINDOWWIDTH div 2,TEXTTOP+10,1);
    MM.Fonts['Pink'].OutText('THIS REMAKE @2023 MKSZTSZ',LOGICALWINDOWWIDTH div 2,TEXTTOP+22,1);
    MM.Fonts['Yellow'].OutText('MUSIC AND SOUND - MIKE FRALEY',LOGICALWINDOWWIDTH div 2,TEXTTOP+34,1);
    MM.Fonts['Yellow'].OutText('GFX AND CODE - GILBY',LOGICALWINDOWWIDTH div 2,TEXTTOP+44,1);
    MM.Fonts['Purple'].OutText('DEVELOPED USING SDL2, BASS',LOGICALWINDOWWIDTH div 2,TEXTTOP+56,1);
    MM.Fonts['Purple'].OutText('AND LAZARUS 3.0 RC 2',LOGICALWINDOWWIDTH div 2,TEXTTOP+66,1);

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


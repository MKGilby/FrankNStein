unit FNSStartScreen;

{$mode ObjFPC}{$H+}
{$define LimitFPS}

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
var quit:boolean;
begin
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,24,36,48,255);
    SDL_RenderClear(PrimaryWindow.Renderer);

    PutTexture(57,8,MM.Textures.ItemByName['Logo']);
    PutTexture(155,28,MM.Textures.ItemByName['LogoRes']);
    if GetTickCount64 mod 1000<500 then
      MM.Fonts['White'].OutText('PRESS START',LOGICALWINDOWWIDTH div 2,64,1);
    MM.Fonts['Blue'].OutText('FRANK N. STEIN RE-BOOTED',LOGICALWINDOWWIDTH div 2,TEXTTOP,1);
    MM.Fonts['Blue'].OutText('@1984-2011 COLIN STEWART',LOGICALWINDOWWIDTH div 2,TEXTTOP+10,1);
    MM.Fonts['Pink'].OutText('THIS REMAKE @2023 MKSZTSZ',LOGICALWINDOWWIDTH div 2,TEXTTOP+22,1);
    MM.Fonts['Yellow'].OutText('MUSIC AND SOUND - MIKE FRALEY',LOGICALWINDOWWIDTH div 2,TEXTTOP+34,1);
    MM.Fonts['Yellow'].OutText('GFX AND CODE - GILBY',LOGICALWINDOWWIDTH div 2,TEXTTOP+44,1);
    MM.Fonts['Purple'].OutText('DEVELOPED USING SDL2, BASS',LOGICALWINDOWWIDTH div 2,TEXTTOP+56,1);
    MM.Fonts['Purple'].OutText('AND LAZARUS 3.0 RC 1',LOGICALWINDOWWIDTH div 2,TEXTTOP+66,1);

    {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    HandleMessages;
    quit:=Terminate;
    if keys[SDL_SCANCODE_ESCAPE] then quit:=true;
    if (SDL_GameControllerGetButton(Controller, SDL_CONTROLLER_BUTTON_A))<>0 then quit:=true;
  until quit;
  Result:=0;
end;

end.


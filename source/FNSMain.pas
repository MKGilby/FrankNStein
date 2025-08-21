{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSMain;

{$mode Delphi}{$H+}

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
  SDL_SetHint(SDL_HINT_RENDER_VSYNC,'1');

  fMainWindow:=TWindow.CreateCustomSized(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    LOGICALWINDOWWIDTH,
    LOGICALWINDOWHEIGHT,
    Format('Frank N Stein Refurbished V%s (%s)',[iVersion,StringReplace(iBuildDate,'/','.',[rfReplaceAll])]));

  Controller:=FindController;

  Init_Audio;

  SetFPS(60);

  LoadAssets;

  fSlotSelector:=TSlotSelector.Create;
  fMapSelect:=TMapSelect.Create(1);
end;

destructor TMain.Destroy;
begin
  if Assigned(fMapSelect) then fMapSelect.Free;
  if Assigned(fSlotSelector) then fSlotSelector.Free;
  FreeAssets;
  if Assigned(fMainWindow) then fMainWindow.Free;
  inherited Destroy;
end;

procedure TMain.Run;
var res:integer;
begin
  VMU.SetSlotUsed(0);
  MM.Musics.ItemByName['Main']._music.Play;
  with TStartScreen.Create do try res:=Run; finally Free; end;
  if res=RES_SUCCESS then begin
    repeat
      res:=fSlotSelector.Run;
      if res=RES_SUCCESS then fMapSelect.Run;
    until res=RES_TERMINATE;
  end;
  MM.Musics.ItemByName['Main']._music.Stop;
end;

end.


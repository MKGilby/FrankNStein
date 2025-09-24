{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSMain;

{$mode Delphi}{$H+}

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

uses sdl2, MKToolbox, Logger, MKStream, FNSShared, MAD4MidLevelUnit, MKAudio,
  Animation2Unit, FNSStartScreen, FNSSlotSelector, FNSMapSelect, FNSPlay1Map;

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
end;

destructor TMain.Destroy;
begin
  FreeAssets;
  fMainWindow.Free;
  inherited Destroy;
end;

procedure TMain.Run;
var res:integer;
begin
//  MM.Musics.ItemByName['Main']._music.Play;
  with TStartScreen.Create do try res:=Run; finally Free; end;
  if res=RES_SUCCESS then begin
    repeat
      with TSlotSelector.Create do try res:=Run; finally Free; end;
      if res=RES_SUCCESS then begin
        VMU.Slots[VMU.Config.LastUsedSlot].IsUsed:=true;
        VMU.Slots[VMU.Config.LastUsedSlot].UpdateLastUsed;
        repeat
          with TMapSelect.Create(VMU.Slots[VMU.Config.LastUsedSlot].LastPlayedMap) do try res:=Run; finally Free; end;
          if res>RES_NONE then begin
            dec(res);
            VMU.Slots[VMU.Config.LastUsedSlot].LastPlayedMap:=res;
            with TPlay1Map.Create(res) do try res:=Run; finally Free; end;
            if res=RES_BACK then res:=RES_NONE;
          end;
        until (res=RES_TERMINATE) or (res=RES_BACK);
//        with TMapSelect.Create(VMU.Slots[VMU.Config.LastUsedSlot].) do try res:=Run;
      end;
    until res=RES_TERMINATE;
  end;
//  MM.Musics.ItemByName['Main']._music.Stop;
end;

end.


unit FNSMeter;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Animation2Unit, mk_sdl2;

type

  { TMeter }

  TMeter=class
    constructor Create;
    destructor Destroy; override;
    procedure Move(pTimeUsed:double);
    procedure Draw;
  private
    fFront:TAnimation;
    fGauge:TStreamingTexture;
    fCurrentValue:double;
  end;

implementation

uses FNSShared;

const
  METERLEFT=21*8;
  METERTOP=0;

{ TMeter }

constructor TMeter.Create;
begin
//  MM.Images.ItemByName['Meter'].WriteFile('test.tga','TGA');
  fFront:=MM.Animations.ItemByName['Meter'].SpawnAnimation;
//  fBack.LogData;
//  fFront.LogData;
  MM.Animations.ItemByName['Meter'].Animation.LogData;
  fGauge:=TStreamingTexture.Create(24,16);
  fGauge.ARGBImage.Clear(0);
  fGauge.Update;
  fCurrentValue:=0;
end;

destructor TMeter.Destroy;
begin
  fGauge.Free;
  fFront.Free;
  inherited Destroy;
end;

procedure TMeter.Move(pTimeUsed:double);
begin

end;

procedure TMeter.Draw;
begin
//  PutTexture(0,32,MM.Textures.ItemByName['Meter']);
//  PutTexture(METERLEFT,METERTOP,fGauge);
  fFront.PutFrame(METERLEFT,METERTOP);
end;

end.


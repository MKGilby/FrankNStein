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
    fBack,fFront:TAnimation;
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
  fBack:=MM.Animations.ItemByName['MeterBack'].SpawnAnimation;
  fFront:=MM.Animations.ItemByName['MeterFront'].SpawnAnimation;
//  fBack.LogData;
//  fFront.LogData;
  MM.Animations.ItemByName['MeterBack'].Animation.LogData;
  MM.Animations.ItemByName['MeterFront'].Animation.LogData;
  fGauge:=TStreamingTexture.Create(24,16);
  fGauge.ARGBImage.Clear(0);
  fGauge.Update;
  fCurrentValue:=0;
end;

destructor TMeter.Destroy;
begin
  fGauge.Free;
  fFront.Free;
  fBack.Free;
  inherited Destroy;
end;

procedure TMeter.Move(pTimeUsed:double);
begin

end;

procedure TMeter.Draw;
begin
//  PutTexture(0,32,MM.Textures.ItemByName['Meter']);
  fBack.PutFrame(METERLEFT,METERTOP);
//  PutTexture(METERLEFT,METERTOP,fGauge);
  fFront.PutFrame(METERLEFT,METERTOP);
end;

end.


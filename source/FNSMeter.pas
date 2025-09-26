unit FNSMeter;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Animation2Unit, Font2Unit, mk_sdl2;

type

  { TMeter }

  TMeter=class
    constructor Create;
    procedure Move(pTimeUsed:double);
    procedure Draw; virtual;abstract;
  protected
    fCurrentValue:double;
  end;

  { TOriginalMeter }

  TOriginalMeter=class(TMeter)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
  private
    fFront:TAnimation;
  end;

  { TRebootedMeter }

  TRebootedMeter=class(TMeter)
    constructor Create;
    procedure Draw; override;
  private
    fFont,fShadowFont:TFont;
  end;

implementation

uses FNSShared;

const
  METERLEFT=21*8;
  METERTOP=0;
  MAXTIME=180;

{ TMeter }

constructor TMeter.Create;
begin
  fCurrentValue:=MAXTIME;
end;

procedure TMeter.Move(pTimeUsed:double);
begin
  if fCurrentValue>0 then begin
    fCurrentValue:=fCurrentValue-pTimeUsed;
    if fCurrentValue<0 then fCurrentValue:=0;
  end;
end;


{ TOriginalMeter }

constructor TOriginalMeter.Create;
begin
  fFront:=MM.Animations.ItemByName['Meter'].SpawnAnimation;
end;

destructor TOriginalMeter.Destroy;
begin
  fFront.Free;
  inherited Destroy;
end;

procedure TOriginalMeter.Draw;
const PIRAD=pi/180;
  C1=32;
  C2=64;
var x,y:integer;d:double;
begin
  d:=fCurrentValue*88/180+136;
  x:=round(sin(d*PIRAD)*12);
  y:=round(cos(d*PIRAD)*10);
  Line(METERLEFT+12,METERTOP+14,METERLEFT+12+x,METERTOP+14+y,C2,C2,C2,255);
  Line(METERLEFT+11,METERTOP+14,METERLEFT+11+x,METERTOP+14+y,C1,C1,C1,255);
  Line(METERLEFT+12,METERTOP+13,METERLEFT+12+x,METERTOP+13+y,C2,C2,C2,255);
  Line(METERLEFT+11,METERTOP+13,METERLEFT+11+x,METERTOP+13+y,C1,C1,C1,255);
  fFront.PutFrame(METERLEFT,METERTOP);
end;

{ TRebootedMeter }

constructor TRebootedMeter.Create;
begin
  inherited Create;
  fFont:=MM.Fonts['Meter'];
  fShadowFont:=MM.Fonts['MeterShadow'];
end;

procedure TRebootedMeter.Draw;
var secs:integer;txt:String;
begin
  secs:=trunc(fCurrentValue);
  txt:=Format('%d:%.2d',[secs div 60,secs mod 60]);
  fShadowFont.OutText(txt,METERLEFT+12,METERTOP+5,1);
  fFont.OutText(txt,METERLEFT+11,METERTOP+4,1);
end;

end.


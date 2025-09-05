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
    fCurrentValue:double;
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
  fFront:=MM.Animations.ItemByName['Meter'].SpawnAnimation;
  MM.Animations.ItemByName['Meter'].Animation.LogData;
  fCurrentValue:=MAXTIME;
end;

destructor TMeter.Destroy;
begin
  fFront.Free;
  inherited Destroy;
end;

procedure TMeter.Move(pTimeUsed:double);
begin
  if fCurrentValue>0 then begin
    fCurrentValue:=fCurrentValue-pTimeUsed;
    if fCurrentValue<0 then fCurrentValue:=0;
  end;
end;

procedure TMeter.Draw;
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

end.


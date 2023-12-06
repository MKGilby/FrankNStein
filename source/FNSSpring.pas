{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSSpring;

{$mode Delphi}{$H+}

interface

uses SysUtils, fgl, Animation2Unit;

type

  { TSpring }

  TSpring=class
    constructor Create(iX,iY:integer);
    destructor Destroy; override;
    procedure Draw;
    procedure Move(pTimeUsed:double);
    procedure Kick;
  private
    fState:(sIdle,sKicking);
    fFase:double;
    fX,fY:integer;
    fAnimation:TAnimation;
  public
    PushedDown:boolean;
    property X:integer read fX;
    property Y:integer read fY;
  end;

  TSpringList=TFPGObjectList<TSpring>;

  { TSprings }

  TSprings=class(TSpringList)
    procedure AddSpringAt(iX,iY:integer);
    procedure Draw;
    procedure Move(pTimeUsed:double);
    function SpringAt(pX,pY:integer):TSpring;
  end;

implementation

uses FNSShared, Logger;

const
  KICKANIMSPEED=3;  // frame/sec
  ANIMFRAMES:array[0..21] of integer=(0,1,2,3,4,5,6,7,6,5,4,3,2,1,0,1,2,3,4,3,2,1);

{ TSpring }

constructor TSpring.Create(iX,iY:integer);
begin
  fX:=iX;
  fY:=iY;
  fAnimation:=MM.Animations.ItemByName['Spring'].SpawnAnimation;
  PushedDown:=false;
  fState:=sIdle;
end;

destructor TSpring.Destroy;
begin
  if Assigned(fAnimation) then fAnimation.Free;
  inherited Destroy;
end;

procedure TSpring.Draw;
begin
  case fState of
    sIdle:begin
      if not PushedDown then
        fAnimation.PutFrame(fX*8,fY*8-8,1)
      else
        fAnimation.PutFrame(fX*8,fY*8-8,0);
    end;
    sKicking:begin
      fAnimation.PutFrame(fX*8,fY*8-8,ANIMFRAMES[trunc(fFase*(length(ANIMFRAMES)-1))]);
    end;
  end;
end;

procedure TSpring.Move(pTimeUsed:double);
begin
  if fState=sKicking then begin
    fFase+=pTimeUsed*KICKANIMSPEED;
    if fFase>1 then fState:=sIdle;
  end;
end;

procedure TSpring.Kick;
begin
  if fState=sIdle then begin
    fState:=sKicking;
    PushedDown:=false;
    fFase:=0;
  end;
end;

{ TSprings }

procedure TSprings.AddSpringAt(iX,iY:integer);
begin
  Add(TSpring.Create(iX,iY));
end;

procedure TSprings.Draw;
var i:integer;
begin
  for i:=0 to Count-1 do Self[i].Draw;
end;

procedure TSprings.Move(pTimeUsed:double);
var i:integer;
begin
  for i:=0 to Count-1 do Self[i].Move(pTimeUsed);
end;

function TSprings.SpringAt(pX,pY:integer):TSpring;
var i:integer;
begin
  for i:=0 to Count-1 do begin
    if (Self[i].X=pX) and (Self[i].Y=pY) then begin
      Result:=Self[i];
      exit;
    end;
  end;
  raise Exception.Create(Format('Internal Error: No spring found at %d,%d!',[px,py]));
end;

end.


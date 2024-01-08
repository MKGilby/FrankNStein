{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSPlay1Map;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, FNSMapStatic, FNSDevice, FNSProf, FNSSpring, FNSMonster;

type

  { TPlay1Map }

  TPlay1Map=class
    constructor Create(iMapNo:integer);
    destructor Destroy; override;
    procedure Draw;
    procedure Move(pTimeUsed:double);
  private
//    fCurrentMapNo:integer;
    fMapStatic:TMapStatic;
    fDevice:TDevice;
    fProf:TProf;
    fMonster:TMonster;
    procedure MoveEx(pTimeUsed:double);
  end;

implementation

uses FNSShared;

{ TPlay1Map }

constructor TPlay1Map.Create(iMapNo: integer);
begin
  fMapStatic:=TMapStatic.Create(iMapNo);
  fDevice:=TDevice.Create(iMapNo);
  fProf:=TProf.Create(fMapStatic.TileMap,fDevice);
  fMonster:=TMonster.Create(Maps[iMapNo].MonsterData[0]);
end;

destructor TPlay1Map.Destroy;
begin
  if Assigned(fMonster) then fMonster.Free;
  if Assigned(fProf) then fProf.Free;
  if Assigned(fDevice) then fDevice.Free;
  if Assigned(fMapStatic) then fMapStatic.Free;
  inherited Destroy;
end;

procedure TPlay1Map.Draw;
begin
  fMapStatic.Draw;
  fDevice.Draw;
  Springs.Draw;
  fMonster.Draw;
  fProf.Draw;
end;

procedure TPlay1Map.Move(pTimeUsed: double);
begin
  if pTimeUsed<=MINLAG then begin  // Shorter than the lag threshold, so process.
    while pTimeUsed>MAXTIMESLICE do begin
      MoveEx(MAXTIMESLICE);
      pTimeUsed-=MAXTIMESLICE;
    end;
    MoveEx(pTimeUsed);
  end;
end;

procedure TPlay1Map.MoveEx(pTimeUsed:double);
begin
  Springs.Move(pTimeUsed);
  fProf.Move(pTimeUsed);
  fDevice.Move(pTimeUsed);
  fMonster.Move(pTimeUsed);
end;

end.


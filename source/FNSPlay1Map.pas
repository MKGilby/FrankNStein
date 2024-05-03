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
    fMonsters:TMonsters;
    procedure MoveEx(pTimeUsed:double);
  end;

implementation

uses FNSShared;

{ TPlay1Map }

constructor TPlay1Map.Create(iMapNo: integer);
var i:integer;
begin
  fMapStatic:=TMapStatic.Create(iMapNo);
  fDevice:=TDevice.Create(iMapNo);
  fProf:=TProf.Create(fMapStatic.TileMap,fDevice);
  fMonsters:=TMonsters.Create;
  for i:=0 to Maps[iMapNo].MonsterCount-1 do
    fMonsters.AddMonster(Maps[iMapNo].MonsterData[i]);
end;

destructor TPlay1Map.Destroy;
begin
  if Assigned(fMonsters) then fMonsters.Free;
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
  fMonsters.Draw;
  fProf.Draw;
end;

procedure TPlay1Map.Move(pTimeUsed: double);
begin
//  if keys[SDL_SCANCODE_A] then fMonster.RestartAtRight;
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
  fMonsters.Move(pTimeUsed);
end;

end.


{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSPlay1Map;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, FNSDevice, FNSProf, FNSSpring, FNSMonster, FNSJsonMap, FNSMeter,
  FNSLever;

type

  { TPlay1Map }

  TPlay1Map=class
    constructor Create(iMapNo:integer);
    destructor Destroy; override;
    procedure Draw;
    procedure Move(pTimeUsed:double);
  private
//    fCurrentMapNo:integer;
    fMap:TJSONMap;
    fDevice:TDevice;
    fSprings:TSprings;
    fProf:TProf;
    fMonsters:TMonsters;
    fMeter:TMeter;
    fLever:TLever;
    procedure MoveEx(pTimeUsed:double);
  end;

implementation

uses FNSShared;

{ TPlay1Map }

constructor TPlay1Map.Create(iMapNo: integer);
var i:integer;
begin
  fSprings:=TSprings.Create;
  fMap:=TJSONMap.Create(iMapNo,fSprings,false);
  fDevice:=TDevice.Create(fMap);
  fProf:=TProf.Create(fMap.TileMap,fDevice,fSprings);
  fMonsters:=TMonsters.Create;
  for i:=0 to fMap.MonsterCount-1 do
    fMonsters.AddMonster(fMap.MonsterData[i]);
  fMeter:=TMeter.Create;
  fLever:=TLever.Create(fMap,25*8,2*8);
end;

destructor TPlay1Map.Destroy;
begin
  fLever.Free;
  fMeter.Free;
  fMonsters.Free;
  fProf.Free;
  fDevice.Free;
  fMap.Free;
  fSprings.Free;
  inherited Destroy;
end;

procedure TPlay1Map.Draw;
begin
  fMap.Draw;
  fDevice.Draw;
  fMeter.Draw;
  fLever.Draw;
  fSprings.Draw;
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
  fSprings.Move(pTimeUsed);
  if fProf.Move(pTimeUsed)=pmrPickedUpLastPiece then fLever.Arm;
  fDevice.Move(pTimeUsed);
  fMonsters.Move(pTimeUsed);
  fMeter.Move(pTimeUsed);
  fLever.Move(pTimeUsed);
end;

end.


unit FNSPlay1Map;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, FNSMapStatic, FNSDevice, FNSProf;

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
  end;

implementation

{ TPlay1Map }

constructor TPlay1Map.Create(iMapNo: integer);
begin
  fMapStatic:=TMapStatic.Create(iMapNo);
  fDevice:=TDevice.Create(iMapNo);
  fProf:=TProf.Create(fMapStatic.TileMap,fDevice);
end;

destructor TPlay1Map.Destroy;
begin
  if Assigned(fProf) then fProf.Free;
  if Assigned(fDevice) then fDevice.Free;
  if Assigned(fMapStatic) then fMapStatic.Free;
  inherited Destroy;
end;

procedure TPlay1Map.Draw;
begin
  fMapStatic.Draw;
  fDevice.Draw;
  fProf.Draw;
end;

procedure TPlay1Map.Move(pTimeUsed: double);
begin
  fProf.Move(pTimeUsed);
end;

end.


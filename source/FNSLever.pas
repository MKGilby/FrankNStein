unit FNSLever;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Animation2Unit, FNSJsonMap;

type

  TLeverMoveRes=(lmrNone,lmrFinished);

  { TLever }

  TLever=class
    constructor Create(iMap:TJSONMap;iX,iY:integer);
    destructor Destroy; override;
    procedure Draw;
    function Move(pTimeUsed:double):TLeverMoveRes;
    procedure Arm;
    procedure Push;
  private
    fState:(sIdle,sArmed,sPushing,sFinished);
    fFase:double;
    fX,fY:integer;
    fAnimation:TAnimation;
  public
    property X:integer read fX;
    property Y:integer read fY;
  end;

implementation

uses FNSShared;

{ TLever }

constructor TLever.Create(iMap:TJSONMap; iX,iY:integer);
begin
  fX:=iX;
  fY:=iY;
  iMap.TileMap.Tiles[fX div 8,fY div 8+1]:=TILE_LEVER;
  fState:=sIdle;
  fAnimation:=MM.Animations['Lever'].SpawnAnimation;
end;

destructor TLever.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TLever.Draw;
begin
  case fState of
    sIdle:fAnimation.PutFrame(fX,fY,0);
    sArmed:if trunc(fFase*4) mod 2=0 then
             fAnimation.PutFrame(fX,fY,1)
           else
             fAnimation.PutFrame(fX,fY,0);
    sPushing: fAnimation.PutFrame(fX,fY,1+trunc(fFase*5));
    sFinished: fAnimation.PutFrame(fX,fY,6);
  end;
end;

function TLever.Move(pTimeUsed: double): TLeverMoveRes;
begin
  Result:=lmrNone;
  case fstate of
    sIdle: ;
    sArmed: fFase:=fFase+pTimeUsed;
    sPushing: begin
      fFase:=fFase+pTimeUsed;
      if fFase>=1 then begin
        fState:=sFinished;
        Result:=lmrFinished;
      end;
    end;
    sFinished: ;
  end;
end;

procedure TLever.Arm;
begin
  if fState=sIdle then begin
    fState:=sArmed;
    fFase:=0;
  end;
end;

procedure TLever.Push;
begin
  if fState=sArmed then begin
    fState:=sPushing;
    fFase:=0;
  end;
end;

end.


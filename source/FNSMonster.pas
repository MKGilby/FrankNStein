unit FNSMonster;

{$mode Delphi}

interface

uses
  SysUtils, FNSJsonMap, Animation2Unit, fgl;

type

  { TMonster }

  TMonster=class
    constructor Create(iMonsterData:TMonsterData);
    destructor Destroy; override;
    procedure Move(pTimeUsed:double);
    procedure Draw;
    procedure RestartAtRight;
  var
    fX:double;
    fY:integer;
    fMin,fMax:integer;
    fDir:integer;       // direction -1 / +1
    fSpeed:double;      // pixel / sec
    fAnimLeft,fAnimRight:TAnimation;
  end;

  { TMonsters }

  TMonsters=class(TFPGObjectList<TMonster>)
    procedure AddMonster(iMonsterData:TMonsterData);
    procedure Move(pTimeUsed:double);
    procedure Draw;
  end;

implementation

uses FNSShared;

{ TMonster }

constructor TMonster.Create(iMonsterData:TMonsterData);
begin
  fX:=iMonsterData._start;
  fY:=iMonsterData._row*8;
  fMin:=iMonsterData._left;
  fMax:=iMonsterData._right;
  fSpeed:=iMonsterData._speed;
  if fSpeed>0 then fDir:=1
  else if fSpeed<0 then fDir:=-1
  else fDir:=0;
  fSpeed:=abs(fSpeed);
  fAnimLeft:=MM.Animations.ItemByName[Format('Mons%.2dLeft',[iMonsterData._animationindex])].SpawnAnimation;
  fAnimRight:=MM.Animations.ItemByName[Format('Mons%.2dRight',[iMonsterData._animationindex])].SpawnAnimation;
end;

destructor TMonster.Destroy;
begin
  if Assigned(fAnimLeft) then fAnimLeft.Free;
  if Assigned(fAnimRight) then fAnimRight.Free;
  inherited Destroy;
end;

procedure TMonster.Move(pTimeUsed:double);
var dist:double;
begin
  fAnimLeft.Animate(pTimeUsed);
  fAnimRight.Animate(pTimeUsed);
  if fDir<>0 then begin
    dist:=pTimeUsed*fSpeed;
    while dist>0 do begin
      if fDir>0 then begin
        if fX+dist>=fMax then begin
          dist+=fX-fMax;
          fX:=fMax;
          fDir:=-1;
        end else begin
          fX+=dist;
          dist:=0;
        end;
      end else
      if fDir<0 then begin
        if fX-dist<fMin then begin
          dist-=fX-fMin;
          fX:=fMin;
          fDir:=1;
        end else begin
          fX-=dist;
          dist:=0
        end;
      end;
    end;
  end;
end;

procedure TMonster.Draw;
begin
  if fDir>=0 then
    fAnimRight.PutFrame(trunc(fX),fY)
  else
    fAnimLeft.PutFrame(trunc(fX),fY);
end;

procedure TMonster.RestartAtRight;
begin
  fX:=fMax;
  fDir:=-1;
end;

{ TMonsters }

procedure TMonsters.AddMonster(iMonsterData: TMonsterData);
begin
  Add(TMonster.Create(iMonsterData));
end;

procedure TMonsters.Move(pTimeUsed: double);
var i:integer;
begin
  for i:=0 to Count-1 do
    Items[i].Move(pTimeUsed);
end;

procedure TMonsters.Draw;
var i:integer;
begin
  for i:=0 to Count-1 do
    Items[i].Draw;
end;

end.


unit FNSDevice;

{$mode delphi}{$H+}

interface

uses
  SysUtils, AnimatedSprite2Unit;

type

  { TDevice }

  TDevice=class
    constructor Create(iMapNo:integer);
    procedure Draw;
    procedure PickupPiece;
  private
    fPieces:array[0..6] of TAnimatedSprite;
    fSkeleton:array[0..6] of TAnimatedSprite;
    fNextPiece:integer;
  public
    property NextPiece:integer read fNextPiece;
  end;

implementation

uses FNSShared, FNSMap;

const
  DEVICEINNERLEFT=28*8;
  DEVICEINNERTOP=2*8;

{ TDevice }

constructor TDevice.Create(iMapNo:integer);
var i,pc:integer;
begin
  fNextPiece:=0;
  fSkeleton[0]:=TAnimatedSprite.Create(DEVICEINNERLEFT+4,DEVICEINNERTOP,
    MM.Animations.ItemByName['Skeleton1'].SpawnAnimation);
  fSkeleton[1]:=TAnimatedSprite.Create(DEVICEINNERLEFT,DEVICEINNERTOP+8,
    MM.Animations.ItemByName['Skeleton2'].SpawnAnimation);
  fSkeleton[2]:=TAnimatedSprite.Create(DEVICEINNERLEFT+8,DEVICEINNERTOP+8,
    MM.Animations.ItemByName['Skeleton3'].SpawnAnimation);
  fSkeleton[3]:=TAnimatedSprite.Create(DEVICEINNERLEFT,DEVICEINNERTOP+16,
    MM.Animations.ItemByName['Skeleton4'].SpawnAnimation);
  fSkeleton[4]:=TAnimatedSprite.Create(DEVICEINNERLEFT+8,DEVICEINNERTOP+16,
    MM.Animations.ItemByName['Skeleton5'].SpawnAnimation);
  fSkeleton[5]:=TAnimatedSprite.Create(DEVICEINNERLEFT,DEVICEINNERTOP+24,
    MM.Animations.ItemByName['Skeleton6'].SpawnAnimation);
  fSkeleton[6]:=TAnimatedSprite.Create(DEVICEINNERLEFT+8,DEVICEINNERTOP+24,
    MM.Animations.ItemByName['Skeleton7'].SpawnAnimation);
  pc:=0;
  for i:=0 to Maps[iMapNo].BlockCount-1 do with Maps[iMapNo].BlockData[i] do
    if _type=btPiece then begin
      inc(pc);
      fPieces[pc]:=TAnimatedSprite.Create(_x*8,_y*8,
        MM.Animations.ItemByName[Format('Piece%d',[pc])].SpawnAnimation);
    end;
end;

procedure TDevice.Draw;
var i:integer;
begin
  for i:=0 to fNextPiece-1 do fSkeleton[i].Draw;
  for i:=fNextPiece to 6 do fPieces[i].Draw;
end;

procedure TDevice.PickupPiece;
begin

end;

end.


{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSDevice;

{$mode delphi}{$H+}

interface

uses
  SysUtils, AnimatedSprite2Unit, mk_sdl2, Animation2Unit, FNSJsonMap;

type

  { TSkeletonPiece }

  TSkeletonPiece=class(TAnimatedSprite)
    procedure SetStartPosition(pX,pY:integer);
    procedure PickedUp;
    procedure Move(pTimeUsed:double); reintroduce;
    procedure Draw; override;
  private
    fStartX,fStartY,fEndX,fEndY:integer;
    fState:integer;  // 0-Not picked up, 1-Floating to place, 2-Finished
    fFase:double;
  end;

  { TDevice }

  TDevice=class
    constructor Create(iMap:TJSONMap);
    destructor Destroy; override;
    procedure Draw;
    procedure Move(pTimeUsed:double);
    procedure PickupPiece;
  private
    fPieces:array[0..6] of TAnimatedSprite;
    fSkeleton:array[0..6] of TSkeletonPiece;
    fOverlay:TAnimation;
    fNextPiece:integer;
  public
    property NextPiece:integer read fNextPiece;
  end;

implementation

uses FNSShared, Logger;

const
  DEVICEINNERLEFT=28*8;
  DEVICEINNERTOP=2*8;
  FLOATINGTIME=3;  // in seconds

{ TSkeletonPiece }

procedure TSkeletonPiece.SetStartPosition(pX,pY:integer);
begin
  fEndX:=X;
  fEndY:=Y;
  fStartX:=pX;
  fStartY:=pY;
  fX:=pX;
  fY:=pY;
  fState:=0;
end;

procedure TSkeletonPiece.PickedUp;
begin
  fState:=1;
  fFase:=0;
end;

procedure TSkeletonPiece.Move(pTimeUsed:double);
begin
  if fState=1 then begin
    fFase+=pTimeUsed/FLOATINGTIME;
    if fFase<1 then begin
      fX:=trunc(fStartX+(fEndX-fStartX)*fFase);
      fY:=trunc(fStartY+(fEndY-fStartY)*fFase);
    end else begin
      fX:=fEndX;
      fY:=fEndY;
      fFase:=2;
    end;
  end;
end;

procedure TSkeletonPiece.Draw;
begin
  if fState>=1 then inherited Draw;
end;

{ TDevice }

constructor TDevice.Create(iMap:TJSONMap);
var i,j,pc:integer;
begin
  fNextPiece:=0;
  fSkeleton[0]:=TSkeletonPiece.Create(DEVICEINNERLEFT+4,DEVICEINNERTOP,
    MM.Animations.ItemByName['Skeleton1'].SpawnAnimation);
  fSkeleton[1]:=TSkeletonPiece.Create(DEVICEINNERLEFT,DEVICEINNERTOP+8,
    MM.Animations.ItemByName['Skeleton2'].SpawnAnimation);
  fSkeleton[2]:=TSkeletonPiece.Create(DEVICEINNERLEFT+8,DEVICEINNERTOP+8,
    MM.Animations.ItemByName['Skeleton3'].SpawnAnimation);
  fSkeleton[3]:=TSkeletonPiece.Create(DEVICEINNERLEFT,DEVICEINNERTOP+16,
    MM.Animations.ItemByName['Skeleton4'].SpawnAnimation);
  fSkeleton[4]:=TSkeletonPiece.Create(DEVICEINNERLEFT+8,DEVICEINNERTOP+16,
    MM.Animations.ItemByName['Skeleton5'].SpawnAnimation);
  fSkeleton[5]:=TSkeletonPiece.Create(DEVICEINNERLEFT,DEVICEINNERTOP+24,
    MM.Animations.ItemByName['Skeleton6'].SpawnAnimation);
  fSkeleton[6]:=TSkeletonPiece.Create(DEVICEINNERLEFT+8,DEVICEINNERTOP+24,
    MM.Animations.ItemByName['Skeleton7'].SpawnAnimation);
  pc:=0;
  for pc:=0 to 6 do
    for j:=0 to MAPHEIGHTINBLOCKS-1 do
      for i:=0 to MAPWIDTHINBLOCKS-1 do
        if iMap.TileMap.Tiles[i,j]=TILE_PIECE+pc then begin
          fPieces[pc]:=TAnimatedSprite.Create(i*8-1,j*8,
            MM.Animations.ItemByName[Format('Piece%d',[pc+1])].SpawnAnimation);
          fSkeleton[pc].SetStartPosition(i*8,j*8);
        end;
  fPieces[0].Animation.Timer.Paused:=false;
  fOverlay:=MM.Animations.ItemByName['DeviceOverlay'].SpawnAnimation;
end;

destructor TDevice.Destroy;
var i:integer;
begin
  if Assigned(fOverlay) then fOverlay.Free;
  for i:=0 to 6 do begin
    if Assigned(fSkeleton[i]) then begin
      fSkeleton[i].Animation.Free;
      fSkeleton[i].Free;
    end;
    if Assigned(fPieces[i]) then begin
      fPieces[i].Animation.Free;
      fPieces[i].Free;
    end;
  end;
  inherited Destroy;
end;

procedure TDevice.Draw;
var i:integer;
begin
  for i:=0 to fNextPiece-1 do fSkeleton[i].Draw;
  for i:=fNextPiece to 6 do fPieces[i].Draw;
  for i:=0 to 3 do fOverlay.PutFrame(DEVICEINNERLEFT,DEVICEINNERTOP+i*8);
end;

procedure TDevice.Move(pTimeUsed:double);
var i:integer;
begin
  for i:=fNextPiece to 6 do
    fPieces[i].Animation.Animate(pTimeUsed);
  for i:=0 to 6 do
    fSkeleton[i].Move(pTimeUsed);
end;

procedure TDevice.PickupPiece;
begin
  if fNextPiece<7 then begin
    fSkeleton[fNextPiece].PickedUp;
    fPieces[fNextPiece].Animation.Timer.CurrentFrameIndex:=0;
    fPieces[fNextPiece].Animation.Timer.Paused:=true;
    inc(fNextPiece);
    if fNextPiece<7 then
      fPieces[fNextPiece].Animation.Timer.Paused:=false;
  end;
end;

end.


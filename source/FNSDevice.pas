{
  Frank N Stein Refurbished - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of Frank N Stein Refurbished.

  Frank N Stein Refurbished is free software: you can redistribute it
  and/or modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  Frank N Stein Refurbished is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  Frank N Stein Refurbished. If not, see <https://www.gnu.org/licenses/>.
}

unit FNSDevice;

{$mode delphi}{$H+}

interface

uses
  SysUtils, AnimatedSprite2Unit, mk_sdl2, Animation2Unit;

type

  { TDevice }

  TDevice=class
    constructor Create(iMapNo:integer);
    destructor Destroy; override;
    procedure Draw;
    procedure PickupPiece;
  private
    fPieces:array[0..6] of TAnimatedSprite;
    fSkeleton:array[0..6] of TAnimatedSprite;
    fOverlay:TAnimation;
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
      if pc=7 then raise Exception.Create('Too many skeleton pieces is map!');
      fPieces[pc]:=TAnimatedSprite.Create(_x*8-1,_y*8,
        MM.Animations.ItemByName[Format('Piece%d',[pc+1])].SpawnAnimation);
      inc(pc);
    end;
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

procedure TDevice.PickupPiece;
begin
  if fNextPiece<7 then inc(fNextPiece);
end;

end.


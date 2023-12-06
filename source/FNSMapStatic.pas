{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSMapStatic;

{$mode Delphi}{$H+}

interface

uses
  mk_sdl2, ARGBImageUnit, FNSMap, TileMapUnit, FNSSpring;

type

  { TMapStatic }

  TMapStatic=class
    constructor Create(iMapNo:integer);
    destructor Destroy; override;
    procedure Draw;
  private
    fTexture:TTexture;
    fTileMap:TTileMap;
    procedure FillBackWithStones(pImage:TARGBImage);
    procedure CreateTileMap;
    procedure AddBlocks(pMap:TMap;pImage:TARGBImage);
  public
    property TileMap:TTileMap read fTileMap;
  end;

implementation

uses FNSShared;

{ TMapStatic }

constructor TMapStatic.Create(iMapNo:integer);
const platf='12345   123334512451233345';
var tmp:TARGBImage;i:integer;
begin
  tmp:=TARGBImage.Create(LOGICALWINDOWWIDTH,LOGICALWINDOWHEIGHT);
  tmp.bar(0,0,tmp.Width,tmp.Height,0,0,0,255);
  FillBackWithStones(tmp);
  MM.Images.ItemByName['Decorations'].CopyTo(0,0,8,16,48,0,tmp,true);  // Bulb
  MM.Images.ItemByName['Decorations'].CopyTo(8,0,48,16,112,0,tmp,true);  // Shelf
  MM.Images.ItemByName['Decorations'].CopyTo(56,0,24,16,8,0,tmp,true);  // Lives
  CreateTileMap;
  if Maps[iMapNo].MapType=MAPTYPECONSTRUCTING then begin
    for i:=0 to 25 do begin
      if platf[i+1]<>' ' then fTileMap[i,4]:=TILE_WALL;
      MM.Images.ItemByName['Decorations'].CopyTo(80+(ord(platf[i+1])-49)*8,0,8,8,i*8,32,tmp,true)  // Top platform
    end;
  end;
  with MM.Images.ItemByName['Device'] do
    CopyTo(0,0,Width,Height,26*8,8,tmp,true);

  AddBlocks(Maps[iMapNo],tmp);

  fTexture:=TStaticTexture.Create(tmp);
  tmp.Free;
end;

destructor TMapStatic.Destroy;
begin
  if Assigned(fTexture) then fTexture.Free;
  if Assigned(fTileMap) then fTileMap.Free;
  inherited Destroy;
end;

procedure TMapStatic.Draw;
begin
  PutTexture(0,0,fTexture);
end;

procedure TMapStatic.FillBackWithStones(pImage:TARGBImage);
var
  helper:array[0..LOGICALWINDOWWIDTH div 8-1,0..LOGICALWINDOWHEIGHT div 8-1] of byte;
  wi,he,i,x,y:integer;
  stones:array[0..3] of TARGBImage;

  function CheckSpace(x,y,w,h:integer):boolean;
  var i,j:integer;
  begin
    Result:=false;
    if (x<0) or (x+w>wi) or (y<0) or (y+h>he) then exit;
    for j:=y to y+h-1 do
      for i:=x to x+w-1 do
        if helper[i,j]<>0 then exit;
    Result:=true;
  end;

  procedure FillSpace(x,y,w,h:integer);
  var i,j:integer;
  begin
    if (x<0) or (x+w>wi) or (y<0) or (y+h>he) then exit;
    for j:=y to y+h-1 do
      for i:=x to x+w-1 do
        helper[i,j]:=1;
  end;

begin
  wi:=LOGICALWINDOWWIDTH div 8;
  he:=LOGICALWINDOWHEIGHT div 8;
  for i:=0 to 3 do begin
    stones[i]:=TARGBImage.Create(48,24);
    MM.Images.ItemByName['Stones'].Copy(0,0,48,24,stones[i]);
    Stones[i].RecolorRGB(150-i*20,150-i*20,150-i*20);
  end;
  fillchar(helper,wi*he,0);
  for i:=0 to 30 do begin
    x:=random(wi-2);
    y:=random(he-2);
    if CheckSpace(x,y,3,3) then begin
      FillSpace(x,y,3,3);
      Stones[random(4)].CopyTo(0,0,24,24,x*8,y*8,pImage);
    end;
  end;
  for y:=0 to he-1 do
    for x:=0 to wi-1 do
      while helper[x,y]=0 do begin
        i:=random(12);
        case i of
          0,1,2:if CheckSpace(x,y,2,2) then begin
                  FillSpace(x,y,2,2);
                  Stones[random(4)].CopyTo(24,0,16,16,x*8,y*8,pImage);
                end;
          3,4,5:if CheckSpace(x,y,2,1) then begin
                  FillSpace(x,y,2,1);
                  Stones[random(4)].CopyTo(24,16,16,8,x*8,y*8,pImage);
                end;
          6,7,8:if CheckSpace(x,y,1,2) then begin
                  FillSpace(x,y,1,2);
                  Stones[random(4)].CopyTo(40,8,8,16,x*8,y*8,pImage);
                end;
          9,10:begin
              Stones[random(4)].CopyTo(40,0,8,8,x*8,y*8,pImage);
              helper[x,y]:=1;
            end;
          11:if CheckSpace(x,y,3,3) then begin
               FillSpace(x,y,3,3);
               Stones[random(4)].CopyTo(0,0,24,24,x*8,y*8,pImage);
             end;
        end;
      end;
  for i:=0 to 3 do Stones[i].Free;
end;

procedure TMapStatic.CreateTileMap;
var i:integer;
begin
  // Create slightly bigger tilemap to cover one tile outside of play area
  fTileMap:=TTileMap.Create(32+2,22+2);
  // Set origin so we can still access play area as Tiles[0..31,0..21]
  fTileMap.OriginX:=-1;
  fTileMap.OriginY:=-1;
  // Add invisible walls to the left and right sides to the map.
  for i:=-1 to 22 do begin
    fTileMap[-1,i]:=TILE_WALL;
    fTileMap[32,i]:=TILE_WALL;
  end;
  // Clear spring container.
  Springs.Clear;
end;

procedure TMapStatic.AddBlocks(pMap:TMap; pImage:TARGBImage);
var i,j,pc:integer;tiles,tmp:TARGBImage;
begin
  // Get Tileset image
  tiles:=MM.Images.ItemByName['Tiles'];
  // Piece count
  pc:=0;
  // Create a temporary image to draw tiles to. This is needed beacuse on some
  // tiles there are multiple blocks (wall+spring for example), but we need
  // only the last one.
  tmp:=TARGBImage.Create(LOGICALWINDOWWIDTH,LOGICALWINDOWHEIGHT);
  try
    tmp.Clear(0);  // Fully transparent
    // Draw wall tiles
    for i:=0 to pMap.BlockCount-1 do
      if pMap.BlockData[i]._type=btWall then with pMap.BlockData[i] do
        for j:=0 to _length-1 do begin
          fTileMap.Tiles[_x+j,_y]:=TILE_WALL;
          tiles.CopyTo(0,0,8,8,(_x+j)*8,_y*8,tmp,true);
        end;

    // Draw remaining objects. These will overwrite wall tiles!
    for i:=0 to pMap.BlockCount-1 do with pMap.BlockData[i] do
      case _type of
        btSpring:begin
          fTileMap.Tiles[_x,_y]:=TILE_SPRING;
//          tiles.CopyTo(8,0,8,8,_x*8,_y*8,tmp);
          Springs.AddSpringAt(_x,_y);
          tmp.bar(_x*8,_y*8,8,8,0);
        end;
        btIce:begin
          fTileMap.Tiles[_x,_y]:=TILE_ICE;
          fTileMap.Tiles[_x+1,_y]:=TILE_ICE;
          tiles.CopyTo(16,0,16,8,_x*8,_y*8,tmp);
        end;
        btZapper:begin
          fTileMap.Tiles[_x,_y]:=TILE_ZAPPER;
          tiles.CopyTo(8,0,8,8,_x*8,_y*8,tmp);
        end;
        btMud:begin
          fTileMap.Tiles[_x,_y]:=TILE_MUD;
          fTileMap.Tiles[_x+1,_y]:=TILE_MUD;
          fTileMap.Tiles[_x+2,_y]:=TILE_MUD;
        end;
        btJumper:begin
          fTileMap.Tiles[_x,_y]:=TILE_JUMPER;
        end;
        btPole:begin
          for j:=0 to _length-1 do begin
            fTileMap.Tiles[_x,_y+j]:=TILE_POLE;
            if j=0 then tiles.CopyTo(64,0,8,8,_x*8,_y*8,tmp)
            else if j=_length-1 then tiles.CopyTo(80,0,8,8,_x*8,(_y+j)*8,tmp)
            else tiles.CopyTo(72,0,8,8,_x*8,(_y+j)*8,tmp);
          end;
        end;
        btStairsL:begin
          for j:=0 to _length-1 do begin
            fTileMap.Tiles[_x-j,_y+j]:=TILE_WALL;
            fTileMap.Tiles[_x+1-j,_y+j]:=TILE_WALL;
            tiles.CopyTo(48,0,16,8,(_x-j)*8,(_y+j)*8,tmp);
          end;
        end;
        btStairsR:begin
          for j:=0 to _length-1 do begin
            fTileMap.Tiles[_x+j,_y+j]:=TILE_WALL;
            fTileMap.Tiles[_x+1+j,_y+j]:=TILE_WALL;
            tiles.CopyTo(32,0,16,8,(_x+j)*8,(_y+j)*8,tmp);
          end;
        end;
        btPiece:begin
          fTileMap.Tiles[_x,_y]:=TILE_PIECE+pc;
          inc(pc);
        end;
      end;
    // Copy the temporary image to the static image.
    tmp.CopyTo(0,0,LOGICALWINDOWWIDTH,LOGICALWINDOWHEIGHT,0,0,pImage,true);
  finally
    tmp.Free;
  end;
end;

end.


{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSJsonMap;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, TileMapUnit, mk_sdl2, ARGBImageUnit;

type

  { TMonsterData }

  TMonsterData=record
    _row:integer;
    _left:integer;
    _right:integer;
    _start:integer;
    _speed:double;
    _animationindex:integer;
    constructor Init(iRow,iLeft,iRight,iStart:integer;iSpeed:double;iAnimationIndex:integer);
  end;

  TMonsterDataList=array of TMonsterData;

  TDecorationType=(dtWindow,dtDoor);

  { TDecorationData }

  TDecorationData=record
    _type:TDecorationType;
    _x,_y:integer;
    constructor Init(iType:TDecorationType;iX,iY:integer);
  end;

  TDecorationDataList=array of TDecorationData;

  { TJSONMap }

  TJSONMap=class
    constructor Create(iMapNo:integer;iMetaDataOnly:boolean=true);
    destructor Destroy; override;
    procedure Draw;
  private
    fName,fAuthor:string;
    fMapType:integer;
    fTileMap:TTileMap;
    fTexture:TTexture;
    // Array of monster data
    fMonsters:TMonsterDataList;
    // Array of decoration data
    fDecorations:TDecorationDataList;
    procedure LoadTiles(JSON:TJSONData;pImage:TARGBImage);
    procedure LoadMonsters(JSON:TJSONData);
    procedure LoadDecorations(JSON:TJSONData);
    procedure FillBackWithStones(pImage:TARGBImage);
    procedure CreateTileMap;
    procedure LoadOverlay(pImage:TARGBImage;pName:string);
    function GetString(JSON:TJSONData;Path:string;Default:string=''):string;
    function GetInteger(JSON:TJSONData;Path:string;Default:integer=0):integer;
    function GetFloat(JSON:TJSONData;Path:string;Default:double=0):double;
    function fGetMonsterData(index:integer):TMonsterData;
    function fGetMonsterCount:integer;
    function fGetDecorationData(index:integer):TDecorationData;
    function fGetDecorationCount:integer;
  public
    property Name:string read fName;
    property Author:string read fAuthor;
    property MapType:integer read fMapType;
    property TileMap:TTileMap read fTileMap;
    property MonsterData[index:integer]:TMonsterData read fGetMonsterData;
    property MonsterCount:integer read fGetMonsterCount;
    property DecorationData[index:integer]:TDecorationData read fGetDecorationData;
    property DecorationCount:integer read fGetDecorationCount;
  end;

  { TMapList }

  TMapList=class(TStringList)
    procedure Load;
  private
    function fGetMapName(index:integer):string;
    function fGetAuthor(index:integer):string;
  public
    property MapNames[index:integer]:string read fGetMapName;
    property Authors[index:integer]:string read fGetAuthor;
  end;

implementation

uses MKStream, FNSShared, MKToolbox;

{ TMonsterData }

constructor TMonsterData.Init(iRow,iLeft,iRight,iStart:integer; iSpeed:double;
  iAnimationIndex:integer);
begin
  _row:=iRow;
  _left:=iLeft;
  _right:=iRight;
  _start:=iStart;
  _speed:=iSpeed;
  _animationindex:=iAnimationIndex;
end;

{ TDecorationData }

constructor TDecorationData.Init(iType:TDecorationType; iX,iY:integer);
begin
  _type:=iType;
  _x:=iX;
  _y:=iY;
end;

{ TJSONMap }

constructor TJSONMap.Create(iMapNo:integer; iMetaDataOnly:boolean);
const platf='12345   123334512451233345';
var Stream:TStream;JSON:TJSONData;tmp:TARGBImage;i:integer;
begin
  Stream:=MKStreamOpener.OpenStream(Format('maps\%.2d.json',[iMapNo]));
  try
    JSON:=GetJSON(Stream);
  finally
    Stream.Free;
  end;
  try
    fMapType:=strtoint(decode(GetString(JSON,'Type','Constructing'),Format(
      'Construction,%d,InterimOriginal,%d,InterimRebooted,%d,Congratulations,%d,%d',
      [MAPTYPECONSTRUCTING,MAPTYPEINTERIMORIGINAL,MAPTYPEINTERIMREBOOTED,MAPTYPECONGRATULATIONS,MAPTYPECONSTRUCTING])));
    fAuthor:=GetString(JSON,'Author','N/A');
    fName:=GetString(JSON,'Name','N/A');
    if not iMetaDataOnly then begin
      tmp:=TARGBImage.Create(LOGICALWINDOWWIDTH,LOGICALWINDOWHEIGHT);
      try
        tmp.bar(0,0,tmp.Width,tmp.Height,0,0,0,255);
        FillBackWithStones(tmp);
        if fMapType=MAPTYPECONSTRUCTING then LoadOverlay(tmp,'constr');
        CreateTileMap;
        if maptype=MAPTYPECONSTRUCTING then begin
          for i:=0 to 25 do begin
            if platf[i+1]<>' ' then fTileMap[i,4]:=TILE_WALL;
          end;
        end;
        LoadTiles(JSON,tmp);
        LoadMonsters(JSON);
        LoadDecorations(JSON);
        fTexture:=TStaticTexture.Create(tmp);
      finally
        tmp.Free;
      end;
    end;
  finally
    JSON.Free;
  end;
end;

destructor TJSONMap.Destroy;
begin
  fTileMap.Free;
  fTexture.Free;
  inherited Destroy;
end;

procedure TJSONMap.Draw;
begin
  PutTexture(0,0,fTexture);
end;

procedure TJSONMap.LoadTiles(JSON:TJSONData; pImage:TARGBImage);
const tileorder='x!()L></^|v';
var JA:TJSONArray;i,j,walltile,pc:integer;s:string;tiles:TARGBImage;
begin
  walltile:=GetInteger(JSON,'WallTileIndex',0);
  // Get Tileset image
  tiles:=MM.Images.ItemByName['Tiles'];
  // Piece count
  pc:=0;
  // Get TileRows array from JSON
  JA:=TJSONArray(JSON.FindPath('TileRows'));
  // If found
  if Assigned(JA) then begin
    for i:=0 to min(JA.Count-1,21) do begin
      s:=JA.Items[i].AsString;
      while length(s)<32 do s+=' ';
      for j:=0 to 31 do begin
        case s[j+1] of
          '-':begin
                fTileMap.Tiles[j,i]:=TILE_WALL;
                tiles.CopyTo(walltile*8,0,8,8,j*8,i*8,pImage,true);
              end;
          'L','>','<','/':begin
                fTileMap.Tiles[j,i]:=TILE_WALL;
                tiles.CopyTo((pos(s[j+1],tileorder)-1)*8,0,8,8,j*8,i*8,pImage,true);
              end;
          '(',')':begin
                fTileMap.Tiles[j,i]:=TILE_ICE;
                tiles.CopyTo((pos(s[j+1],tileorder)-1)*8,0,8,8,j*8,i*8,pImage,true);
              end;
          '^','|','v':begin
                fTileMap.Tiles[j,i]:=TILE_POLE;
                tiles.CopyTo((pos(s[j+1],tileorder)-1)*8,0,8,8,j*8,i*8,pImage,true);
              end;
          '!':begin
                fTileMap.Tiles[j,i]:=TILE_ZAPPER;
                tiles.CopyTo((pos(s[j+1],tileorder)-1)*8,0,8,8,j*8,i*8,pImage,true);
              end;
          ':':begin
                fTileMap.Tiles[j,i]:=TILE_SPRING;
                Springs.AddSpringAt(j,i);
              end;
          '1'..'7':begin
                fTileMap.Tiles[j,i]:=TILE_PIECE+ord(s[j+1])-ord('1');
                tiles.CopyTo(walltile*8,0,8,8,j*8,i*8,pImage,true);
                inc(pc);
              end;
        end;
      end;
    end;
  end;
end;

procedure TJSONMap.LoadMonsters(JSON:TJSONData);
var JA:TJSONArray;JD:TJSONData;tmpM:TMonsterData;i:integer;
begin
  SetLength(fMonsters,0);
  JA:=TJSONArray(JSON.FindPath('Monsters'));
  if Assigned(JA) then
    for i:=0 to JA.Count-1 do begin
      JD:=JA.Items[i];
      tmpM:=TMonsterData.Init(
        JD.FindPath('Row').AsInteger,
        JD.FindPath('Left').AsInteger,
        JD.FindPath('Right').AsInteger,
        JD.FindPath('Start').AsInteger,
        JD.FindPath('PPS').AsFloat,
        JD.FindPath('AnimationNo').AsInteger
      );
      SetLength(fMonsters,length(fMonsters)+1);
      fMonsters[length(fMonsters)-1]:=tmpM;
    end;
end;

procedure TJSONMap.LoadDecorations(JSON:TJSONData);
var JA:TJSONArray;JD:TJSONData;tmpD:TDecorationData;i:integer;s:String;
begin
  SetLength(fDecorations,0);
  JA:=TJSONArray(JSON.FindPath('Decorations'));
  if Assigned(JA) then
    for i:=0 to JA.Count-1 do begin
      JD:=JA.Items[i];
      s:=uppercase(JD.FindPath('Type').AsString);
      if s='WINDOW' then tmpD:=TDecorationData.Init(
        dtWindow,
        JD.FindPath('Left').AsInteger*8,
        JD.FindPath('Top').AsInteger*8)
      else if s='DOOR' then tmpD:=TDecorationData.Init(
        dtDoor,
        JD.FindPath('Left').AsInteger*8,
        JD.FindPath('Top').AsInteger*8);
      SetLength(fDecorations,length(fDecorations)+1);
      fDecorations[length(fDecorations)-1]:=tmpD;
    end;
end;

procedure TJSONMap.FillBackWithStones(pImage:TARGBImage);
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
  helper[0,0]:=0;
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

procedure TJSONMap.CreateTileMap;
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
  // Add invisible walls to the top and bottom sides to the map.
  for i:=0 to 31 do begin
    fTileMap[i,-1]:=TILE_WALL;
    fTileMap[i,22]:=TILE_WALL;
  end;
  // Clear spring container.
  Springs.Clear;
end;

procedure TJSONMap.LoadOverlay(pImage: TARGBImage; pName: string);
var tmp:TARGBImage;
begin
  tmp:=TARGBImage.Create(Format('ovr_%s.png',[pName]));
  try
    pImage.PutImage(0,0,tmp,true);
  finally
    tmp.Free;
  end;
end;

function TJSONMap.GetString(JSON: TJSONData; Path: string; Default: string ): string;
begin
  if Assigned(JSON.FindPath(Path)) then
    Result:=JSON.FindPath(Path).AsString
  else
    Result:=Default;
end;

function TJSONMap.GetInteger(JSON: TJSONData; Path: string; Default: integer ): integer;
begin
  if Assigned(JSON.FindPath(Path)) then
    Result:=JSON.FindPath(Path).AsInteger
  else
    Result:=Default;
end;

function TJSONMap.GetFloat(JSON: TJSONData; Path: string; Default: double ): double;
begin
  if Assigned(JSON.FindPath(Path)) then
    Result:=JSON.FindPath(Path).AsFloat
  else
    Result:=Default;
end;

function TJSONMap.fGetMonsterData(index:integer):TMonsterData;
begin
  if (index>=0) and (index<length(fMonsters)) then
    Result:=fMonsters[index]
  else
    Result:=TMonsterData.Init(0,0,31,15,1,0);
end;

function TJSONMap.fGetMonsterCount:integer;
begin
  Result:=length(fMonsters);
end;

function TJSONMap.fGetDecorationData(index:integer):TDecorationData;
begin
  if (index>=0) and (index<length(fDecorations)) then
    Result:=fDecorations[index]
  else
    Result:=TDecorationData.Init(dtWindow,0,0);
end;

function TJSONMap.fGetDecorationCount:integer;
begin
  Result:=length(fDecorations);
end;

{ TMapList }

procedure TMapList.Load;
var i:integer;map:TJSONMap;
begin
  i:=1;
  while MKStreamOpener.FileExists(Format('maps\%.2d.json',[i])) do begin
    map:=TJSONMap.Create(i);
    Add(Format('%s=%s',[map.Name,map.Author]));
    map.Free;
    inc(i);
  end;
end;

function TMapList.fGetMapName(index:integer):string;
begin
  dec(index);  // Maps go 1..n, list goes 0..n-1
  if (index>=0) and (index<Count) then
    Result:=Names[index]
  else
    Result:='N/A';
end;

function TMapList.fGetAuthor(index:integer):string;
begin
  dec(index);  // Maps go 1..n, list goes 0..n-1
  if (index>=0) and (index<Count) then
    Result:=ValueFromIndex[index]
  else
    Result:='N/A';
end;

end.


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

unit FNSMap;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

interface

uses Classes, fgl;

type
  TBlockType=(btWall,btSpring,btIce,btZapper,btMud,btJumper,btPole,btStairsR,btStairsL,btPiece);

  { TBlockData }

  // Skeleton pieces are in order of head, ribs L/R, hips L/R, legs L/R
  TBlockData=record
    _type:TBlockType;
    _x,_y:integer;
    _length:integer;  // only used for btWall, btPole, btStairsR, btStairsL;
    constructor Init(iType:TBlockType;iX,iY:integer;iLength:integer=1);
  end;

  TBlockDataList=array of TBlockData;

  { TMonsterData }

  TMonsterData=record
    _row,
    _left,
    _right,
    _start,
    _speed,
    _imageindex:integer;
    constructor Init(iRow,iLeft,iRight,iStart,iSpeed,iImageIndex:integer);
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

  { TMap }

  TMap=class
    // Creates a new empty map.
    constructor Create; overload;
    // Creates map from file.
    constructor Create(iFilename:String); overload;
    // Creates map from stream.
    constructor Create(iStream:TStream); overload;
    // Saves map to file.
    procedure Save(pFilename:String); overload;
    // Saves map to stream.
    procedure Save(pStream:TStream); overload;
    // Add wall.
    procedure AddWall(pX,pY,pLength:integer);
    // Add spring.
    procedure AddSpring(pX,pY:integer);
    // Add ice.
    procedure AddIce(pX,pY:integer);
    // Add Zapper.
    procedure AddZapper(pX,pY:integer);
    // Add Mud.
    procedure AddMud(pX,pY:integer);
    // Add Jumper.
    procedure AddJumper(pX,pY:integer);
    // Add Pole.
    procedure AddPole(pX,pY,pLength:integer);
    // Add StairsRight.
    procedure AddStairsRight(pX,pY,pLength:integer);
    // Add StairsLeft.
    procedure AddStairsLeft(pX,pY,pLength:integer);
    // Add skeleton piece.
    procedure AddPiece(pX,pY:integer);
    // Add monster.
    procedure AddMonster(pRow,pLeft,pRight,pStart,pSpeed,pImageIndex:integer);
    // Add decoration.
    procedure AddDecoration(pType:TDecorationType;pX,pY:integer);
  private
    // Author name
    fAuthor:string;
    // Map name
    fName:String;
    // Wall block gfx index
    fWall:integer;
    // Map type (0-Construction, 1-Original interim, 2-Rebooted interim, 3-Congratulations)
    fType:integer;
    // Array of block data including walls
    fBlocks:TBlockDataList;
    // Array of monster data
    fMonsters:TMonsterDataList;
    // Array of decoration data
    fDecorations:TDecorationDataList;
    // Loads map data from stream
    procedure LoadMap(pStream:TStream);
    // Loads wall data from stream
    procedure LoadWalls(pStream:TStream);
    // Loads block data from stream
    procedure LoadBlocks(pStream:TStream);
    // Loads skeleton pieces data from stream
    procedure LoadSkeletonPieces(pStream:TStream);
    // Loads monsters data from stream
    procedure LoadMonsters(pStream:TStream);
    // Loads decoration data from stream
    procedure LoadDecorations(pStream:TStream);
    // Save wall data to stream
    procedure SaveWalls(pStream:TStream);
    // Save block data to stream
    procedure SaveBlocks(pStream:TStream);
    // Save skeleton data to stream
    procedure SaveSkeletonPieces(pStream:TStream);
    // Save monsters data to stream
    procedure SaveMonsters(pStream:TStream);
    // Save decoration data to stream
    procedure SaveDecorations(pStream:TStream);

    function fReadString(pStream:TStream):string;
    procedure fWriteString(pStream:TStream;s:String;head:boolean=true);
    function fReadByte(pStream:TStream):integer;
    procedure fWriteByte(pStream:TStream;i:integer);
    procedure fReadCoordLen(pStream:TStream;out x,y,len:integer);
    procedure fWriteCoordLen(pStream:TStream;x,y,len:integer);
    procedure fReadCoord(pStream:TStream;out x,y:integer);
    procedure fWriteCoord(pStream:TStream;x,y:integer);
    function fReadShortint(pStream:TStream):integer;
    function fGetMonsterData(index:integer):TMonsterData;
    function fGetMonsterCount:integer;
    function fGetDecorationData(index:integer):TDecorationData;
    function fGetDecorationCount:integer;
    function fGetBlockData(index:integer):TBlockData;
    function fGetBlockCount:integer;
  public
    property Name:string read fName write fName;
    property Author:string read fAuthor write fAuthor;
    property Wall:integer read fWall write fWall;
    property MapType:integer read fType write fType;
    property BlockData[index:integer]:TBlockData read fGetBlockData;
    property BlockCount:integer read fGetBlockCount;
    property MonsterData[index:integer]:TMonsterData read fGetMonsterData;
    property MonsterCount:integer read fGetMonsterCount;
    property DecorationData[index:integer]:TDecorationData read fGetDecorationData;
    property DecorationCount:integer read fGetDecorationCount;
  end;

  TMapListSpec=specialize TFPGObjectList<TMap>;

  TMapList=class(TMapListSpec)
    constructor Create(iFilename:String);
  end;

implementation

uses SysUtils, MKStream, MKToolBox, Logger, FNSShared;

const
  HEAD='FNSR';

{ TBlockData }

constructor TBlockData.Init(iType:TBlockType; iX,iY:integer; iLength:integer);
begin
  _type:=iType;
  _x:=iX;
  _y:=iY;
  _length:=iLength;
end;

{ TMonsterData }

constructor TMonsterData.Init(iRow,iLeft,iRight,iStart,iSpeed,iImageIndex:integer);
begin
  _row:=iRow;
  _left:=iLeft;
  _right:=iRight;
  _start:=iStart;
  _speed:=iSpeed;
  _imageindex:=iImageIndex;
end;

{ TDecorationData }

constructor TDecorationData.Init(iType:TDecorationType; iX,iY:integer);
begin
  _type:=iType;
  _x:=iX;
  _y:=iY;
end;

{ TMap }

constructor TMap.Create;
begin
  fAuthor:='Unknown';
  fName:='Not named';
  fWall:=0;
  fType:=0;
end;

constructor TMap.Create(iFilename:String);
var Stream:TStream;
begin
  try
    Stream:=MKStreamOpener.OpenStream(iFilename);
    Create(Stream);
  finally
    Stream.Free;
  end;
end;

constructor TMap.Create(iStream:TStream);
var s:String;
begin
  s:=#0#0#0#0;
  iStream.Read(s[1],4);
  if s<>HEAD then
    raise Exception.Create(Format('Not a Frank''N''Stein map (Bad FourCC)! (%s)',[s]));
  LoadMap(iStream);
end;

procedure TMap.Save(pFilename:String);
var Stream:TStream;
begin
  Stream:=TFileStream.Create(pFilename,fmCreate);
  try
    Save(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMap.Save(pStream:TStream);
begin
  fWriteString(pStream,'FNSR',false);
  fWriteString(pStream,fAuthor);
  fWriteString(pStream,fName);
  fWriteByte(pStream,(fType and 3)<<6+(fWall and $3F));

  SaveWalls(pStream);
  SaveBlocks(pStream);
  if MapType=0 then SaveSkeletonPieces(pStream);
  SaveMonsters(pStream);
  SaveDecorations(pStream);
end;

procedure TMap.AddWall(pX,pY,pLength:integer);
begin
  // Cut to map area
  if pX<0 then begin pLength+=pX;pX:=0;end;
  if pX>31 then begin pX:=31;pLength:=1;end;
  if (pX+pLength)>32 then pLength:=32-pX;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btWall,pX,pY,pLength);
end;

procedure TMap.AddSpring(pX,pY:integer);
begin
  // Check if inside map area
  if pX<0 then pX:=0;
  if pX>31 then pX:=31;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btSpring,pX,pY);
end;

procedure TMap.AddIce(pX,pY:integer);
begin
  // Check if inside map area
  if pX<0 then pX:=0;
  if pX>31 then pX:=31;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btIce,pX,pY);
end;

procedure TMap.AddZapper(pX,pY:integer);
begin
  // Check if inside map area
  if pX<0 then pX:=0;
  if pX>31 then pX:=31;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btZapper,pX,pY);
end;

procedure TMap.AddMud(pX,pY:integer);
begin
  // Check if inside map area
  if pX<0 then pX:=0;
  if pX>31 then pX:=31;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btMud,pX,pY);
end;

procedure TMap.AddJumper(pX,pY:integer);
begin
  // Check if inside map area
  if pX<0 then pX:=0;
  if pX>31 then pX:=31;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btJumper,pX,pY);
end;

procedure TMap.AddPole(pX,pY,pLength:integer);
begin
  // Cut to map area
  if pX<0 then pX:=0;
  if pX>31 then pX:=31;
  if pY<0 then begin pLength+=pY;pY:=0;end;
  if pY>21 then begin pY:=19;pLength:=3;end;
  if (pY+pLength)>22 then pLength:=22-pY;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btPole,pX,pY,pLength);
end;

procedure TMap.AddStairsRight(pX,pY,pLength:integer);
begin
  // Cut to map area
  if pX<0 then begin pLength+=pX;pX:=0;end;
  if pX>31 then begin pX:=31;pLength:=1;end;
  if (pX+pLength)>32 then pLength:=32-pX;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  if (py+pLength)>22 then pLength:=22-pY;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btStairsR,pX,pY,pLength);
end;

procedure TMap.AddStairsLeft(pX,pY,pLength:integer);
begin
  // Cut to map area
  if pX<0 then begin pLength+=pX;pX:=0;end;
  if pX>31 then begin pX:=31;pLength:=1;end;
  if (pX+pLength)>32 then pLength:=32-pX;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  if (py+pLength)>22 then pLength:=22-pY;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btStairsL,pX,pY,pLength);
end;

procedure TMap.AddPiece(pX,pY:integer);
begin
  // Check if inside map area
  if pX<0 then pX:=0;
  if pX>31 then pX:=31;
  if pY<0 then pY:=0;
  if pY>21 then pY:=21;
  SetLength(fBlocks,length(fBlocks)+1);
  fBlocks[length(fBlocks)-1]:=TBlockData.Init(btPiece,pX,pY);
end;

procedure TMap.AddMonster(pRow,pLeft,pRight,pStart,pSpeed,pImageIndex:integer);
begin
  SetLength(fMonsters,length(fMonsters)+1);
  if pSpeed>4 then pSpeed:=4
  else if pSpeed<-4 then pSpeed:=-4;
  fMonsters[length(fMonsters)-1]:=TMonsterData.Init(pRow,pLeft,pRight,pStart,pSpeed,pImageIndex);
end;

procedure TMap.AddDecoration(pType:TDecorationType; pX,pY:integer);
begin
  SetLength(fDecorations,Length(fDecorations)+1);
  fDecorations[length(fDecorations)-1]:=TDecorationData.Init(pType,pX,pY);
end;

procedure TMap.LoadMap(pStream:TStream);
begin
  fAuthor:=fReadString(pStream);
  fName:=fReadString(pStream);
  fWall:=fReadByte(pStream);
  fType:=(fWall and $c0)>>6;  // xx......
  fWall:=fWall and $3f;     // ..xxxxxx

  LoadWalls(pStream);
  LoadBlocks(pStream);
  if fType=0 then LoadSkeletonPieces(pStream);
  LoadMonsters(pStream);
  LoadDecorations(pStream);
end;

procedure TMap.LoadWalls(pStream:TStream);
var cnt,x,y,len:integer;
begin
  cnt:=fReadByte(pStream);
  while cnt>0 do begin
    fReadCoordLen(pStream,x,y,len);
//    Log.LogDebug(Format('Loadwall: %d,%d,%d',[x,y,len]));
    SetLength(fBlocks,length(fBlocks)+1);
    fBlocks[length(fBlocks)-1]:=TBlockData.Init(btWall,x,y,len);
    dec(cnt);
  end;
end;

procedure TMap.LoadBlocks(pStream:TStream);
var cnt,b,x,y,len:integer;
begin
  // Simple blocks
  cnt:=fReadByte(pStream);
  while cnt>0 do begin
    b:=fReadByte(pStream);
    fReadCoord(pStream,x,y);
//    Log.LogDebug(Format('Loadblock: %d,%d,%d',[b,x,y]));
    SetLength(fBlocks,length(fBlocks)+1);
    case b of
        // Spring
      1:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btSpring,x,y);
        // Ice
      2:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btIce,x,y);
        // '!' thingy (Zapper)
      3:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btZapper,x,y);
        // Mud
      4:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btMud,x,y);
        // 'J'umper
      5:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btJumper,x,y);
      else raise Exception(Format('Invalid simple block id! (%d)',[b]));
    end;
    dec(cnt);
  end;
  // Advanced blocks
  cnt:=fReadByte(pStream);
  while cnt>0 do begin
    b:=fReadByte(pStream);
    fReadCoordLen(pStream,x,y,len);
//    Log.LogDebug(Format('Loadblock: %d,%d,%d,%d',[b,x,y,len]));
    SetLength(fBlocks,length(fBlocks)+1);
    case b of
        // Sliding pole
      6:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btPole,x,y,len);
        // Stairs right
      7:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btStairsR,x,y,len);
        // Stairs left
      8:fBlocks[length(fBlocks)-1]:=TBlockData.Init(btStairsL,x,y,len);
      else raise Exception(Format('Invalid advanced block id! (%d)',[b]));
    end;
    dec(cnt);
  end;
end;

procedure TMap.LoadSkeletonPieces(pStream:TStream);
var i,x,y:integer;
begin
  for i:=0 to 6 do begin
    fReadCoord(pStream,x,y);
//    Log.LogDebug(Format('LoadPiece: %d,%d',[x,y]));
    SetLength(fBlocks,length(fBlocks)+1);
    fBlocks[length(fBlocks)-1]:=TBlockData.Init(btPiece,x,y);
  end;
end;

procedure TMap.LoadMonsters(pStream:TStream);
var cnt,i:integer;
begin
  cnt:=fReadByte(pStream);
  while cnt>0 do begin
    SetLength(fMonsters,length(fMonsters)+1);
    with fMonsters[length(fMonsters)-1] do begin
      i:=fReadByte(pStream);
      if i and 8=0 then
        _speed:=0-(i and 7)
      else
        _speed:=i and 7;
      fReadCoord(pStream,_start,_row);
      _left:=fReadByte(pStream);
      _right:=fReadByte(pStream);
      _imageindex:=fReadByte(pStream);
//      Log.LogDebug(Format('LoadMonster: %d (%d),%d,%d,%d,%d,%d',[_speed,i,_start,_row,_left,_right,_imageindex]));
    end;
    dec(cnt);
  end;
end;

procedure TMap.LoadDecorations(pStream:TStream);
var cnt,b,x,y:integer;dtype:TDecorationType;
begin
  cnt:=fReadByte(pStream);
  while cnt>0 do begin
    b:=fReadByte(pStream);
    if b=0 then dtype:=dtWindow
    else if b=1 then dtype:=dtDoor
    else raise Exception.Create(Format('Invalid decoration ID! (%d)',[b]));
    fReadCoord(pStream,x,y);
//    Log.LogDebug(Format('LoadDecoration: %d,%d,%d',[b,x,y]));
    SetLength(fDecorations,length(fDecorations)+1);
    fDecorations[length(fDecorations)-1]:=TDecorationData.Init(dtype,x,y);
    dec(cnt);
  end;
end;

procedure TMap.SaveWalls(pStream:TStream);
var i,cnt:integer;spos:int64;
begin
  cnt:=0;
  spos:=pStream.Position;
  fWriteByte(pStream,cnt);
  for i:=0 to length(fBlocks)-1 do
    if fBlocks[i]._type=btWall then begin
      fWriteCoordLen(pStream,fBlocks[i]._x,fBlocks[i]._y,fBlocks[i]._length);
//      Log.LogDebug(Format('Savewall: %d,%d,%d',[fBlocks[i]._x,fBlocks[i]._y,fBlocks[i]._length]));
      inc(cnt);
    end;
  pStream.Position:=spos;
  fWriteByte(pStream,cnt);
  pStream.Position:=pStream.Size;
end;

procedure TMap.SaveBlocks(pStream:TStream);
var i,cnt:integer;spos:int64;
begin
  // Simple blocks
  cnt:=0;
  spos:=pStream.Position;
  fWriteByte(pStream,cnt);
  for i:=0 to length(fBlocks)-1 do
    if (fBlocks[i]._type in [btSpring,btIce,btZapper,btMud,btJumper]) then begin
      fWriteByte(pStream,ord(fBlocks[i]._type));
      fWriteCoord(pStream,fBlocks[i]._x,fBlocks[i]._y);
      inc(cnt);
//      Log.LogDebug(Format('SaveSimpleBlock: %d,%d,%d',[ord(fBlocks[i]._type),fBlocks[i]._x,fBlocks[i]._y]));
    end;
  pStream.Position:=spos;
  fWriteByte(pStream,cnt);
  pStream.Position:=pStream.Size;

  // Advanced blocks
  cnt:=0;
  spos:=pStream.Position;
  fWriteByte(pStream,cnt);
  for i:=0 to length(fBlocks)-1 do
    if (fBlocks[i]._type in [btPole, btStairsR, btStairsL]) then begin
      fWriteByte(pStream,ord(fBlocks[i]._type));
      fWriteCoordLen(pStream,fBlocks[i]._x,fBlocks[i]._y,fBlocks[i]._length);
      inc(cnt);
//      Log.LogDebug(Format('SaveAdvancedBlock: %d,%d,%d,%d',[ord(fBlocks[i]._type),fBlocks[i]._x,fBlocks[i]._y,fBlocks[i]._length]));
    end;
  pStream.Position:=spos;
  fWriteByte(pStream,cnt);
  pStream.Position:=pStream.Size;
end;

procedure TMap.SaveSkeletonPieces(pStream:TStream);
var i,cnt:integer;
begin
  // Skeleton pieces
  cnt:=0;
  for i:=0 to length(fBlocks)-1 do
    if (fBlocks[i]._type=btPiece) and (cnt<7) then begin
      fWriteCoord(pStream,fBlocks[i]._x,fBlocks[i]._y);
//      Log.LogDebug(Format('SavePiece: %d,%d',[fBlocks[i]._x,fBlocks[i]._y]));
      inc(cnt);
    end;
  if cnt<7 then raise Exception.Create('Not enough skeleton pieces!');
end;

procedure TMap.SaveMonsters(pStream:TStream);
var i,b:integer;
begin
  fWriteByte(pStream,length(fMonsters));
  for i:=0 to length(fMonsters)-1 do with fMonsters[i] do begin
    if _speed<0 then
      b:=abs(_speed)
    else
      b:=8+_speed;
    fWriteByte(pStream,b);
    fWriteCoord(pStream,_start,_row);
    fWriteByte(pStream,_left);
    fWriteByte(pStream,_right);
    fWriteByte(pStream,_imageindex);
//    Log.LogDebug(Format('SaveMonster: %d (%d),%d,%d,%d,%d,%d',[_speed,b,_start,_row,_left,_right,_imageindex]));
  end;
end;

procedure TMap.SaveDecorations(pStream:TStream);
var i:integer;
begin
  fWriteByte(pStream,length(fDecorations));
  for i:=0 to length(fDecorations)-1 do begin
//    Log.LogDebug(Format('SaveDecoration: %d,%d,%d',[ord(fDecorations[i]._type),fDecorations[i]._x,fDecorations[i]._y]));
    fWriteByte(pStream,ord(fDecorations[i]._type));
    fWriteCoord(pStream,fDecorations[i]._x,fDecorations[i]._y);
  end;
end;

function TMap.fReadString(pStream:TStream):string;
var i:integer;
begin
  i:=fReadByte(pStream);
  SetLength(Result,i);
  pStream.Read(Result[1],i);
end;

function TMap.fReadByte(pStream:TStream):integer;
var b:Byte;
begin
  b:=0;
  pStream.Read(b,1);
  Result:=b;
end;

function TMap.fReadShortint(pStream:TStream):integer;
var i:shortint;
begin
  i:=0;
  pStream.Read(i,1);
  Result:=i;
end;

function TMap.fGetMonsterData(index:integer):TMonsterData;
begin
  if (index>=0) and (index<length(fMonsters)) then
    Result:=fMonsters[index]
  else
    Result:=TMonsterData.Init(0,0,31,15,1,0);
end;

function TMap.fGetMonsterCount:integer;
begin
  Result:=length(fMonsters);
end;

function TMap.fGetDecorationData(index:integer):TDecorationData;
begin
  if (index>=0) and (index<length(fDecorations)) then
    Result:=fDecorations[index]
  else
    Result:=TDecorationData.Init(dtWindow,0,0);
end;

function TMap.fGetDecorationCount:integer;
begin
  Result:=length(fDecorations);
end;

function TMap.fGetBlockData(index:integer):TBlockData;
begin
  if (index>=0) and (index<length(fBlocks)) then
    Result:=fBlocks[index]
  else
    Result:=TBlockData.Init(btWall,0,0,1);
end;

function TMap.fGetBlockCount:integer;
begin
  Result:=length(fBlocks);
end;

procedure TMap.fWriteString(pStream:TStream;s:String;head:boolean);
var i:integer;
begin
  i:=length(s);
  if head then pStream.Write(i,1);
  if i>0 then pStream.Write(s[1],i);
end;

procedure TMap.fWriteByte(pStream:TStream;i:integer);
begin
  pStream.Write(i,1);
end;

procedure TMap.fReadCoordLen(pStream:TStream; out x,y,len:integer);
begin
  x:=0;y:=0;len:=0;
  pStream.Read(x,1);
  pStream.Read(y,1);
  pStream.Read(len,1);
end;

procedure TMap.fWriteCoordLen(pStream:TStream; x,y,len:integer);
begin
  pStream.Write(x,1);
  pStream.Write(y,1);
  pStream.Write(len,1);
end;

procedure TMap.fReadCoord(pStream:TStream; out x,y:integer);
begin
  x:=0;y:=0;
  pStream.Read(x,1);
  pStream.Read(y,1);
end;

procedure TMap.fWriteCoord(pStream:TStream; x,y:integer);
begin
  pStream.Write(x,1);
  pStream.Write(y,1);
end;

// ----------------------------------------------------------- [ TMapList ] ---

constructor TMapList.Create(iFilename:String);
var Stream:TStream;tmpM:TMap;
begin
  inherited Create;
  Stream:=MKStreamOpener.OpenStream(iFilename);
  while Stream.Position<Stream.Size do begin
    Log.LogStatus(Format('Loading map %d...',[Count]));
    tmpM:=TMap.Create(Stream);
    Self.Add(tmpM);
  end;
  Stream.Free;
end;

end.


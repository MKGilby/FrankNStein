{
  Frank N Stein Refurbished - Map Converter tool - Copyright 2023-24 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

Program MapConverter;

{$mode Delphi}

uses Classes, SysUtils, MKToolBox, Logger, MKStream, FNSMap;

var
  Order:TStringList;
  Pack:TStream;
//  Deco:TStringList;

procedure ProcessMAP(ifn,ofn:string;maptype:integer);
const _from=' ?@A<>=;,3456789012:(*+-./!"#$%&'#39;
const   _to=' [=]\></J1234567^|v:!(){#}-------';
var t:text;s,tiles:string;i,j,l:integer;
    map:TMap;
    dtype:TDecorationType;

begin
  write('Processing ',ifn,'...');
  map:=TMap.Create;
  assign(t,ifn);
  reset(t);
  readln(t,s);
  while (copy(s,1,2)<>'AU') do readln(t,s);
  delete(s,1,2);
  if s='PSS' then s:='Colin Stewart';
  Map.Author:=s;
  readln(t,s);
  Map.Name:=copy(s,3,length(s)-2);  // Map Name
  write(1);
  while (copy(s,1,2)<>'MP') do readln(t,s);
  tiles:='';
  j:=0;
  write(2);
  while (j<22) and (copy(s,1,2)='MP') do begin
    delete(s,1,2);
    if ExtractFileExt(ifn)='.mpp' then
      for i:=1 to length(s) do if pos(s[i],_from)>0 then s[i]:=_to[pos(s[i],_from)];
    while length(s)<32 do s+=' ';
    tiles+=s;
    inc(j);
    readln(t,s);
  end;
  delete(s,1,2);
  map.Wall:=ord(s[1])-49;
  map.MapType:=maptype;
  write(3);

  // Walls
  s:=tiles;
  for i:=1 to length(s) do
    if s[i] in ['1'..'7',':','^','|','v','!','(',')','{','}','#'] then s[i]:='-';
  for j:=0 to 21 do
    for i:=0 to 31 do
      if s[j*32+i+1]='-' then begin
        s[j*32+i+1]:=' ';
        l:=1;
        while (i+l<32) and (s[j*32+i+1+l]='-') do begin
          if s[j*32+i+1+l]='-' then s[j*32+i+1+l]:=' ';
          inc(l);
        end;
        map.AddWall(i,j,l);
      end;

  // Blocks
  for j:=0 to 21 do begin
    for i:=0 to 31 do begin
      case tiles[j*32+i+1] of
        '-':begin
          tiles[j*32+i+1]:=' ';
//          l:=1;
//          while (i+l<32) and (tiles[j*32+i+1+l] in ['-','1'..'7',':','^','|','v','!','(',')','{','}','#']) do begin
//            if tiles[j*32+i+1+l]='-' then tiles[j*32+i+1+l]:=' ';
//            inc(l);
//          end;
//          Log.Trace(Format('Wall: %d,%d,%d',[i,j,l]));
//          map.AddWall(i,j,l);
        end;
        ':':begin
          tiles[j*32+i+1]:=' ';
          map.AddSpring(i,j);
        end;
        '(':begin
          tiles[j*32+i+1]:=' ';
          tiles[j*32+i+2]:=' ';
          map.AddIce(i,j);
        end;
        '!':begin
          tiles[j*32+i+1]:=' ';
          map.AddZapper(i,j);
        end;
        '{':begin
          tiles[j*32+i+1]:=' ';
          tiles[j*32+i+2]:=' ';
          tiles[j*32+i+3]:=' ';
          map.AddMud(i,j);
        end;
        'J':begin
          tiles[j*32+i+1]:=' ';
          map.AddJumper(i,j);
        end;
        '^':begin
          tiles[j*32+i+1]:=' ';
          l:=1;
          while (j+l<22) and (tiles[j*32+i+1+l*32] in ['|','v']) do begin
            tiles[j*32+i+1+l*32]:=' ';
            inc(l);
          end;
          map.AddPole(i,j,l);
        end;
        '\':begin
          tiles[j*32+i+1]:=' ';
          tiles[j*32+i+2]:=' ';
          l:=1;
          while (j+l<22) and (i+l<32) and (tiles[j*32+i+1+l+l*32]='\') do begin
            tiles[j*32+i+1+l+l*32]:=' ';
            tiles[j*32+i+1+l+l*32+1]:=' ';
            inc(l);
          end;
          map.AddStairsRight(i,j,l);
        end;
        '<':begin
          tiles[j*32+i+1]:=' ';
          tiles[j*32+i+2]:=' ';
          l:=1;
          while (j+l<22) and (i>=l) and (tiles[j*32+i+1-l+l*32]='<') do begin
            tiles[j*32+i+1-l+l*32]:=' ';
            tiles[j*32+i+1-l+l*32+1]:=' ';
            inc(l);
          end;
          map.AddStairsLeft(i,j,l);
        end;
      end;
    end;
  end;

  write(4);
  if maptype=0 then begin
    for l:=49 to 55 do
      for i:=0 to 22*32-1 do
        if tiles[i+1]=chr(l) then begin
          map.AddPiece(i mod 32,i div 32);
          break;
        end;
  end;

  write(5);
  // Monsters
  j:=0;
  readln(t,s);
  while (j<5) and (copy(s,1,2)<>'XX') do begin
    delete(s,1,2);
    if s<>'.' then begin
      map.AddMonster(
        strtoint(GetNthSegment(s,',',1)),
        strtoint(GetNthSegment(s,',',2)),
        strtoint(GetNthSegment(s,',',3)),
        strtoint(GetNthSegment(s,',',4)),
        strtoint(GetNthSegment(s,',',5)),
        strtoint(GetNthSegment(s,',',6))
      );
    end;
    inc(j);
    readln(t,s);
  end;

  write(6);
  // Decorations
  while (copy(s,1,2)<>'XX') do begin
    delete(s,1,2);
    if length(s)<100 then begin
      s:=uppercase(s);
      if GetNthSegment(s,',',1)='WINDOW' then dtype:=dtWindow
      else if GetNthSegment(s,',',1)='DOOR' then dtype:=dtDoor
      else raise Exception.Create(Format('Unknown decoration type! (%s)',[GetNthSegment(s,',',1)]));
      map.AddDecoration(
        dtype,
        strtoint(GetNthSegment(s,',',2)),
        strtoint(GetNthSegment(s,',',3))
      );
    end;
    readln(t,s);
  end;
  write(7);
//  map.Save(ofn);
  map.Save(Pack);
  map.Free;
{  Order.Add(ExtractFileName(ofn));
  write(8);
  map:=TMap.Create(ofn);
  write(9);
  map.Save(ChangeFileExt(ofn,'_2.bin'));
  write('A');
  map.Free;}
  closefile(t);
  writeln('...OK');
end;

begin
  MKStreamOpener.AddDirectory('.',100);
//  Deco:=LoadDeco;
  Order:=TStringList.Create;
  Pack:=TFileStream.Create('maps.bin',fmCreate);
  ProcessMAP('old\01.mpt','new\01.bin',0);
  ProcessMAP('old\26.mpt','new\int1.bin',1);
  ProcessMAP('old\02r.mpt','new\02.bin',0);
  ProcessMAP('old\int2.mpt','new\int2.bin',2);
  ProcessMAP('old\02.mpt','new\03.bin',0);
  ProcessMAP('old\03.mpt','new\04.bin',0);
  ProcessMAP('old\04.mpt','new\05.bin',0);
  ProcessMAP('old\06r.mpt','new\06.bin',0);
  ProcessMAP('old\05.mpt','new\07.bin',0);
  ProcessMAP('old\06.mpt','new\08.bin',0);
  ProcessMAP('old\07.mpt','new\09.bin',0);
  ProcessMAP('old\10r.mpt','new\10.bin',0);
  ProcessMAP('old\08.mpt','new\11.bin',0);
  ProcessMAP('old\09.mpt','new\12.bin',0);
  ProcessMAP('old\10.mpt','new\13.bin',0);
  ProcessMAP('old\14r.mpt','new\14.bin',0);
  ProcessMAP('old\11.mpt','new\15.bin',0);
  ProcessMAP('old\12.mpt','new\16.bin',0);
  ProcessMAP('old\13.mpt','new\17.bin',0);
  ProcessMAP('old\18r.mpt','new\18.bin',0);
  ProcessMAP('old\14.mpt','new\19.bin',0);
  ProcessMAP('old\15.mpt','new\20.bin',0);
  ProcessMAP('old\16.mpt','new\21.bin',0);
  ProcessMAP('old\22r.mpt','new\22.bin',0);
  ProcessMAP('old\17.mpt','new\23.bin',0);
  ProcessMAP('old\18.mpt','new\24.bin',0);
  ProcessMAP('old\19.mpt','new\25.bin',0);
  ProcessMAP('old\26r.mpt','new\26.bin',0);
  ProcessMAP('old\20.mpt','new\27.bin',0);
  ProcessMAP('old\21.mpt','new\28.bin',0);
  ProcessMAP('old\22.mpt','new\29.bin',0);
  ProcessMAP('old\30r.mpt','new\30.bin',0);
  ProcessMAP('old\23.mpt','new\31.bin',0);
  ProcessMAP('old\24.mpt','new\32.bin',0);
  ProcessMAP('old\25.mpt','new\33.bin',0);
  ProcessMAP('old\27.mpt','new\gratz.bin',3);
  FreeAndNil(Pack);
//  Order.SaveToFile('new\order.txt');
  FreeAndNil(Order);
//  FreeAndNil(Deco);
end.

{
  Start   Length   Descriptions
  0       4        FourCC (FMAP)
  4       1        Author length
 [.       .        Author]
  .       1        Map name length
 [.       .        Map name]
  .       1        Settings. 8 bits, ttwwwwww where
    tt - Map type (00-normal, 01-original interim, 10-rebooted interim, 11-congrats)
    wwwwww - Wall graphics tile index in Tiles.tga
    (Tile #255 will be changed to this and also needed to replace the skeleton
    piece when picked up)
  .       32*22    Map tiles (represented by tile index in Tiles.tga)
  .       1        Monster count
[ .       6        One monster data (row,left,right,start,dir/speed,imageindex)]

  If you change order in Tiles.tga, you have to check the _to constants in
  ProcessMPP and ProcessMPT, and also you need to make changes in the game
  source. (probably in Play1MapUnit)
}


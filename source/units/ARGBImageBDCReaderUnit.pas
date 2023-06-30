// BDC reader for TARGBImage (BurdockPaint CEL)
// ------------------------------------------------------------------
// You can freely distribute the sources
//
// Written by Gilby/MKSZTSZ
// Hungary, 2023
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2023.03.23
//     * Initial creation.
//   1.01 - Gilby - 2023.06.28
//     * Following file format changes.
//     * BUGFix: Memory corruption (buffer overflow)

unit ARGBImageBDCReaderUnit;

{$mode Delphi}

interface


implementation

uses Classes, SysUtils, ARGBImageUnit, AnimationDataUnit, FontDataUnit, Logger,
  MyZStreamUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.01';
  IMAGEDATAID=$49;
  PALETTEDATAID=$43;
  REGIONDATAID=$52;

var
  MaxPaletteEntries:integer;
  PaletteEntries:pointer;
  ImageWidth,ImageHeight:integer;
  ImageData:pointer;

procedure LoadFromStreamV1(Source:TStream);
var count:integer;
begin
  count:=0;
  Source.Read(count,2);
  MaxPaletteEntries:=count;
  PaletteEntries:=GetMem(MaxPaletteEntries*4);
  Source.Read(PaletteEntries^,MaxPaletteEntries*4);
end;

procedure LoadFromStreamV2(Source:TStream);
var i,j,count:integer;
begin
  count:=0;
  Source.Read(count,2);
  MaxPaletteEntries:=count;
  PaletteEntries:=GetMem(MaxPaletteEntries*4);
  for j:=0 to 3 do
    for i:=0 to MaxPaletteEntries-1 do
      Source.Read((PaletteEntries+i*4+j)^,1);
end;

procedure LoadFromStreamV3(Source:TStream);
var i,j,count:integer;b,b2:byte;
begin
  count:=0;
  Source.Read(count,2);
  MaxPaletteEntries:=count;
  PaletteEntries:=GetMem(MaxPaletteEntries*4);
  b2:=0;
  for j:=0 to 3 do begin
    b:=0;
    for i:=0 to MaxPaletteEntries-1 do begin
      Source.Read(b2,1);
      b:=(b+b2) and $ff;
      byte((PaletteEntries+i*4+j)^):=b;
    end;
  end;
end;

procedure ReadPAL(Source:TStream);
var size,curr:int64;b:byte;Xs:TMemoryStream;
begin
  b:=0;
  Source.Read(b,1);
  if b<>PALETTEDATAID then raise Exception.Create(Format('ID is not for palette data! (%.2x)',[b]));
  size:=0;
  Source.Read(Size,4);
  curr:=Source.Position;
  Xs:=TMemoryStream.Create;
  UnCompressStream(Source,Xs);
  Xs.Position:=0;
  b:=0;
  Xs.Read(b,1);
  if b=1 then LoadFromStreamV1(Xs)
  else if b=2 then LoadFromStreamV2(Xs)
  else if b=3 then LoadFromStreamV3(Xs)
  else raise Exception.Create(Format('Unknown palette data version! (%d)',[b]));
  FreeAndNil(Xs);
  Source.Position:=curr+size;
end;

procedure LoadWholeImageDataFromStream(Source:TStream);
var curr,size:int64;b:byte;Xs:TStream;
begin
  b:=0;
  Source.Read(b,1);
  if b<>REGIONDATAID then raise Exception.Create(Format('Region data ID expected (0x%.2x), got 0x%.2x!',[REGIONDATAID,b]));
  size:=0;
  Source.Read(size,4);
  curr:=Source.Position;
  Xs:=TMemoryStream.Create;
  UnCompressStream(Source,Xs);
  Source.Position:=curr+size;
  Xs.Position:=0;

  Xs.Read(b,1);
  if b<>1 then Exception.Create(Format('Unknown PixelData version! (%d)',[b]));

  ImageWidth:=0;ImageHeight:=0;
  Xs.Read(ImageWidth,2);
  Xs.Read(ImageWidth,2);
  Xs.Read(ImageWidth,2);
  Xs.Read(ImageHeight,2);
  ImageData:=Getmem(ImageWidth*ImageHeight*2);
  Xs.Read(ImageData^,ImageWidth*ImageHeight*2);
  FreeAndNil(Xs);
end;

procedure ReadBDC(pSource:TStream;out Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var size:int64;b:byte;i:integer;s,t:pointer;
begin
  b:=0;
  pSource.Read(b,1);
  if b<>IMAGEDATAID then raise Exception.Create('Not an image file!');
  size:=0;
  pSource.Read(size,4);
  pSource.Read(b,1);
  if b<>1 then raise Exception.Create(Format('Unknown image version! (%d)',[b]));
  ReadPal(pSource);
  LoadWholeImageDataFromStream(pSource);
  Width:=ImageWidth;
  Height:=ImageHeight;
  rawdata:=Getmem(Width*Height*4);
  t:=RawData;
  s:=ImageData;
  for i:=0 to Width*Height-1 do begin
    uint32(t^):=uint32((PaletteEntries+word(s^)*4)^);
    inc(s,2);
    inc(t,4);
  end;
  Freemem(PaletteEntries);
  Freemem(ImageData);
  FontData:=nil;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('BDC',@ReadBDC,true);

end.


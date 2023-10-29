unit FNSMapStatic;

{$mode ObjFPC}{$H+}

interface

uses
  mk_sdl2, ARGBImageUnit;

type

  { TMapStatic }

  TMapStatic=class
    constructor Create(iMapNo:integer);
    destructor Destroy; override;
    procedure Draw;
  private
    fTexture:TTexture;
    procedure FillBackWithStones(pImage:TARGBImage);
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
  if Maps[iMapNo].MapType=MAPTYPECONSTRUCTING then begin
    for i:=0 to 25 do
      MM.Images.ItemByName['Decorations'].CopyTo(80+(ord(platf[i+1])-49)*8,0,8,8,i*8,32,tmp,true)  // Top platform
  end;
  fTexture:=TStaticTexture.Create(tmp);
  tmp.Free;
end;

destructor TMapStatic.Destroy;
begin
  if assigned(fTexture) then fTexture.Free;
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

end.


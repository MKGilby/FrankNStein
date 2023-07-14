program TextureFont;

uses ARGBImageUnit, ARGBImagePNGReaderUnit, ARGBImagePNGWriterUnit, MKStream;

type

  { TMain }

  TMain=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fFontFace:TARGBImage;
  end;

{ TMain }

constructor TMain.Create;
begin
  MKStreamOpener.AddDirectory('.',0);
  fFontFace:=TARGBImage.Create('fontface.png');
end;

destructor TMain.Destroy;
begin
  if Assigned(fFontFace) then fFontFace.Free;
  inherited Destroy;
end;

procedure TMain.Run;
var Texture:TARGBImage;i,j,r:integer;
begin
  Texture:=TARGBImage.Create(fFontFace.Width,fFontFace.Height);
  for j:=0 to fFontFace.Height-1 do
    for i:=0 to fFontFace.Width-1 do begin
      r:=random(24)+212;
      Texture.PutPixel(i,j,r,r,r,255);
    end;
  for i:=0 to fFontFace.Width div 4-1 do begin
    r:=250+random(5);
    Texture.Putpixel(i*4+random(4),random(Texture.Height),r,r,r,255);
    r:=176+random(16);
    Texture.Putpixel(i*4+random(4),random(Texture.Height),r,r,r,255);
    if i mod 2=0 then begin
      r:=128+random(16);
      Texture.Putpixel(i*4+random(8),random(Texture.Height),r,r,r,255);
    end;
  end;
//  Texture.WriteFile('texture.png','PNG');
  for j:=0 to fFontFace.Height-1 do
    for i:=0 to fFontFace.Width-1 do
      if fFontFace.GetPixel(i,j)<>0 then fFontFace.PutPixel(i,j,Texture.GetPixel(i,j));
  fFontFace.WriteFile('font.png','PNG');
  Texture.Free;
end;

begin
  with TMain.Create do try Run; finally Free; end;
end.


program FontRip;

uses
  Classes,
  SysUtils,
  ARGBImageUnit,
  ARGBImageBMPReaderUnit,
  ARGBImagePNGWriterUnit,
  MKStream,
  MKToolbox,
  Logger;

const
  WantedChars:string='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-@./''';

type

  { TMain }

  TMain=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fFont:TARGBImage;
    fChars:string;
    procedure ProcessBMP(pFilename:string);
  end;

{ TMain }

constructor TMain.Create;
begin
  fFont:=TARGBImage.Create(length(WantedChars)*8,8);
  fChars:=WantedChars;
  MKStreamOpener.AddDirectory('.',0);
end;

destructor TMain.Destroy;
begin
  if Assigned(fFont) then begin
    fFont.WriteFile('orgfont.png','PNG');
    fFont.Free;
  end;
  inherited Destroy;
end;

procedure TMain.Run;
var i,j:integer;
begin
  ProcessBMP('origmenu');  // Don't write extension!
  ProcessBMP('orig357');  // Don't write extension!
  ProcessBMP('orig6');  // Don't write extension!
  Log.LogStatus('Missing chars: '+WantedChars);
  for j:=0 to 7 do
    for i:=0 to fFont.Width-1 do
      if fFont.GetPixel(i,j)<>$ff030303 then begin
//        Log.Trace(hexstr(fFont.GetPixel(i,j),8));
        fFont.PutPixel(i,j,255,255,255,255);
      end;
end;

procedure TMain.ProcessBMP(pFilename: string);
var txt:TStringList;img:TARGBImage;i,j:integer;
begin
  txt:=TStringList.Create;
  img:=TARGBImage.Create;
  try
    txt.LoadFromFile('data\'+pFilename+'.txt');
    img.ReadFile('data\'+pFilename+'.bmp');
    for j:=0 to txt.Count-1 do
      for i:=0 to length(txt[j])-1 do
        if pos(txt[j][i+1],WantedChars)>0 then begin
//          Log.Trace(txt[j][i+1]+', '+inttostr(pos(txt[j][i+1],fChars)-1));
//          k:=pos(txt[j][i+1],fChars)-1;
          img.CopyTo(i*8,j*8,8,8,(pos(txt[j][i+1],fChars)-1)*8,0,fFont);
          delete(WantedChars,pos(txt[j][i+1],WantedChars),1);
//          fFont.WriteFile(st(k,4,'0')+'.png','PNG');
        end;
  finally
    txt.Free;
    img.Free;
  end;
end;

begin
  with TMain.Create do try Run; finally Free; end;
end.


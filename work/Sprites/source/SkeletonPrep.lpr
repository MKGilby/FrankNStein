program SkeletonPrep;

uses SysUtils, ARGBImageUnit, ARGBImagePNGReaderUnit, TextureAtlasGeneratorUnit,
  AnimationDataUnit, MKStream, ARGBImagePNGWriterUnit;

type

  { TMain }

  TMain=class
    procedure Run;
  end;

{ TMain }

procedure TMain.Run;
var big:TARGBImage;atl:TTextureAtlasGenerator;

  procedure CreatePiece(x,y,order:integer);
  const SKBACK=$60000000;
  var tmp:TARGBImage;i,j:integer;
      tmpa:TFrameBasedAnimationData;
      TBA:TTimeBasedAnimationData;
  begin
    tmp:=TARGBImage.Create(20,8);
    tmp.Clear(0);
    tmp.bar(1,0,8,8,SKBACK);
    tmp.bar(11,0,8,8,SKBACK);
    big.CopyTo(x,y,8,8,1,0,tmp,true);
    big.CopyTo(x,y,8,8,11,0,tmp,true);
    for j:=0 to 7 do
      for i:=0 to 9 do
        if (tmp.GetPixel(i,j)=SKBACK) or (tmp.GetPixel(i,j)=0) then begin
          if ((i>1) and (big.GetPixel(x+i-2,y+j)<>0)) or
             ((i<8) and (big.GetPixel(x+i,y+j)<>0)) then begin
               tmp.putpixel(i,j,$ff400404);
               tmp.putpixel(i+10,j,$ff046004);
             end;
        end;
    TBA:=TTimeBasedAnimationData.Create(10,8);
    TBA.AddFrame(0,0);
    TBA.AddFrame(10,0);
    TBA.Paused:=true;
    TBA.StartFrame:=0;
    TBA.FPS:=2;
    TBA.Looped:=true;
    TBA.Name:=Format('Piece%d',[order]);
    tmp.Animations.AddObject(Format('Piece%d',[order]),TBA);
    atl.AddImage(tmp);

    tmpa:=TFrameBasedAnimationData.Create(8,8);
    tmpa.AddFrame(x,y);
    tmpa.Paused:=true;
    tmpa.Name:=Format('Skeleton%d',[order]);
    big.Animations.AddObject(Format('Skeleton%d',[order]),tmpa);
    tmp.Free;
  end;

begin
  atl:=TTextureAtlasGenerator.Create(320,200,1);
  big:=TARGBImage.Create('data\skeleton.png');
  big.SetColorkey(0,0,0);
  CreatePiece(4,0,1);
  CreatePiece(0,8,2);
  CreatePiece(8,8,3);
  CreatePiece(0,16,4);
  CreatePiece(8,16,5);
  CreatePiece(0,24,6);
  CreatePiece(8,24,7);
  atl.AddImage(big);
  atl.Crop;
  atl.TextureAtlas.WriteFile('skeletonsprite.png','PNG');
  big.Free;
  atl.Free;
end;

begin
  MKStreamOpener.AddDirectory('.',0);
  with TMain.Create do try Run; finally Free; end;
end.


{ -[Name]-------------------------------------------

            TextureAtlasGenerator class

  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2020-

  -[Description]------------------------------------

    You can add TARGBImages with animations to it, and
    it creates a TextureAtlas from them, with the specified
    padding.

    The result is a TARGBImage with the transformed
    animation data.

    You can feed it to a TMediaManager and simply create
    sprites from MediaManager.Animations.ItemByName['Player'].

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.07.09
//     * Initial creation
//  V1.01: Gilby - 2020.07.10
//     + LoadImage added (Loads image from file. PNGs with animations.)
//  V1.02: Gilby - 2022.03.18
//     * Following changes in AnimationDataUnit
//     * AddImage now have an optional second parameter to specify one animation
//       name to add.
//  V1.03: Gilby - 2022.07.19
//     * Leaving out duplicate frames within one image.
//  V1.04: Gilby - 2023.05.12
//     * Changing Lists.TGenericList to fgl.TFPGObjectList.

unit TextureAtlasGeneratorUnit;

{$mode delphi}

interface

uses
  Classes, ARGBImageUnit, fgl;

type

  { TTextureLine }

  TTextureLine=class
    constructor Create(iTop,iHeight,iMaxWidth,iPadding:integer);
    function IsTherePlaceFor(pWidth:integer):boolean;
    function AddImage(pWidth:integer):boolean;
  private
    fTop:integer;
    fHeight:integer;
    fPadding:integer;
    fCurrentLeft:integer;
    fMaxWidth:integer;
  public
    property CurrentLeft:integer read fCurrentLeft;
    property Height:integer read fHeight;
    property Top:integer read fTop;
  end;

  { TTextureLines }

  TTextureLines=class(TFPGObjectList<TTextureLine>)
    function SearchLine(pWidth,pHeight:integer):TTextureLine;
    function CurrentTop:integer;
  end;

  { TTextureAtlasGenerator }

  TTextureAtlasGenerator=class
    constructor Create(iWidth,iHeight,iPadding:integer);
    destructor Destroy; override;
    procedure AddImage(pImage:TARGBImage;pAnimationName:string='');
    procedure LoadImage(pFilename:string);
  private
    fTextureAtlas:TARGBImage;
    fLines:TTextureLines;
    fPadding:integer;
    fFreeTextureAtlas:boolean;
    function fGetTextureAtlas:TARGBImage;
  public
    property TextureAtlas:TARGBImage read fGetTextureAtlas;
    property FreeImage:boolean read fFreeTextureAtlas write fFreeTextureAtlas;
  end;

implementation

uses sysutils, AnimationDataUnit, Logger, MKToolbox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.04';

constructor TTextureLine.Create(iTop,iHeight,iMaxWidth,iPadding:integer);
begin
  fTop:=iTop;
  fHeight:=iHeight;
  fMaxWidth:=iMaxWidth;
  fPadding:=iPadding;
  fCurrentLeft:=fPadding;
end;

function TTextureLine.IsTherePlaceFor(pWidth:integer):boolean;
begin
  Result:=fCurrentLeft+pWidth+fPadding<=fMaxWidth;
end;

function TTextureLine.AddImage(pWidth:integer):boolean;
begin
  if IsTherePlaceFor(pWidth) then begin
    Result:=true;
    fCurrentLeft+=pWidth+fPadding;
  end else Result:=false;
end;

function TTextureLines.SearchLine(pWidth,pHeight:integer):TTextureLine;
var i:integer;
begin
  for i:=0 to Count-1 do
    if (Self[i].Height=pHeight) and (Self[i].IsTherePlaceFor(pWidth)) then begin
      Result:=Self[i];
      exit;
    end;
  Result:=nil;
end;

function TTextureLines.CurrentTop:integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if Self[i].Top+Self[i].Height>Result then Result:=Self[i].Top+Self[i].Height;
end;

constructor TTextureAtlasGenerator.Create(iWidth,iHeight,iPadding:integer);
begin
  fTextureAtlas:=TARGBImage.Create(iWidth,iHeight);
  fTextureAtlas.Bar(0,0,iWidth,iHeight,0,0,0,0);
  fLines:=TTextureLines.Create;
  fPadding:=iPadding;
  fFreeTextureAtlas:=true;
end;

destructor TTextureAtlasGenerator.Destroy;
begin
  FreeAndNil(fLines);
  if fFreeTextureAtlas then FreeAndNil(fTextureAtlas);
  inherited ;
end;

function TTextureAtlasGenerator.fGetTextureAtlas:TARGBImage;
begin
  Result:=fTextureAtlas;
//  fFreeTextureAtlas:=false;
end;

procedure TTextureAtlasGenerator.AddImage(pImage: TARGBImage; pAnimationName: string);
var anim,frame:integer;atm:TAnimationData;Line:TTextureLine;
  PrevFrames:TStringList;key:string;
begin
  PrevFrames:=TStringList.Create;
  try
    for anim:=0 to pImage.Animations.Count-1 do begin
      if (pAnimationName='') or (pImage.Animations[anim].Name=pAnimationName) then begin
        atm:=TAnimationData.Create(pImage.Animations[anim].Width,pImage.Animations[anim].Height);
        atm.Name:=pImage.Animations[anim].Name;
        atm.FrameDelay:=pImage.Animations[anim].FrameDelay;
        atm.StartFrame:=pImage.Animations[anim].StartFrame;
        atm.LoopDelay:=pImage.Animations[anim].LoopDelay;
        atm.Looped:=pImage.Animations[anim].Looped;
        atm.RandomStart:=pImage.Animations[anim].RandomStart;
        atm.Paused:=pImage.Animations[anim].Paused;
        atm.PingPong:=pImage.Animations[anim].PingPong;
        atm.ReverseAnim:=pImage.Animations[anim].ReverseAnim;

        for frame:=0 to pImage.Animations[anim].FrameCount-1 do begin
          key:=Format('%d,%d',[pImage.Animations[anim].Frames[frame].x,pImage.Animations[anim].Frames[frame].y]);
          if PrevFrames.Values[key]='' then begin
            Line:=fLines.SearchLine(atm.Width,atm.Height);
            if Line=nil then begin
              Line:=TTextureLine.Create(fLines.CurrentTop+fPadding,atm.Height,fTextureAtlas.Width,fPadding);
              fLines.Add(Line);
            end;
            atm.AddFrame(Line.CurrentLeft,Line.Top);
            with pImage.Animations[anim] do
              pImage.CopyTo(Frames[frame].x,Frames[frame].y,atm.Width,atm.Height,Line.CurrentLeft,Line.Top,fTextureAtlas);
            PrevFrames.Add(Format('%s=%d,%d',[key,Line.CurrentLeft,Line.Top]));
            Line.AddImage(atm.Width);
          end else begin
            key:=PrevFrames.Values[key];
            atm.AddFrame(strtoint(GetNthSegment(key,',',1)),strtoint(GetNthSegment(key,',',2)));
          end;
        end;
        fTextureAtlas.Animations.AddObject(atm.Name,atm);
      end;
    end;

  finally
    PrevFrames.Free;
  end;
end;

procedure TTextureAtlasGenerator.LoadImage(pFilename:string);
var image:TARGBImage;
begin
  image:=TARGBImage.Create(pFilename);
  AddImage(image);
  FreeAndNil(image);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.


{ -[Name]-------------------------------------------

     MKSZTSZ Animated Sprite class for SDL2
                  using TAnimation for animations
           
  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2020-2022

  -[Description]------------------------------------

    [to be written]

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.02.10
//     - Initial creation from AnimatedSpriteUnit.
//  V1.01: Gilby - 2020.06.25
//     * Following changes in AnimationDataUnit.
//  V1.02: Gilby - 2021.03.03
//     * SetAnimation is now sets width and height according to new animation.
//     + Using HotPoint info in Draw. Example:
//       X,Y=32,32  HotPointX,HotPointY=3,5  Sprite will be drawn at 29,27
//  V1.03: Gilby - 2022.05.17
//     * Setting framedelay and loopdelay did not update current framedelaycount
//       and loopdelaycount
//  V1.04: Gilby - 2022.06.23
//     * Animation logic moved to TAnimation. (It means to create two animated
//       sprites with not the same animation phase you have to create two
//       animations.)
//       Animation only advances when you call TAnimation.Animate! (not automatically)
//  V1.05: Gilby - 2022.12.13
//     * LogSprite doesn't fail when no animation is assigned yet.
//  V1.06: Gilby - 2023.12.14
//     * Following changes in Animation2Unit

{$ifdef fpc}
  {$smartlink on}
  {$mode delphi}
{$endif}

unit AnimatedSprite2Unit;

interface

uses LogicalSpriteUnit, Animation2Unit;

type

  { TAnimatedSprite }

  TAnimatedSprite=class(TLogicalSprite)
    constructor Create(iX,iY:integer;iAnimation:TAnimation); overload;
    procedure SetAnimation(pAnimation:TAnimation;pResetFrameIndex:boolean=false);
    procedure Draw; override;
    procedure LogSpriteData; virtual;
  protected
    fAnimation:TAnimation;
  public
    property Animation:TAnimation read fAnimation;
  end;

//var
//  DefaultSpriteTargetImage:TImage;

implementation

uses Logger, SysUtils, MK_SDL2;

const
  Fstr={$I %FILE%}+', ';
  Version='1.06';

constructor TAnimatedSprite.Create(iX,iY:integer;iAnimation:TAnimation);
begin
  inherited Create(iX,iY,iAnimation.Timer.Width,iAnimation.Timer.Height);
  fAnimation:=iAnimation;
  fName:=iAnimation.Name;
end;

procedure TAnimatedSprite.SetAnimation(pAnimation:TAnimation;pResetFrameIndex:boolean=false);
begin
  if Assigned(pAnimation) then begin
    fAnimation:=pAnimation;
    fWidth:=fAnimation.Timer.Width;
    fHeight:=fAnimation.Timer.Height;
    if pResetFrameIndex then fAnimation.Timer.ResetFrameIndex;
  end
{$ifdef debug}
    else raise Exception.Create('NIL animation passed!');
{$endif}
end;

procedure TAnimatedSprite.Draw;
begin
  if not(fDead or not fVisible) then fAnimation.PutFrame(fX,fY);
end;

procedure TAnimatedSprite.LogSpriteData;
var s:String;
begin
  Log.LogDebug('------ AnimatedSprite data start ------');
  Log.LogDebug('Name: '+fName);
  Log.LogDebug('Position: '+inttostr(fX)+', '+inttostr(fY));
  Log.LogDebug('Z-Order: '+inttostr(fZOrder));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  s:='[';
  if fVisible then s+='X' else s+=' ';
  s+='] Visible   [';
  if fDead then s+='X' else s+=' ';
  s+='] Dead';
  Log.LogDebug(s);
  if Assigned (fAnimation) then fAnimation.LogData
  else Log.LogDebug('Sprite has no animation.');
  Log.LogDebug('------- Sprite data end -------');

end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
//  DefaultSpriteTargetImage:=nil;

end.

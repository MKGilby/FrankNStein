{ -[Name]-------------------------------------------

      TAnimationData class. Base for TAnimation.

  -[Disclaimer]-------------------------------------

    See copyright.txt in project sources.

    Written by Gilby/MKSZTSZ   Hungary, 2020-

  -[Description]------------------------------------

    Contains animation data, except image.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.03.16
//     * Initial creation from Animation2Unit
//  V1.01: Gilby - 2020.03.22
//     + Added StartFrame
//  V1.02: Gilby - 2020.04.01
//     + Added TAnimationDatas type
//  V1.03: Gilby - 2020.04.01
//     + Name property added
//     * FrameCount is now equals the count of added frames,
//       thus not needed at Create.
//  V1.04: Gilby - 2021.03.03
//     * Added HotPointX and HotPointY property. It defines the hot point of
//       the animation, that will be at the x,y point of the sprite (if used in
//       AnimatedSprite) (effectively shifting the image -HotPointX,-HotPointY
//       pixels).
//  V1.05: Gilby - 2021.04.22
//     + Added LogData
//  V1.05a: Gilby - 2021.11.09
//     - Removed MKStream from uses.
//  V1.06: Gilby - 2022.03.18
//     * TAnimationDatas is now TNamedList (to be searchable for names).
//  V1.07: Gilby - 2022.06.23
//     + Added ReverseAnim.
//  V1.08: Gilby - 2022.07.19
//     + Added animation flag constants
//     + Added logging of animation flags
//  V1.09: Gilby - 2023.12.13-14
//     * Reworked to support Time-based animations.
//     * Can be created directly from stream.
//     * Can be saved to stream.
//       - Frame-based version: 1
//       - Time-based version: 2
//  V1.10: Gilby - 2023.12.29
//     + Added LogData to base class.
//     + Added SkipFrames parameter to Clone.

{$mode delphi}

unit AnimationDataUnit;

interface

uses
  Classes, Lists;

const
  AF_LOOPED=1;
  AF_RANDOMSTART=2;
  AF_PAUSED=4;
  AF_PINGPONG=8;
  AF_REVERSEANIM=16;
  AF_TIMEBASED=32;

type

  { TBaseAnimationData }

  TBaseAnimationData=class abstract
    constructor Create(iWidth,iHeight:integer);  virtual;
    procedure AddFrame(pX,pY:integer);
    procedure SavetoStream(pStream:TStream); virtual;
    procedure LogData; virtual; abstract;
  private
    function fGetFrame(index:integer):TRect;
  protected
    fWidth,fHeight:integer;
    fFrames:array of TRect;
    function fGetFrameCount:integer;
  public
    HotPointX:integer;
    HotPointY:integer;
    StartFrame:integer;
    Name:string;
    RandomStart:boolean;
    ReverseAnim:boolean;
    Paused:boolean;
    PingPong:boolean;
    Looped:boolean;
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property FrameCount:integer read fGetFrameCount;
    property Frames[index:integer]:TRect read fGetFrame;
  end;

  { TFrameBasedAnimationData }

  TFrameBasedAnimationData=class(TBaseAnimationData)
    constructor Create(iWidth,iHeight:integer);  override;
    constructor CreateFromStreamLegacy(iStream:TStream);
    procedure SavetoStream(pStream:TStream); override;
    procedure LogData; override;
    function Clone(pSkipFrames:boolean=false):TFrameBasedAnimationData;
  public
    FrameDelay:integer;
    LoopDelay:integer;
  end;

  { TTimeBasedAnimationData }

  TTimeBasedAnimationData=class(TBaseAnimationData)
    constructor Create(iWidth,iHeight:integer);  override;
    constructor CreateFromStreamV2(iStream:TStream);
    procedure SavetoStream(pStream:TStream); override;
    procedure LogData; override;
    function Clone(pSkipFrames:boolean=false):TTimeBasedAnimationData;
  public
    FPS:double;  // to allow 1.5 frames per sec = 3 frames/2 sec
    LoopDelay:double;  // in seconds
  end;

  TAnimationDatas=TNamedList<TBaseAnimationData>;

implementation

uses SysUtils, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.10';

// -------------------------------------------------------[ TBaseAnimationData ]---

constructor TBaseAnimationData.Create(iWidth,iHeight:integer);
begin
  fWidth:=iWidth;
  fHeight:=iHeight;
  HotPointX:=0;
  HotPointY:=0;
  StartFrame:=0;
  Looped:=false;
  RandomStart:=false;
  Paused:=false;
  PingPong:=false;
  SetLength(fFrames,0);
  Name:='';
end;

procedure TBaseAnimationData.AddFrame(pX,pY:integer);
begin
  SetLength(fFrames,length(fFrames)+1);
  with fFrames[length(fFrames)-1] do begin
    Left:=pX;
    Top:=pY;
    Width:=fWidth;
    Height:=fHeight;
  end;
end;

procedure TBaseAnimationData.SavetoStream(pStream:TStream);
begin
  raise Exception.Create('SaveToStream is not supported!');
end;

function TBaseAnimationData.fGetFrameCount:integer;
begin
  Result:=length(fFrames);
end;

function TBaseAnimationData.fGetFrame(index:integer):TRect;
begin
  if (index>=0) and (index<length(fFrames)) then
    Result:=fFrames[index]
  else begin
    Result.Left:=0;
    Result.Top:=0;
    Result.Width:=1;
    Result.Height:=1;
  end;
end;

{ TFrameBasedAnimationData }

constructor TFrameBasedAnimationData.Create(iWidth,iHeight:integer);
begin
  inherited Create(iWidth,iHeight);
  FrameDelay:=0;
  LoopDelay:=0;
end;

constructor TFrameBasedAnimationData.CreateFromStreamLegacy(iStream:TStream);
var i,w,h:integer;flags:byte;
begin
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);

  i:=0;
  iStream.Read(i,2);
  FrameDelay:=0;
  iStream.Read(FrameDelay,2);
  LoopDelay:=0;
  iStream.Read(LoopDelay,2);
  StartFrame:=0;
  iStream.Read(StartFrame,2);

  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;

  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;
end;

procedure TFrameBasedAnimationData.SavetoStream(pStream:TStream);
var b:Byte;i:integer;
begin
  b:=1;
  pStream.Write(b,1);  // Version, indicates Frame-basedness too
  b:=length(Name);
  pStream.Write(b,1);
  if b>0 then pStream.Write(Name[1],b);

  pStream.Write(Width,2);
  pStream.Write(Height,2);
  pStream.Write(FrameCount,2);
  pStream.Write(FrameDelay,2);
  pStream.Write(LoopDelay,2);
  pStream.Write(StartFrame,2);
  b:=0;
  if Looped then b:=b or AF_LOOPED;
  if RandomStart then b:=b or AF_RANDOMSTART;
  if Paused then b:=b or AF_PAUSED;
  if PingPong then b:=b or AF_PINGPONG;
  if ReverseAnim then b:=b or AF_REVERSEANIM;
  pStream.Write(b,1);
  for i:=0 to FrameCount-1 do begin
    pStream.Write(Frames[i].Left,2);
    pStream.Write(Frames[i].Top,2);
  end;
end;

procedure TFrameBasedAnimationData.LogData;
var i:integer;s:string;
begin
  Log.LogDebug('--- AnimationData logging starts ---');
  Log.LogDebug(Format('Name: %s',[name]));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  Log.LogDebug('Type: Frame-based');
  Log.LogDebug(Format('Hotpoint: %d, %d',[HotPointX,HotPointY]));
  Log.LogDebug(Format('Frame and loopdelay: %d, %d',[FrameDelay,LoopDelay]));
  Log.LogDebug(Format('Framecount: %d',[length(fFrames)]));
  Log.LogDebug(Format('StartFrame: %d',[StartFrame]));
  s:='     ';
  if Looped then s[1]:='X';
  if RandomStart then s[2]:='X';
  if Paused then s[3]:='X';
  if PingPong then s[4]:='X';
  if ReverseAnim then s[5]:='X';
  Log.LogDebug('Looped ['+s[1]+']  RandomStart ['+s[2]+']  Paused ['+s[3]+']  PingPong ['+s[4]+']  ReverseAnim ['+s[5]+']');
  Log.LogDebug('Frames:');
  for i:=0 to length(fFrames)-1 do with fFrames[i] do
    Log.LogDebug(Format('  %d. x=%d, y=%d, w=%d, h=%d',[i,Left,Top,Width,Height]));
end;

function TFrameBasedAnimationData.Clone(pSkipFrames:boolean):TFrameBasedAnimationData;
var i:integer;
begin
  Result:=TFrameBasedAnimationData.Create(fWidth,fHeight);
  Result.Name:=Name;
  Result.FrameDelay:=FrameDelay;
  Result.LoopDelay:=LoopDelay;
  Result.StartFrame:=StartFrame;
  Result.Looped:=Looped;
  Result.RandomStart:=RandomStart;
  Result.Paused:=Paused;
  Result.PingPong:=PingPong;
  Result.ReverseAnim:=ReverseAnim;
  if not pSkipFrames then
    for i:=0 to FrameCount-1 do Result.AddFrame(Frames[i].Left,Frames[i].Top);
end;

{ TTimeBasedAnimationData }

constructor TTimeBasedAnimationData.Create(iWidth,iHeight:integer);
begin
  inherited Create(iWidth,iHeight);
  FPS:=1;
end;

constructor TTimeBasedAnimationData.CreateFromStreamV2(iStream:TStream);
var i,w,h:integer;flags:byte;
begin
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);

  i:=0;
  iStream.Read(i,2);
  FPS:=0;
  iStream.Read(FPS,sizeof(FPS));
  LoopDelay:=0;
  iStream.Read(LoopDelay,sizeof(LoopDelay));
  StartFrame:=0;
  iStream.Read(StartFrame,2);

  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;

  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;
end;

procedure TTimeBasedAnimationData.SavetoStream(pStream:TStream);
var b:Byte;i:integer;
begin
  b:=2;
  pStream.Write(b,1);  // Version, indicates Time-basedness too
  b:=length(Name);
  pStream.Write(b,1);
  if b>0 then pStream.Write(Name[1],b);

  pStream.Write(Width,2);
  pStream.Write(Height,2);
  pStream.Write(FrameCount,2);
  pStream.Write(FPS,sizeof(double));
  pStream.Write(LoopDelay,sizeof(double));
  pStream.Write(StartFrame,2);
  b:=0;
  if Looped then b:=b or AF_LOOPED;
  if RandomStart then b:=b or AF_RANDOMSTART;
  if Paused then b:=b or AF_PAUSED;
  if PingPong then b:=b or AF_PINGPONG;
  if ReverseAnim then b:=b or AF_REVERSEANIM;
  pStream.Write(b,1);
  for i:=0 to FrameCount-1 do begin
    pStream.Write(Frames[i].Left,2);
    pStream.Write(Frames[i].Top,2);
  end;
end;

procedure TTimeBasedAnimationData.LogData;
var s:string;i:integer;
begin
  Log.LogDebug('--- AnimationData logging starts ---');
  Log.LogDebug(Format('Name: %s',[name]));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  Log.LogDebug('Type: Time-based');
  Log.LogDebug(Format('Hotpoint: %d, %d',[HotPointX,HotPointY]));
  Log.LogDebug(Format('Frame/sec: %.2f',[FPS]));
  Log.LogDebug(Format('Loopdelay: %.2f secs',[LoopDelay]));
  Log.LogDebug(Format('Framecount: %d',[length(fFrames)]));
  Log.LogDebug(Format('StartFrame: %d',[StartFrame]));
  s:='     ';
  if Looped then s[1]:='X';
  if RandomStart then s[2]:='X';
  if Paused then s[3]:='X';
  if PingPong then s[4]:='X';
  if ReverseAnim then s[5]:='X';
  Log.LogDebug('Looped ['+s[1]+']  RandomStart ['+s[2]+']  Paused ['+s[3]+']  PingPong ['+s[4]+']  ReverseAnim ['+s[5]+']');
  Log.LogDebug('Frames:');
  for i:=0 to length(fFrames)-1 do with fFrames[i] do
    Log.LogDebug(Format('  %d. x=%d, y=%d, w=%d, h=%d',[i,Left,Top,Width,Height]));
end;

function TTimeBasedAnimationData.Clone(pSkipFrames:boolean):TTimeBasedAnimationData;
var i:integer;
begin
  Result:=TTimeBasedAnimationData.Create(fWidth,fHeight);
  Result.Name:=Name;
  Result.FPS:=FPS;
  Result.LoopDelay:=LoopDelay;
  Result.StartFrame:=StartFrame;
  Result.Looped:=Looped;
  Result.RandomStart:=RandomStart;
  Result.Paused:=Paused;
  Result.PingPong:=PingPong;
  Result.ReverseAnim:=ReverseAnim;
  if not pSkipFrames then
    for i:=0 to FrameCount-1 do Result.AddFrame(Frames[i].Left,Frames[i].Top);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.


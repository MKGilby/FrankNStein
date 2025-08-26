// MKSZTSZ JSON writer for TARGBImage
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 3.
//
// Written by Szabó "Gilby" Zsolt/MKSZTSZ               Hungary, 2025
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2025.08.07
//     * Initial creation

unit ARGBImageJSONWriterUnit;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

implementation

uses
  Classes, SysUtils, ARGBImageUnit, Logger, AnimationDataUnit, FontDataUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

procedure WriteLine(pTarget:TStream;pLine:string);
const SPACES='                                ';
begin
  pLine:=pLine+LineEnding;
  if ord(pLine[1]) in [1..31] then begin
    pLine:=copy(SPACES,1,ord(pLine[1]))+copy(pLine,2);
  end;
  pTarget.Write(pLine[1],length(pLine));
end;

procedure WriteTimeBasedAnimToJSon(pAnimationData:TTimeBasedAnimationData;pTarget:TStream;pIsLast:boolean);
var s:string;i:integer;
begin
  WriteLine(pTarget,#4'{');
  WriteLine(pTarget,#6'"Type" : "TimeBased",');
  WriteLine(pTarget,Format(#6'"Name" : "%s",',[pAnimationData.Name]));
  WriteLine(pTarget,Format(#6'"Width" : %d,',[pAnimationData.Width]));
  WriteLine(pTarget,Format(#6'"Height" : %d,',[pAnimationData.Height]));
  WriteLine(pTarget,Format(#6'"FPS" : %s,',[StringReplace(FloatToStr(pAnimationData.FPS),',','.',[rfReplaceAll])]));
  if pAnimationData.PPS<>1 then
    WriteLine(pTarget,Format(#6'"PPS" : %s,',[StringReplace(FloatToStr(pAnimationData.PPS),',','.',[rfReplaceAll])]));
  WriteLine(pTarget,Format(#6'"StartFrame" : %d,',[pAnimationData.StartFrame]));
  if pAnimationData.LoopDelay<>0 then
    WriteLine(pTarget,Format(#6'"LoopDelay" : %s,',[StringReplace(FloatToStr(pAnimationData.LoopDelay),',','.',[rfReplaceAll])]));
  if (pAnimationData.HotPointX<>0) or (pAnimationData.HotPointY<>0) then
    WriteLine(pTarget,Format(#6'"Hotpoint" : { "X" : %d, "Y" : %d },',[pAnimationData.HotPointX,pAnimationData.HotPointY]));
  WriteLine(pTarget,Format(#6'"Looped" : "%s",',[BoolToStr(pAnimationData.Looped,true)]));
  WriteLine(pTarget,Format(#6'"Paused" : "%s",',[BoolToStr(pAnimationData.Paused,true)]));
  if pAnimationData.RandomStart then
    WriteLine(pTarget,#6'"RandomStart" : "True",');
  if pAnimationData.PingPong then
    WriteLine(pTarget,#6'"Pingpong" : "True",');
  if pAnimationData.ReverseAnim then
    WriteLine(pTarget,#6'"Reversed" : "True",');
  WriteLine(pTarget,#6'"Frames" :');
  WriteLine(pTarget,#8'[');
  for i:=0 to pAnimationData.FrameCount-1 do begin
    if i<pAnimationData.FrameCount-1 then s:=',' else s:='';
    WriteLine(pTarget,Format(#10'{ "X" : %d, "Y" : %d }%s',[pAnimationData.Frames[i].Left,pAnimationData.Frames[i].Top,s]));
  end;
  WriteLine(pTarget,#8']');
  if not pIsLast then
    WriteLine(pTarget,#4'},')
  else
    WriteLine(pTarget,#4'}');
end;

procedure WriteFrameBasedAnimToJSon(pAnimationData:TFrameBasedAnimationData;pTarget:TStream;pIsLast:boolean);
var s:string;i:integer;
begin
  WriteLine(pTarget,#4'{');
  WriteLine(pTarget,#6'"Type" : "FrameBased",');
  WriteLine(pTarget,Format(#6'"Name" : "%s",',[pAnimationData.Name]));
  WriteLine(pTarget,Format(#6'"Width" : %d,',[pAnimationData.Width]));
  WriteLine(pTarget,Format(#6'"Height" : %d,',[pAnimationData.Height]));
  WriteLine(pTarget,Format(#6'"FrameDelay" : %d,',[pAnimationData.FrameDelay]));
  if pAnimationData.LoopDelay<>0 then
    WriteLine(pTarget,Format(#6'"LoopDelay" : %d,',[pAnimationData.LoopDelay]));
  WriteLine(pTarget,Format(#6'"StartFrame" : %d,',[pAnimationData.StartFrame]));
  if (pAnimationData.HotPointX<>0) or (pAnimationData.HotPointY<>0) then
    WriteLine(pTarget,Format(#6'"Hotpoint" : { "X" : %d, "Y" : %d },',[pAnimationData.HotPointX,pAnimationData.HotPointY]));
  WriteLine(pTarget,Format(#6'"Looped" : "%s",',[BoolToStr(pAnimationData.Looped,true)]));
  WriteLine(pTarget,Format(#6'"Paused" : "%s",',[BoolToStr(pAnimationData.Paused,true)]));
  if pAnimationData.RandomStart then
    WriteLine(pTarget,#6'"RandomStart" : "True",');
  if pAnimationData.PingPong then
    WriteLine(pTarget,#6'"Pingpong" : "True",');
  if pAnimationData.ReverseAnim then
    WriteLine(pTarget,#6'"Reversed" : "True",');
  WriteLine(pTarget,#6'"Frames" :');
  WriteLine(pTarget,#8'[');
  for i:=0 to pAnimationData.FrameCount-1 do begin
    if i<pAnimationData.FrameCount-1 then s:=',' else s:='';
    WriteLine(pTarget,Format(#10'{ "X" : %d, "Y" : %d }%s',[pAnimationData.Frames[i].Left,pAnimationData.Frames[i].Top,s]));
  end;
  WriteLine(pTarget,#8']');
  if not pIsLast then
    WriteLine(pTarget,#4'},')
  else
    WriteLine(pTarget,#4'}');
end;

procedure WriteJSON(pTarget:TStream;pWidth,pHeight:integer;pRawData:pointer;pAnimations:TAnimationDatas;pFontData:TFontData);
var i:integer;w:boolean;
begin
  if pAnimations.Count>0 then begin
    WriteLine(pTarget,'{ "Animations" :');
    WriteLine(pTarget,#2'[');
    for i:=0 to pAnimations.Count-1 do
      if pAnimations.Items[i] is TTimeBasedAnimationData then
        WriteTimeBasedAnimToJSon(TTimeBasedAnimationData(pAnimations.Items[i]),pTarget,i=pAnimations.Count-1)
      else if pAnimations.Items[i] is TFrameBasedAnimationData then
        WriteFrameBasedAnimToJSon(TFrameBasedAnimationData(pAnimations.Items[i]),pTarget,i=pAnimations.Count-1)
      else raise Exception.Create('Unknown animation data type!');
    WriteLine(pTarget,#2']');
    WriteLine(pTarget,'}');
  end else
  if Assigned(pFontData) then begin
    WriteLine(pTarget,'{ "CharBoxes" :');
    WriteLine(pTarget,#2'[');
    w:=false;
    for i:=0 to 255 do
      if pFontData.CharBoxes[i].Width<>0 then begin
        if w then begin
          WriteLine(pTarget,#4'},');
          w:=false;
        end;
        WriteLine(pTarget,#4'{');
        WriteLine(pTarget,Format(#6'"AsciiCode" : %d,',[i]));
        WriteLine(pTarget,Format(#6'"Left" : %d,',[pFontData.CharBoxes[i].Left]));
        WriteLine(pTarget,Format(#6'"Top" : %d,',[pFontData.CharBoxes[i].Top]));
        WriteLine(pTarget,Format(#6'"Width" : %d,',[pFontData.CharBoxes[i].Width]));
        WriteLine(pTarget,Format(#6'"Height" : %d',[pFontData.CharBoxes[i].Height]));
        w:=true;
      end;
    if w then WriteLine(pTarget,#4'}');
    WriteLine(pTarget,#2']');
    WriteLine(pTarget,'}');
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageWriter('JSON',@WriteJSON);

end.


// MKSZTSZ JSON reader for TARGBImage
//
// Reads animation or font data from JSON file.
//
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 3.
//
// Written by Szabó "Gilby" Zsolt/MKSZTSZ               Hungary, 2025
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2025.08.07
//     * Initial creation

unit ARGBImageJSONReaderUnit;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

implementation

uses
  Classes, SysUtils, ARGBImageUnit, fpjson, jsonparser, Logger,
  AnimationDataUnit, FontDataUnit, MKToolbox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

procedure ProcessAnimation(Animations:TAnimationDatas;JD:TJSONData);
type TFrame=record x,y:integer;end;
var name:String;ty,i,wi,he,fdly,ldlyi,hpx,hpy,stfr:integer;fps,ldlyt,pps:double;
  looped,randomstart,paused,pingpong,reversed:boolean;
  frames:array of TFrame=nil;
  atmF:TFrameBasedAnimationData;
  atmT:TTimeBasedAnimationData;
  JA:TJSONArray;
begin
  ty:=strtoint(decode(uppercase(JD.FindPath('Type').AsString),
    'FRAMEBASED,1,TIMEBASED,2,0'));
  if ty=0 then
    raise Exception.Create(Format('Unknown Animation type attribute! (%s)',[string(JD.FindPath('Type').AsString)]));
  wi:=0;he:=0;
  name:='';
  hpx:=0;
  hpy:=0;
  stfr:=0;
  fdly:=0;
  ldlyi:=0;
  fps:=1;
  ldlyt:=0;
  pps:=1;
  looped:=false;
  randomstart:=false;
  paused:=false;
  pingpong:=false;
  reversed:=false;
  SetLength(frames,0);
  if Assigned(JD.FindPath('Name')) then name:=JD.FindPath('Name').AsString;
  if Assigned(JD.FindPath('Width')) then wi:=JD.FindPath('Width').AsInteger;
  if Assigned(JD.FindPath('Height')) then he:=JD.FindPath('Height').AsInteger;
  if Assigned(JD.FindPath('StartFrame')) then stfr:=JD.FindPath('StartFrame').AsInteger;
  if ty=1 then begin
    if Assigned(JD.FindPath('FrameDelay')) then fdly:=JD.FindPath('FrameDelay').AsInteger;
    if Assigned(JD.FindPath('LoopDelay')) then ldlyi:=JD.FindPath('LoopDelay').AsInteger;
  end else
  if ty=2 then begin
    if Assigned(JD.FindPath('FPS')) then fps:=JD.FindPath('FPS').AsFloat;
    if Assigned(JD.FindPath('LoopDelay')) then ldlyt:=JD.FindPath('LoopDelay').AsFloat;
    if Assigned(JD.FindPath('PPS')) then pps:=JD.FindPath('PPS').AsFloat;
  end;
  if Assigned(JD.FindPath('HotPoint.X')) then hpx:=JD.FindPath('HotPoint.X').AsInteger;
  if Assigned(JD.FindPath('HotPoint.x')) then hpx:=JD.FindPath('HotPoint.x').AsInteger;
  if Assigned(JD.FindPath('HotPoint.Y')) then hpy:=JD.FindPath('HotPoint.Y').AsInteger;
  if Assigned(JD.FindPath('HotPoint.y')) then hpy:=JD.FindPath('HotPoint.y').AsInteger;
  if Assigned(jd.FindPath('Looped')) then looped:=jd.FindPath('Looped').AsBoolean;
  if Assigned(jd.FindPath('RandomStart')) then randomstart:=jd.FindPath('RandomStart').AsBoolean;
  if Assigned(jd.FindPath('Paused')) then paused:=jd.FindPath('Paused').AsBoolean;
  if Assigned(jd.FindPath('Pingpong')) then pingpong:=jd.FindPath('Pingpong').AsBoolean;
  if Assigned(jd.FindPath('Reversed')) then reversed:=jd.FindPath('Reversed').AsBoolean;
  if Assigned(JD.FindPath('Frames')) then begin
    JA:=TJSONArray(JD.FindPath('Frames'));
    SetLength(frames,JA.Count);
    for i:=0 to JA.Count-1 do begin
      if Assigned(JA[i].FindPath('X')) then
        frames[i].x:=JA[i].FindPath('X').AsInteger
      else
        if Assigned(JA[i].FindPath('x')) then
          frames[i].x:=JA[i].FindPath('x').AsInteger
        else
          frames[i].x:=0;
      if Assigned(JA[i].FindPath('Y')) then
        frames[i].y:=JA[i].FindPath('Y').AsInteger
      else
        if Assigned(JA[i].FindPath('y')) then
          frames[i].y:=JA[i].FindPath('y').AsInteger
        else
          frames[i].y:=0;
    end;
  end;

  if (wi=0) then raise Exception.Create('Width is not specified!');
  if (he=0) then raise Exception.Create('Height is not specified!');
  if length(frames)=0 then raise Exception.Create('No frames are specified!');
  if ty=1 then begin
    atmf:=TFrameBasedAnimationData.Create(wi,he);
    atmf.Name:=name;
    atmf.StartFrame:=stfr;
    atmf.FrameDelay:=fdly;
    atmf.LoopDelay:=ldlyi;
    atmf.HotPointX:=hpx;
    atmf.HotPointY:=hpy;
    atmf.Looped:=looped;
    atmf.RandomStart:=randomstart;
    atmf.Paused:=paused;
    atmf.PingPong:=pingpong;
    atmf.ReverseAnim:=reversed;
    for i:=0 to length(frames)-1 do atmf.AddFrame(frames[i].x,frames[i].y);
    Animations.AddObject(atmf.Name,atmf);
  end else
  if ty=2 then begin
    atmt:=TTimeBasedAnimationData.Create(wi,he);
    atmt.Name:=name;
    atmt.StartFrame:=stfr;
    atmt.FPS:=fps;
    atmt.PPS:=pps;
    atmt.LoopDelay:=ldlyt;
    atmt.HotPointX:=hpx;
    atmt.HotPointY:=hpy;
    atmt.Looped:=looped;
    atmt.RandomStart:=randomstart;
    atmt.Paused:=paused;
    atmt.PingPong:=pingpong;
    atmt.ReverseAnim:=reversed;
    for i:=0 to length(frames)-1 do atmt.AddFrame(frames[i].x,frames[i].y);
    Animations.AddObject(atmt.Name,atmt);
  end;
end;

procedure ProcessCharBox(FontData:TFontData;JD:TJSONData);
var c,x,y,w,h:integer;
begin
  c:=0;
  x:=0;
  y:=0;
  w:=0;
  h:=0;
  c:=JD.FindPath('AsciiCode').AsInteger;
  x:=JD.FindPath('Left').AsInteger;
  y:=JD.FindPath('Top').AsInteger;
  w:=JD.FindPath('Width').AsInteger;
  h:=JD.FindPath('Height').AsInteger;
  if (w<>0) and (h<>0) then
    FontData.SetCharBox(c,x,y,w,h);
end;

procedure ReadJSON(pSource:TStream;var Width,Height:integer;var RawData:pointer;Animations:TAnimationDatas;var FontData:TFontData);
var
  J:TJSONData;
  JA:TJSONArray;
  i:integer;
begin
  J:=GetJSON(pSource);
  try
    if Assigned(J.FindPath('Animations')) then begin
      JA:=TJSONArray(J.FindPath('Animations'));
      for i:=0 to JA.Count-1 do
        ProcessAnimation(Animations,JA.Items[i]);
    end else
    if Assigned(J.FindPath('CharBoxes')) then begin
      if Assigned(FontData) then FontData.Free;
      FontData:=TFontData.Create;
      JA:=TJSONArray(J.FindPath('CharBoxes'));
      for i:=0 to JA.Count-1 do
        ProcessCharBox(FontData,JA.Items[i]);
    end;
  finally
    J.Free;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('JSON',@ReadJSON,false);

end.


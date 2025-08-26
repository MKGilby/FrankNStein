{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSVMU;

{$mode Delphi}{$H+}

interface

uses Classes, SysUtils, BASS;

const
  VMUFILENAME:string='FrankNStein.vmu';
  LEVELPACKNAME:string='<internal>';
  MAXSLOTS=3;

type

  { TConfiguration }

  TConfiguration=class
    constructor Create;
    constructor CreateFromStream(pSource:TStream);
    procedure SaveToStream(pTarget:TStream);
  private
    fSoundVolume,
    fMusicVolume:float;
    fScalingQuality:integer;
    fLastUsedSlot:integer;
    procedure LoadFromStream_V1(pSource:TStream);
    procedure fSetSoundVolume(value:float);
    procedure fSetMusicVolume(value:float);
    procedure fSetScalingQuality(value:integer);
    procedure fSetLastUsedSlot(value:integer);
  public
    FullScreen:boolean;
    property SoundVolume:float read fSoundVolume write fSetSoundVolume;
    property MusicVolume:float read fMusicVolume write fSetMusicVolume;
    property ScalingQuality:integer read fScalingQuality write fSetScalingQuality;
    property LastUsedSlot:integer read fLastUsedSlot write fSetLastUsedSlot;
  end;

  { TSlot }

  TSlot=class
    constructor Create;
    constructor CreateFromStream(pSource:TStream);
    procedure SaveToStream(pTarget:TStream);
    procedure UpdateLastUsed;
  private
    fLastUsed:TDateTime;
    fMapCompleted:array of boolean;
    procedure LoadFromStream_V1(pSource:TStream);
    function fGetMapCompleted(index:integer):boolean;
    function fGetMapCount:integer;
    procedure fSetMapCount(value:integer);
    function fGetCompletedMapCount:integer;
  public
    IsUsed:boolean;
    property LastUsed:TDateTime read fLastUsed;
    property MapCompleted[index:integer]:boolean read fGetMapCompleted;
    property MapCount:integer read fGetMapCount write fSetMapCount;
    property CompletedMapCount:integer read fGetCompletedMapCount;
  end;

  { TVMU }

  TVMU=class
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(pFilename:string);
    procedure LoadFromStream(pSource:TStream);
    procedure SaveToFile(pFilename:string);
    procedure SaveToStream(pTarget:TStream);

{    procedure SetSlotUsed(pSlot:integer);
    function IsSlotUsed(pSlot:integer):boolean;}
{    function IsMapCompleted(pSlot,pMapNo:integer):boolean;
    procedure SetMapState(pSlot,pMapNo:integer;pCompleted:boolean);
    procedure ClearSlot(pSlot:integer);
    function GetCompletedMapCount(pSlot:integer):integer;
    procedure CompleteAllMaps(pSlot:integer);
    procedure UpdateSlotTimestamp(pSlot:integer);
    function GetSlotLastUseDate(pSlot:integer):string;
    function GetSlotLastUseTime(pSlot:integer):string;}
  private
    fConfig:TConfiguration;
    fSlots:array[0..MAXSLOTS-1] of TSlot;
//    fMapCount:integer;
//    fLevelPackID:integer;
    procedure fSetMapCount(value:integer);

    procedure LoadFromStream_V1(pSource:TStream);
    function fGetSlot(index:integer):TSlot;
  public
    property Config:TConfiguration read fConfig;
    property Slots[index:integer]:TSlot read fGetSlot;
    property MapCount:integer write fSetMapCount;
  end;

implementation

uses
  Logger;

const
  VMUMARKER='VMU';
  FULLSCREENMASK=$01;
  FULLSCREENSHIFT=0;
  SCALINGQUALITYMASK=$06;
  SCALINGQUALITYSHIFT=1;
  LASTUSEDSLOTMASK=$18;
  LASTUSEDSLOTSHIFT=3;
  VMUSTREAMCURRENTVERSION=1;
  CONFIGSTREAMCURRENTVERSION=1;
  SLOTSTREAMCURRENTVERSION=1;
  MAPCOMPLETEDMASK=$45;
  MAPCOMPLETEDINVERSEMASK=$FF xor MAPCOMPLETEDMASK;

{ TConfiguration }
{$region /fold}

constructor TConfiguration.Create;
begin
  SoundVolume:=0.5;
  MusicVolume:=0.5;
  FullScreen:=false;
  ScalingQuality:=0;
  LastUsedSlot:=0;
end;

constructor TConfiguration.CreateFromStream(pSource:TStream);
var b:byte=0;
begin
  pSource.Read(b,1);
  case b of
    1:LoadFromStream_V1(pSource);
    else raise Exception.Create(Format('Invalid configuration block version! (%d)',[b]));
  end;
end;

procedure TConfiguration.SaveToStream(pTarget:TStream);
var b:byte=0;
begin
  b:=CONFIGSTREAMCURRENTVERSION;
  pTarget.Write(b,1);
  pTarget.Write(fSoundVolume,SizeOf(float));
  pTarget.Write(fMusicVolume,SizeOf(float));
  if FullScreen then b:=1 else b:=0;
  b:=b shl FULLSCREENSHIFT or
     (fScalingQuality shl SCALINGQUALITYSHIFT) or
     (fLastUsedSlot shl LASTUSEDSLOTSHIFT);
  pTarget.Write(b,1);
  b:=0;
  pTarget.Write(b,1);
  pTarget.Write(b,1);
end;

procedure TConfiguration.LoadFromStream_V1(pSource:TStream);
var b:byte=0;
// <sound_volume(f)><music_volume(f)><flags(b)><unused(w)>
begin
  pSource.Read(fSoundVolume,sizeof(float));
  pSource.Read(fMusicVolume,sizeof(float));
  pSource.Read(b,1);
  FullScreen:=((b and FULLSCREENMASK) shr FULLSCREENSHIFT)<>0;
  ScalingQuality:=(b and SCALINGQUALITYMASK) shr SCALINGQUALITYSHIFT;
  LastUsedSlot:=(b and LASTUSEDSLOTMASK) shr LASTUSEDSLOTSHIFT;
  pSource.Seek(2,soFromCurrent);  // Skip unused word
end;

procedure TConfiguration.fSetSoundVolume(value:float);
begin
  if (value<>fSoundVolume) and (value>=0) and (value<=1) then
    fSoundVolume:=value;
end;

procedure TConfiguration.fSetMusicVolume(value:float);
begin
  if (value<>fMusicVolume) and (value>=0) and (value<=1) then
    fMusicVolume:=value;
end;

procedure TConfiguration.fSetScalingQuality(value:integer);
begin
  if (value<>fScalingQuality) and (fScalingQuality>=0) and (fScalingQuality<4) then
    fScalingQuality:=value
  else
    fScalingQuality:=0;
end;

procedure TConfiguration.fSetLastUsedSlot(value:integer);
begin
  if (value<>fLastUsedSlot) and (fLastUsedSlot>=0) and (fScalingQuality<MAXSLOTS) then
    fLastUsedSlot:=value
  else
    fLastUsedSlot:=0;
end;

{$endregion}

{ TSlot }
{$region /fold}

constructor TSlot.Create;
begin
  IsUsed:=false;
  fLastUsed:=now;
  SetLength(fMapCompleted,0);
end;

constructor TSlot.CreateFromStream(pSource:TStream);
var b:byte=0;
begin
  pSource.Read(b,1);
  case b of
    1:LoadFromStream_V1(pSource);
    else raise Exception.Create(Format('Invalid slot block version! (%d)',[b]));
  end;
end;

procedure TSlot.SaveToStream(pTarget:TStream);
var b,b2:byte;i:integer;
begin
  b:=SLOTSTREAMCURRENTVERSION;
  pTarget.Write(b,1);
  if IsUsed then begin
    b:=1;
    pTarget.Write(b,1);
    pTarget.Write(fLastUsed,SizeOf(TDateTime));
    b:=length(fMapCompleted);
    pTarget.Write(b,1);
    if b>0 then begin
      for i:=0 to length(fMapCompleted)-1 do begin
        if fMapCompleted[i] then
          b2:=random(256) or MAPCOMPLETEDMASK
        else
          b2:=random(256) and MAPCOMPLETEDINVERSEMASK;
        pTarget.Write(b2,1);
      end;
    end;
  end else begin
    b:=0;
    pTarget.Write(b,1);
  end;
end;

procedure TSlot.UpdateLastUsed;
begin
  fLastUsed:=now;
end;

procedure TSlot.LoadFromStream_V1(pSource:TStream);
//<state(b)><last_save_time(dt)><map_count><map_completion_data(r)>
var
  b:byte=0;
  b2:byte=0;
  i:integer;
begin
  pSource.Read(b,1);
  IsUsed:=b<>0;
  if IsUsed then begin
    pSource.Read(fLastUsed,sizeof(TDateTime));
    pSource.Read(b,1);  // MapCount
    if b>0 then begin
      SetLength(fMapCompleted,b);
      for i:=0 to b-1 do begin
        pSource.Read(b2,1);
        fMapCompleted[i]:=(b2 and MAPCOMPLETEDMASK=MAPCOMPLETEDMASK);
      end;
    end;
  end;
end;

function TSlot.fGetMapCompleted(index:integer):boolean;
begin
  if (index>=0) and (index<length(fMapCompleted)) then
    Result:=fMapCompleted[index]
  else
    Result:=false;
end;

function TSlot.fGetMapCount:integer;
begin
  Result:=length(fMapCompleted);
end;

procedure TSlot.fSetMapCount(value:integer);
var i,j:integer;
begin
  j:=length(fMapCompleted);
  SetLength(fMapCompleted,value);
  if value>j then
    for i:=j to value-1 do
      fMapCompleted[i]:=false;
end;

function TSlot.fGetCompletedMapCount:integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to length(fMapCompleted)-1 do
    if fMapCompleted[i] then inc(Result);
end;

{$endregion}

{ TVMU }
{$region /fold}

constructor TVMU.Create;
begin
  inherited Create;
  LoadFromFile(VMUFILENAME);
end;

destructor TVMU.Destroy;
var i:integer;
begin
  SaveToFile(VMUFILENAME);
  for i:=0 to MAXSLOTS-1 do
    fSlots[i].Free;
  fConfig.Free;
  inherited Destroy;
end;

procedure TVMU.LoadFromFile(pFilename:string);
var S:TStream;i:integer;
begin
  if FileExists(pFilename) then begin
    S:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream(S);
    finally
      S.Free;
    end;
  end else begin
    fConfig:=TConfiguration.Create;
    for i:=0 to MAXSLOTS-1 do
      fSlots[i]:=TSlot.Create;
  end;
end;

procedure TVMU.LoadFromStream(pSource:TStream);
var b:byte=0;s:string=#0#0#0;
begin
  pSource.Read(s[1],3);
  if s<>'VMU' then
    raise Exception.Create(Format('Invalid VMU file marker! (not "%s")',[VMUMARKER]));
  pSource.Read(b,1);
  case b of
    1:LoadFromStream_V1(pSource);
    else
      raise Exception.Create(Format('Invalid VMU file version! %d',[b]));
  end;
end;

procedure TVMU.SaveToFile(pFilename:string);
var S:TStream;
begin
  S:=TFileStream.Create(pFilename,fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TVMU.SaveToStream(pTarget:TStream);
var s:string;i:integer;
begin
  s:=VMUMARKER;
  pTarget.Write(s[1],length(s));
  i:=VMUSTREAMCURRENTVERSION;
  pTarget.Write(i,1);
  Config.SaveToStream(pTarget);
  for i:=0 to MAXSLOTS-1 do
    fSlots[i].SaveToStream(pTarget);
end;

{procedure TVMU.SetSlotUsed(pSlot: integer);
var i:integer;
begin
  if pSlot in [0..MAXSLOTS-1] then begin
    i:=random(255)+1;  // 1..255
    WriteData(pSlot,fLevelPackID,SLOTSTATEOFFSET,1,i);
    UpdateSlotTimestamp(pSlot);
  end else raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
end;

function TVMU.IsSlotUsed(pSlot: integer): boolean;
var i:integer;
begin
  if pSlot in [0..MAXSLOTS-1] then begin
    i:=0;
    if ReadData(pSlot,fLevelPackID,SLOTSTATEOFFSET,1,i) then begin
      Result:=(i<>0);
    end else
      Result:=false;
  end else raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
end;}

{procedure TVMU.SelectSlot(pSlot: integer);
begin
  // -1 means unselect slot
  if (pSlot>=-1) and (pSlot<MAXSLOTS) then begin
    fSlot:=pSlot;
  end else Log.LogWarning(Format('Invalid slot number! (Got: %d, should be: -1..%d)',[pSlot,MAXSLOTS-1]));
end;}

{function TVMU.IsMapCompleted(pSlot,pMapNo:integer):boolean;
var i:integer;
begin
  if fMapCount>-1 then begin
    if pSlot in [0..MAXSLOTS-1] then begin
      if (pMapNo>=0) and (pMapNo<fMapCount) then begin
        i:=0;
        if ReadData(pSlot,fLevelPackID,pMapNo+SLOTMAPSTATESOFFSET,1,i) then
          Result:=i and MAPCOMPLETEDMASK<>0
        else
          Result:=false;
      end else raise Exception.Create(Format('Invalid map number! (Got: %d, should be: 0..%d)',[pMapNo,fMapCount]))
    end else raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
  end else raise Exception.Create('Set VMU.MapCount before calling VMU.GetMapState!');
end;

procedure TVMU.SetMapState(pSlot,pMapNo:integer; pCompleted:boolean);
const CompleteMasks:array[0..6] of byte=(1,4,5,64,65,68,69);
var i:integer;
begin
  if fMapCount>-1 then begin
    if pSlot in [0..MAXSLOTS-1] then begin
      if (pMapNo>=0) and (pMapNo<fMapCount) then begin
        SetSlotUsed(pSlot);
        if pCompleted then
          i:=random(256) and MAPCOMPLETEDINVERSEMASK or CompleteMasks[random(7)]
        else
          i:=random(256) and MAPCOMPLETEDINVERSEMASK;
        if not WriteData(pSlot,fLevelPackID,pMapNo+SLOTMAPSTATESOFFSET,1,i) then
          raise Exception.Create(Format('WriteData failed! (Slot: %d, LPID: %d, MapNo: %d, Data: %d)',[pSlot,fLevelPackID,pMapNo,i]))
      end else raise Exception.Create(Format('Invalid map number! (Got: %d, should be: 0..%d)',[pMapNo,fMapCount]))
    end else raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
  end else raise Exception.Create('Set VMU.MapCount before calling VMU.SetMapState!');
end;

procedure TVMU.ClearSlot(pSlot:integer);
begin
  if (pSlot>0) and (pSlot<MAXSLOTS) then
    ClearData(pSlot,fLevelPackID)
  else
    raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
end;

function TVMU.GetCompletedMapCount(pSlot:integer):integer;
var i:integer;
begin
  if fMapCount>-1 then begin
    if pSlot in [0..MAXSLOTS-1] then begin
      Result:=0;
      for i:=0 to fMapCount-1 do
        if IsMapCompleted(pSlot,i) then inc(Result);
    end;
  end else
    raise Exception.Create('Set VMU.MapCount before calling VMU.GetCompletedMapCount!');
end;

procedure TVMU.CompleteAllMaps(pSlot:integer);
var i:integer;
begin
  if fMapCount>-1 then begin
    if pSlot in [0..MAXSLOTS-1] then begin
      for i:=0 to fMapCount-1 do
        SetMapState(pSlot,i,true);
    end else
      raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
  end else
    raise Exception.Create('Set VMU.MapCount before calling VMU.CompleteAllMaps!');
end;

procedure TVMU.UpdateSlotTimestamp(pSlot: integer);
var i,j,k,l:word;
begin
  if pSlot in [0..MAXSLOTS-1] then begin
    DecodeDate(Now,i,j,k);
    if not (
        WriteData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET,2,i) and
        WriteData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+2,1,j) and
        WriteData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+3,1,k)) then
      raise Exception.Create('Cannot update slot last use date!');
    DecodeTime(Now,i,j,k,l);
    if not (
        WriteData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+4,1,i) and
        WriteData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+5,1,j)) then
      raise Exception.Create('Cannot update slot last use time!');
  end else
    raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
end;

function TVMU.GetSlotLastUseDate(pSlot: integer): string;
var i,j,k:word;
begin
  if pSlot in [0..MAXSLOTS-1] then begin
    i:=0;j:=0;k:=0;
    if not(
        ReadData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET,2,i) and
        ReadData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+2,1,j) and
        ReadData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+3,1,k)) then
      raise Exception.Create('Cannot read slot last use date!');
    Result:=Format('%d.%.2d.%d',[i,j,k]);
  end else
    raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
end;

function TVMU.GetSlotLastUseTime(pSlot: integer): string;
var i,j:word;
begin
  if pSlot in [0..MAXSLOTS-1] then begin
    i:=0;j:=0;
    if not(
        ReadData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+4,1,i) and
        ReadData(pSlot,fLevelPackID,SLOTLASTUSEOFFSET+5,1,j)) then
      raise Exception.Create('Cannot read slot last use time!');
    Result:=Format('%d.%.2d',[i,j]);
  end else
    raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: 0..%d)',[pSlot,MAXSLOTS-1]));
end;

procedure TVMU.fSetSoundVolume(value: float);
begin
  if (value<>fSoundVolume) and (value>=0) and (value<=1) then fSoundVolume:=value;
end;

procedure TVMU.fSetMusicVolume(value: float);
begin
  if (value<>fMusicVolume) and (value>=0) and (value<=1) then fMusicVolume:=value;
end;}

procedure TVMU.fSetMapCount(value: integer);
var i:integer;
begin
  if (value>0) and (value<256) then begin
    for i:=0 to MAXSLOTS-1 do
      fSlots[i].MapCount:=value;
  end else
    raise Exception.Create(Format('Invalid MapCount value! (%d)',[value]));
end;

{procedure TVMU.fSetLastUsedSlot(value: integer);
begin
  if (value<>fLastUsedSlot) then fLastUsedSlot:=value;
end;}

procedure TVMU.LoadFromStream_V1(pSource:TStream);
//<config(r)><slot0data(r)><slot1data(r)><slot2data(r)>
var i:integer;
begin
  fConfig:=TConfiguration.CreateFromStream(pSource);
  for i:=0 to MAXSLOTS-1 do
    fSlots[i]:=TSlot.CreateFromStream(pSource);
end;

function TVMU.fGetSlot(index:integer):TSlot;
begin
  if (index>=0) and (index<MAXSLOTS) then
    Result:=fSlots[index]
  else
    raise Exception.Create(Format('Invalid slot number! (%d)',[index]));
end;

{$endregion}

end.

{

  Used types:
    (b) - byte (UInt8), 1 byte
    (w) - word (UInt16), 2 bytes
    (d) - dword (UInt32), 4 bytes
    (f) - float (Double), 4 bytes
    (dt) - TDateTime, 4 bytes
    (r) - record, contains one or more fields, varying in size

  File map (version 1):
    0    3           4          16            .             .
    "VMU"<version(b)><config(r)><slot0data(r)><slot1data(r)><slot2data(r)>

  Version (now and later):
    File or block version.

  Config block format (version 1):
    <version(b)><sound_volume(f)><music_volume(f)><flags(b)><unused(w)>

  Volumes:
     Float values beetween 0 and 1 inclusive.

  Flag bits:
     0   - FullScreen (1-True, 0-False)
     1-2 - Scaling quality (when FullScreen is true) (0..3)
     3-4 - Last used slot (0..2, 3 is invalid will be replaced with 0 at loading)
     5-7 - Not used, can be anything

  One save slot data format (version 1):
    0           1         2                   6          7
    <version(b)><state(b)><last_save_time(dt)><map_count><map_completion_data(r)>

  State:
    0     - unused - the remaining data does not present!
    not 0 - used

  Map_completion_data:
    0                         n-1
    <is_map_1_completed(b)>...<is_map_n_completed(b)>

  Is_map_n_completed:
    Seemingly random number, when (value and 69)<>0 -> completed, otherwise not.

}

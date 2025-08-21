{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSVMU;

{$mode Delphi}{$H+}

interface

uses SysUtils, PlayerRegistryUnit, BASS;

const
  VMUFILENAME:string='FrankNStein.vmu';
  LEVELPACKNAME:string='<internal>';

type

  { TVMU }

  TVMU=class(TPlayerRegistry)
    constructor Create;
    destructor Destroy; override;
    procedure SetSlotUsed(pSlot:integer);
    function IsSlotUsed(pSlot:integer):boolean;
//    procedure SelectSlot(pSlot:integer);
    function IsMapCompleted(pSlot,pMapNo:integer):boolean;
    procedure SetMapState(pSlot,pMapNo:integer;pCompleted:boolean);
    procedure ClearSlot(pSlot:integer);
    function GetCompletedMapCount(pSlot:integer):integer;
    procedure CompleteAllMaps(pSlot:integer);
    procedure UpdateSlotTimestamp(pSlot:integer);
    function GetSlotLastUseDate(pSlot:integer):string;
    function GetSlotLastUseTime(pSlot:integer):string;
  private
    fSoundVolume, fMusicVolume: float;
//    fSpeed:integer;
    fScalingQuality:integer;
    fMapCount:integer;
    fLevelPackID:integer;
    fLastUsedSlot:integer;
    procedure fSetSoundVolume(value:float);
    procedure fSetMusicVolume(value:float);
    procedure fSetMapCount(value:integer);
    procedure fSetLastUsedSlot(value:integer);
  public
    FullScreen:boolean;
    property ScalingQuality:integer read fScalingQuality write fScalingQuality;
    property SoundVolume:float read fSoundVolume write fSetSoundVolume;
    property MusicVolume:float read fMusicVolume write fSetMusicVolume;
    property MapCount:integer read fMapCount write fSetMapCount;
    property LastUsedSlot:integer read fLastUsedSlot write fSetLastUsedSlot;
  end;

implementation

uses
  Logger, FNSShared;

const
  CFG=#0'*CFG';
  COMPLETEDMASK=$45;
  COMPLETEDINVERSEMASK=$FF-COMPLETEDMASK;
  SLOTSTATEOFFSET=0;
  SLOTLASTUSEOFFSET=1;
  SLOTMAPSTATESOFFSET=7;

{ TVMU }

constructor TVMU.Create;
var i:integer;
begin
  inherited Create;
  Verbose:=false;
  Load(VMUFILENAME);
  AddLevelPack(LEVELPACKNAME);
  for i:=fPlayers.Count to 4 do AddPlayer(chr(i));
  AddLevelPack(CFG);
  AddPlayer(CFG);
  i:=0;
//  fSlot:=-1;
//  fSpeed:=-1;
  if not ReadData(CFG,CFG,0,sizeof(Float),fSoundVolume) then fSoundVolume:=1;
  if not ReadData(CFG,CFG,sizeof(Float),sizeof(Float),fMusicVolume) then fMusicVolume:=1;
  if not ReadData(CFG,CFG,2*sizeof(Float),1,i) then begin
    FullScreen:=false;
    fScalingQuality:=0;
  end else begin
    FullScreen:=(i and 1=1);
    fScalingQuality:=(i and $06)>>1;
  end;
  if not ReadData(CFG,CFG,2*sizeof(Float)+1,1,fLastUsedSlot) then fLastUsedSlot:=0;
  fLevelPackID:=LevelPacks.IndexOf(LEVELPACKNAME);
  fMapCount:=-1;
end;

destructor TVMU.Destroy;
var b:byte;
begin
  WriteData(CFG,CFG,0,sizeof(Float),fSoundVolume);
  WriteData(CFG,CFG,sizeof(Float),sizeof(Float),fMusicVolume);
  if FullScreen then b:=1 else b:=0;
  b+=fScalingQuality*2;
  WriteData(CFG,CFG,2*sizeof(Float),1,b);
  WriteData(CFG,CFG,2*sizeof(Float)+1,1,fLastUsedSlot);
  Save(VMUFILENAME);
  inherited Destroy;
end;

procedure TVMU.SetSlotUsed(pSlot: integer);
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
end;

{procedure TVMU.SelectSlot(pSlot: integer);
begin
  // -1 means unselect slot
  if (pSlot>=-1) and (pSlot<MAXSLOTS) then begin
    fSlot:=pSlot;
  end else Log.LogWarning(Format('Invalid slot number! (Got: %d, should be: -1..%d)',[pSlot,MAXSLOTS-1]));
end;}

function TVMU.IsMapCompleted(pSlot,pMapNo:integer):boolean;
var i:integer;
begin
  if fMapCount>-1 then begin
    if pSlot in [0..MAXSLOTS-1] then begin
      if (pMapNo>=0) and (pMapNo<fMapCount) then begin
        i:=0;
        if ReadData(pSlot,fLevelPackID,pMapNo+SLOTMAPSTATESOFFSET,1,i) then
          Result:=i and COMPLETEDMASK<>0
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
          i:=random(256) and COMPLETEDINVERSEMASK or CompleteMasks[random(7)]
        else
          i:=random(256) and COMPLETEDINVERSEMASK;
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
end;

procedure TVMU.fSetMapCount(value: integer);
begin
  if (value<>fMapCount) and (value>0) then fMapCount:=value;
end;

procedure TVMU.fSetLastUsedSlot(value: integer);
begin
  if (value<>fLastUsedSlot) then fLastUsedSlot:=value;
end;

end.

{

  One save slot data format:
   0      1               7
   <state><last_save_time><map_completion_data>

  State(b):

   0     - unused
   not 0 - used

  Last_save_time:
   1        3         4       5          6
   <year(w)><month(b)><day(b)><hour24(b)><minutes(b)>

  Map_completion_data:
   7                      7+n-1
   <is_map_1_completed>...<is_map_n_completed>

  Is_map_n_completed(b):

   Seemingly random number

   if (value and 69)<>0 -> completed, otherwise not.

}

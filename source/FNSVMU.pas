{
  Frank N Stein Resurrected - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of Frank N Stein Resurrected.

  Frank N Stein Resurrected is free software: you can redistribute it
  and/or modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  Frank N Stein Resurrected is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  Frank N Stein Resurrected. If not, see <https://www.gnu.org/licenses/>.
}

unit FNSVMU;

{$mode ObjFPC}{$H+}

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
//    procedure SelectSlot(pSlot:integer);
    function IsMapCompleted(pSlot,pMapNo:integer):boolean;
    procedure SetMapState(pSlot,pMapNo:integer;pCompleted:boolean);
    procedure ClearSlot(pSlot:integer);
    function GetCompletedMapCount(pSlot:integer):integer;
    procedure CompleteAllMaps(pSlot:integer);
  private
    fSoundVolume, fMusicVolume: float;
//    fSpeed:integer;
    fScalingQuality:integer;
    fMapCount:integer;
    fLevelPackID:integer;
    procedure fSetSoundVolume(value:float);
    procedure fSetMusicVolume(value:float);
    procedure fSetMapCount(value:integer);
  public
    FullScreen:boolean;
    property ScalingQuality:integer read fScalingQuality write fScalingQuality;
    property SoundVolume:float read fSoundVolume write fSetSoundVolume;
    property MusicVolume:float read fMusicVolume write fSetMusicVolume;
    property MapCount:integer read fMapCount write fSetMapCount;
  end;

implementation

uses
  Logger, FNSShared;

const
  CFG=#0'*CFG';

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
  Save(VMUFILENAME);
  inherited Destroy;
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
        if ReadData(pSlot,fLevelPackID,pMapNo,1,i) then
          Result:=true
        else
          Result:=false;
      end else raise Exception.Create(Format('Invalid map number! (Got: %d, should be: 0..%d)',[pMapNo,fMapCount]))
    end else raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: -1..%d)',[pSlot,MAXSLOTS-1]));
  end else raise Exception.Create('Set VMU.MapCount before calling VMU.GetMapState!');
end;

procedure TVMU.SetMapState(pSlot,pMapNo:integer; pCompleted:boolean);
var i:integer;
begin
  if fMapCount>-1 then begin
    if pSlot in [0..MAXSLOTS-1] then begin
      if (pMapNo>=0) and (pMapNo<fMapCount) then begin
        if pCompleted then i:=1 else i:=0;
        if not WriteData(pSlot,fLevelPackID,pMapNo,1,i) then
          raise Exception.Create(Format('WriteData failed! (Slot: %d, LPID: %d, MapNo: %d, Data: %d)',[pSlot,fLevelPackID,pMapNo,i]))
      end else raise Exception.Create(Format('Invalid map number! (Got: %d, should be: 0..%d)',[pMapNo,fMapCount]))
    end else raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: -1..%d)',[pSlot,MAXSLOTS-1]));
  end else raise Exception.Create('Set VMU.MapCount before calling VMU.SetMapState!');
end;

procedure TVMU.ClearSlot(pSlot:integer);
begin
  if (pSlot>0) and (pSlot<MAXSLOTS) then
    ClearData(pSlot,fLevelPackID)
  else
    raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: -1..%d)',[pSlot,MAXSLOTS-1]));
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
      raise Exception.Create(Format('Invalid slot number! (Got: %d, should be: -1..%d)',[pSlot,MAXSLOTS-1]));
  end else
    raise Exception.Create('Set VMU.MapCount before calling VMU.CompleteAllMaps!');
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

end.


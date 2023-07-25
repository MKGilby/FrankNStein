{ -[Name]-------------------------------------------

                Player Registry Unit


  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it.

  Written by Gilby/MKSZTSZ
  Hungary, 2005-2020

  --------------------------------------------------

  -[Description]------------------------------------

   Player registry class with save/load functions

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     - Initial creation
//  V1.10: Gilby - 2006.09.28
//     - LevelPack data size now freely configurable
//     - New levelpack saving mode (loading old levelpacks is OK)
//     - ReadData and WriteData methods added
//  V1.11: Gilby - 2006.09.29
//     - ChangePlayerName(index:integer;newname:string); method added.
//  V2.00: Gilby - 2008.04.24
//     - Complete rewrite
//     - It's now a class
//  V2.02: Gilby - 2010.06.29
//     - Uses MAD4_LowLevelUnit
//  V3.00: Gilby - 2013.10.25
//     - File structure changed. No container used.
//       (MAD4 removed from saving, loading is OK.)
//  V3.01: Gilby - 2013.11.14
//     + List method added (To log content)
//  V3.02: Gilby - 2014.09.16
//     * Dec2Hex changed to IntToHex
//  V3.03: Gilby - 2014.09.16
//     * ReadData for unknown player/levelpack combination now returns false,
//       and leaves the target var unchanged
//  V3.03a: Gilby - 2017.02.23
//     * Changes to suppress hints in Lazarus
//  V3.04: Gilby - 2019.05.03
//     - Global PlayerRegistry variable removed
//     * Using TNamedList<T> instead of TList
//     + PlayerExists(pName):boolean added
//     + LevelPackExists(pName):boolean added
//  V3.05: Gilby - 2019.09.19-20
//     + public properties: Players, LevelPacks
//     + ReadData, WriteData, ClearData with Player and LevelPack indices
//     - PlayerCount removed, use Players.Count instead
//     - PlayerExists removed, use Players.IndexOf()>-1 instead
//     - LevelPackExists removed, user LevelPacks.Indexof()>-1 instead
//     * Fix in RemovePlayer and RemoveLevelPack
//      (associated data was not removed)
//  V3.06: Gilby - 2020.02.14
//     * Fix: Result was not set in Load.
//  V3.07: Gilby - 2022.05.01
//     * Following changes in MAD4MidLevel
//  V3.07a: Gilby - 2022.10.05
//     * Changes to remove Hints in Lazarus.

{$ifdef fpc}
  {$smartlink on}
  {$mode delphi}
{$endif}

unit PlayerRegistryUnit;

interface

uses Classes, Lists;

type
  TPlayerData=class
    _ID:integer;
    _Name:string;
    _Password:string;
  end;
  TPlayerList=TNamedList<TPlayerData>;

  TLevelPackData=class
    _ID:integer;
    _Name:string;
  end;
  TLevelPackList=TNamedList<TLevelPackData>;

  TDataData=class
    _PlayerID:integer;
    _LevelPackID:integer;
    _Data:TMemoryStream;
  end;
  TDataList=TNamedList<TDataData>;

  TPlayerRegistry=class
    constructor Create;
    destructor Destroy; override;
       
    function AddPlayer(pName:string):boolean; virtual; overload;
    function AddPlayer(pName,pPass:string):boolean; virtual; overload;
    function RemovePlayer(pName:string):boolean; virtual;
//    function PlayerCount:integer; virtual;
//    function GetPlayer(pIndex:integer):string; virtual;
    function RenamePlayer(pIndex:integer;pNewName:string):boolean; virtual;
//    function PlayerExists(pName:string):boolean;

    function AddLevelPack(pName:string):boolean; virtual;
    function RemoveLevelPack(pName:string):boolean; virtual;
//    function LevelPackExists(pName:string):boolean;

    function WriteData(pPlayerName,pLevelPackName:string;pStart,pLength:integer;var pData):boolean; virtual; overload;
    function ReadData(pPlayerName,pLevelPackName:string;pStart,pLength:integer;var pData):boolean; virtual; overload;
    function ClearData(pPlayerName,pLevelPackName:string):boolean; virtual; overload;
    function WriteData(pPlayerIndex,pLevelPackIndex:integer;pStart,pLength:integer;var pData):boolean; virtual; overload;
    function ReadData(pPlayerIndex,pLevelPackIndex:integer;pStart,pLength:integer;var pData):boolean; virtual; overload;
    function ClearData(pPlayerIndex,pLevelPackIndex:integer):boolean; virtual; overload;

    function Save(pFilename:string):boolean; virtual;
    function Load(pFilename:string):boolean; virtual;

    procedure List;
  protected
    fPlayers:TPlayerList;
    fLevelPacks:TLevelPackList;
  private
    fDatas:TDataList;
    fNextID:integer;
    function LoadMAD4(pFilename:string):boolean;
    function LoadPREG(pFilename:string):boolean;
    function GetNextID:integer;
    function GetPlayerID(pName:string):integer;
    function GetLevelPackID(pName:string):integer;
    function IndexOfData(pPlayerID,pLevelPackID:integer):integer;
    function IndexOfPlayerID(pPlayerID:integer):integer;
    function IndexOfLevelPackID(pLevelPackID:integer):integer;
  public
    Verbose:boolean;
    property Players:TPlayerList read fPlayers;
    property LevelPacks:TLevelPackList read fLevelPacks;
  end;

implementation

uses SysUtils, MAD4MidLevelUnit, MKToolBox, Logger{, crc32};

const
  Fstr={$I %FILE%}+', ';
  Version='3.07a';

constructor TPlayerRegistry.Create;
begin
  fPlayers:=TPlayerList.Create;
  fLevelPacks:=TLevelPackList.Create;
  fDatas:=TDataList.Create;
  fNextID:=0;
  Verbose:=false;
end;

destructor TPlayerRegistry.Destroy;
var i:integer;
begin
  FreeAndNil(fPlayers);
  FreeAndNil(fLevelPacks);
  for i:=0 to fDatas.Count-1 do fDatas.Items[i]._Data.Free;
  FreeAndNil(fDatas);
  inherited ;
end;

function TPlayerRegistry.GetNextID:integer;
begin
  Result:=fNextID;
  inc(fNextID);
end;

{function TPlayerRegistry.PlayerExists(pName:string):boolean;
begin
  Result:=fPlayers.IndexOf(pName)>-1;
end;

function TPlayerRegistry.LevelPackExists(pName:string):boolean;
begin
  Result:=fLevelPacks.IndexOf(pName)>-1;
end;}

function TPlayerRegistry.GetPlayerID(pName:string):integer;
var i:integer;
begin
  i:=fPlayers.IndexOf(pName);
  if i>-1 then
    Result:=fPlayers[i]._ID
  else
    Result:=-1;
end;

function TPlayerRegistry.GetLevelPackID(pName:string):integer;
var i:integer;
begin
  i:=fLevelPacks.IndexOf(pName);
  if i>-1 then begin
    Result:=fLevelPacks[i]._ID;
    exit;
  end else Result:=-1;
end;

function TPlayerRegistry.IndexOfPlayerID(pPlayerID:integer):integer;
var i:integer;
begin
  for i:=0 to fPlayers.Count-1 do
    if fPlayers[i]._ID=pPlayerID then begin
      Result:=i;
      exit;
    end;
end;

function TPlayerRegistry.IndexOfLevelPackID(pLevelPackID:integer):integer;
var i:integer;
begin
  for i:=0 to fLevelPacks.Count-1 do
    if fLevelPacks[i]._ID=pLevelPackID then begin
      Result:=i;
      exit;
    end;
end;

function TPlayerRegistry.IndexOfData(pPlayerID,pLevelPackID:integer):integer;
begin
  Result:=fDatas.IndexOf(inttostr(pPlayerID)+'.'+inttostr(pLevelPackID));
end;

function TPlayerRegistry.AddPlayer(pName:string):boolean;
begin
  Result:=AddPlayer(pName,'');
end;

function TPlayerRegistry.AddPlayer(pName,pPass:string):boolean;
const Istr=Fstr+'TPlayerRegistry.AddPlayer';
var atm:TPlayerData;
begin
  if Verbose then Log.LogStatus('Addplayer('#39+pName+#39','#39+pPass+#39')',Istr);
  if fPlayers.IndexOf(pName)=-1 then begin
    atm:=TPlayerData.Create;
    atm._ID:=GetNextID;
    atm._Name:=pName;
    atm._Password:=pPass;
    fPlayers.AddObject(pName,atm);
    Result:=true;
    if Verbose then Log.LogStatus('Player added. ID='+inttostr(atm._ID),Istr);
  end else begin
    Result:=false;
    if Verbose then Log.LogWarning('AddPlayer failed.',Istr);
  end;
end;

function TPlayerRegistry.RemovePlayer(pName:string):boolean;
var i,j:integer;
begin
  i:=fPlayers.IndexOf(pName);
  if i>-1 then begin
    for j:=fDatas.Count-1 downto 0 do
      if fDatas[j]._PlayerID=fPlayers[i]._ID then begin
        FreeAndNil(fDatas[j]._Data);
        fDatas[j].Free;
        fDatas.Delete(j);
      end;
    fPlayers[i].Free;
    fPlayers.Delete(i);
    Result:=true;
  end else Result:=false;
end;

function TPlayerRegistry.RenamePlayer(pIndex:integer;pNewName:string):boolean;
begin
  if (pIndex>=0) and (pIndex<fPlayers.Count) then begin
    fPlayers[pIndex]._Name:=pNewName;
    Result:=true;
  end else Result:=false;
end;

function TPlayerRegistry.AddLevelPack(pName:string):boolean;
var atm:TLevelPackData;
begin
  if fLevelPacks.IndexOf(pName)=-1 then begin
    atm:=TLevelPackData.Create;
    atm._ID:=GetNextID;
    atm._Name:=pName;
    fLevelPacks.AddObject(pName,atm);
    Result:=true;
  end else Result:=false;
end;

function TPlayerRegistry.RemoveLevelPack(pName:string):boolean;
var i,j:integer;
begin
  i:=fLevelPacks.IndexOf(pName);
  if i>-1 then begin
    for j:=fDatas.Count-1 downto 0 do
      if fDatas[j]._LevelPackID=fLevelPacks[i]._ID then begin
        FreeAndNil(fDatas[j]._Data);
        fDatas[j].Free;
        fDatas.Delete(j);
      end;
    fLevelPacks[i].Free;
    fLevelPacks.Delete(i);
    Result:=true;
  end else Result:=false;
end;

function TPlayerRegistry.WriteData(pPlayerName,pLevelPackName:string;pStart,pLength:integer;var pData):boolean;
const Istr=Fstr+'TPlayerRegistry.WriteData';
var PlayerID,LevelPackID,i:integer;
    atm:TDataData;
begin
  if Verbose then Log.LogStatus('WriteData('#39+pPlayerName+#39','#39+pLevelPackName+#39','+inttostr(pStart)+','+inttostr(pLength)+')',Istr);
  PlayerID:=GetPlayerID(pPlayerName);
//  Log.Trace(PlayerID);
  if PlayerID>-1 then begin
    if Verbose then Log.LogStatus('PlayerID='+inttostr(PlayerID),Istr);
    LevelPackID:=GetLevelPackID(pLevelPackName);
//    Log.Trace(LevelPackID);
    if LevelPackID>-1 then begin
      if Verbose then Log.LogStatus('LevelPackID='+inttostr(LevelPackID),Istr);
      i:=IndexOfData(PlayerID,LevelPackID);
      if i=-1 then begin
        if Verbose then Log.LogStatus('No data record for this combination, creating a new one.',Istr);
        atm:=TDataData.Create;
        atm._PlayerID:=PlayerID;
        atm._LevelPackID:=LevelPackID;
        atm._Data:=TMemoryStream.Create;
        fDatas.AddObject(inttostr(PlayerID)+'.'+inttostr(LevelPackID),atm);
      end else atm:=fDatas[i];
      if Verbose then Log.LogStatus('Data current size: '+inttostr(atm._Data.Size),Istr);
      if atm._Data.Size<pStart then begin
        if Verbose then Log.LogStatus('Padding with '+inttostr(pStart-atm._Data.Size)+' bytes of zeroes.',Istr);
        atm._Data.Seek(0,soFromEnd);
        i:=0;
        while atm._Data.Size<pStart do atm._Data.Write(i,1);
      end;
      if Verbose then Log.LogStatus('Writing data.',Istr);
      atm._Data.Seek(pStart,soFromBeginning);
      atm._Data.Write(pData,pLength);
      if Verbose then Log.LogStatus('Data size after writing: '+inttostr(atm._Data.Size),Istr);
      Result:=true;
    end else
      Result:=false;
  end else
    Result:=false;
end;

function TPlayerRegistry.WriteData(pPlayerIndex,pLevelPackIndex:integer;pStart,pLength:integer;var pData):boolean;
const Istr=Fstr+'TPlayerRegistry.WriteData';
var atm:TDataData;i:integer;
begin
  if (pPlayerIndex>=0) and (pPlayerIndex<fPlayers.Count) and
     (pLevelPackIndex>=0) and (pLevelPackIndex<fLevelPacks.Count) then begin

    i:=IndexOfData(fPlayers[pPlayerIndex]._ID,fLevelPacks[pLevelPackIndex]._ID);
    if i=-1 then begin
      if Verbose then Log.LogStatus('No data record for this combination, creating a new one.',Istr);
      atm:=TDataData.Create;
      atm._PlayerID:=fPlayers[pPlayerIndex]._ID;
      atm._LevelPackID:=fLevelPacks[pLevelPackIndex]._ID;
      atm._Data:=TMemoryStream.Create;
      fDatas.AddObject(inttostr(fPlayers[pPlayerIndex]._ID)+'.'+inttostr(fLevelPacks[pLevelPackIndex]._ID),atm);
    end else atm:=fDatas[i];
    if Verbose then Log.LogStatus('Data current size: '+inttostr(atm._Data.Size),Istr);
    if atm._Data.Size<pStart then begin
      if Verbose then Log.LogStatus('Padding with '+inttostr(pStart-atm._Data.Size)+' bytes of zeroes.',Istr);
      atm._Data.Seek(0,soFromEnd);
      i:=0;
      while atm._Data.Size<pStart do atm._Data.Write(i,1);
    end;
    if Verbose then Log.LogStatus('Writing data.',Istr);
    atm._Data.Seek(pStart,soFromBeginning);
    atm._Data.Write(pData,pLength);
    if Verbose then Log.LogStatus('Data size after writing: '+inttostr(atm._Data.Size),Istr);
    Result:=true;
  end else
    Result:=false;
end;

function TPlayerRegistry.ReadData(pPlayerName,pLevelPackName:string;pStart,pLength:integer;var pData):boolean;
const Istr='PlayerRegistryUnit.pas, TPlayerRegistry.ReadData';
var PlayerID,LevelPackID,i:integer;
    atm:TDataData;
begin
  PlayerID:=GetPlayerID(pPlayerName);
  if PlayerID>-1 then begin
    LevelPackID:=GetLevelPackID(pLevelPackName);
    if LevelPackID>-1 then begin
      i:=IndexOfData(PlayerID,LevelPackID);
      if i>-1 then begin
        atm:=fDatas[i];
        atm._Data.Seek(0,soFromEnd);
        i:=0;
        while atm._Data.Size<Int64(pStart)+Int64(pLength) do atm._Data.Write(i,1);
        atm._Data.Seek(pStart,soFromBeginning);
        atm._Data.Read(pData,pLength);
        Result:=true;
      end else begin
        Log.LogWarning('Data record not found. ('+pPlayerName+', '+pLevelPackName+')',Istr);
        Result:=false;
      end;
    end else begin
      Log.LogWarning('LevelPack not found. ('+pPlayerName+', '+pLevelPackName+')',Istr);
      Result:=false;
    end;
  end else begin
    Log.LogWarning('Player not found. ('+pPlayerName+', '+pLevelPackName+')',Istr);
    Result:=false;
  end;
end;

function TPlayerRegistry.ReadData(pPlayerIndex,pLevelPackIndex:integer;pStart,pLength:integer;var pData):boolean;
const Istr='PlayerRegistryUnit.pas, TPlayerRegistry.ReadData';
var i:integer;atm:TDataData;
begin
  if (pPlayerIndex>=0) and (pPlayerIndex<fPlayers.Count) and
     (pLevelPackIndex>=0) and (pLevelPackIndex<fLevelPacks.Count) then begin

    i:=IndexOfData(fPlayers[pPlayerIndex]._ID,fLevelPacks[pLevelPackIndex]._ID);
    if i>-1 then begin
      atm:=fDatas[i];
      atm._Data.Seek(0,soFromEnd);
      i:=0;
      while atm._Data.Size<Int64(pStart)+Int64(pLength) do atm._Data.Write(i,1);
      atm._Data.Seek(pStart,soFromBeginning);
      atm._Data.Read(pData,pLength);
      Result:=true;
    end else begin
      Log.LogWarning(Format('Data record not found. (%d,%d)',[pPlayerIndex,pLevelPackIndex]),Istr);
      Result:=false;
    end;
  end;
end;

function TPlayerRegistry.ClearData(pPlayerName,pLevelPackName:string):boolean;
var PlayerID,LevelPackID,i:integer;
begin
  PlayerID:=GetPlayerID(pPlayerName);
  if PlayerID>-1 then begin
    LevelPackID:=GetLevelPackID(pLevelPackName);
    if LevelPackID>-1 then begin
      i:=IndexOfData(PlayerID,LevelPackID);
      if i>-1 then begin
        fDatas[i]._Data.SetSize(0);
        Result:=true;
      end else
        Result:=false;
    end else
      Result:=false;
  end else
    Result:=false;
end;

function TPlayerRegistry.ClearData(pPlayerIndex,pLevelPackIndex:integer):boolean;
var i:integer;
begin
  if (pPlayerIndex>=0) and (pPlayerIndex<fPlayers.Count) and
     (pLevelPackIndex>=0) and (pLevelPackIndex<fLevelPacks.Count) then begin

    i:=IndexOfData(fPlayers[pPlayerIndex]._ID,fLevelPacks[pLevelPackIndex]._ID);
    if i>-1 then begin
      fDatas[i]._Data.SetSize(0);
      Result:=true;
    end else
      Result:=false;
  end;
end;

function TPlayerRegistry.Save(pFilename:string):boolean;
//const Istr=Fstr+'TPlayerRegistry.Save';
var Stream:TStream;
    i,k:integer;j:word;b:byte;s:string;
begin
  Stream:=TFileStream.Create(pFilename,fmCreate);
  s:='PREG';
  Stream.Write(s[1],4);
  Stream.Write(fNextID,4);
  j:=fPlayers.Count;
  Stream.Write(j,2);
  for i:=0 to fPlayers.Count-1 do with TPlayerData(fPlayers[i]) do begin
    Stream.Write(_ID,4);
    j:=length(_Name);
    Stream.Write(j,2);
    if j>0 then Stream.Write(_Name[1],j);
    j:=length(_Password);
    Stream.Write(j,2);
    if j>0 then Stream.Write(_Password[1],j);
  end;
  j:=fLevelPacks.Count;
  Stream.Write(j,2);
  for i:=0 to fLevelPacks.Count-1 do with TLevelPackData(fLevelPacks.Items[i]) do begin
    Stream.Write(_ID,4);
    j:=length(_Name);
    if j>0 then Stream.Write(j,2);
    Stream.Write(_Name[1],j);
  end;
  j:=fDatas.Count;
  Stream.Write(j,2);
  b:=0;
  for i:=0 to fDatas.Count-1 do with TDataData(fDatas.Items[i]) do begin
    Stream.Write(_PlayerID,4);
    Stream.Write(_LevelPackID,4);
//    Log.DumpStream(Data,0,Data.Size,'',Istr);
    k:=_Data.Size-1;
    _Data.Seek(k,soFromBeginning);
    _Data.Read(b,1);
    while (k>0) and (b=0) do begin
      dec(k);
      _Data.Seek(k,soFromBeginning);
      _Data.Read(b,1);
    end;
    if b=0 then dec(k);
    inc(k);
    Stream.Write(k,4);
    _Data.Seek(0,soFromBeginning);
    if k>0 then Stream.CopyFrom(_Data,k);
  end;
  FreeAndNil(Stream);
  Result:=true;
end;

function TPlayerRegistry.Load(pFilename:string):boolean;
const Istr=Fstr+'TPlayerRegistry.Load';
var Xs:TStream;s:string;
begin
  Result:=true;
  if FileExists(pFilename) then begin
    Xs:=TFileStream.Create(pFilename,fmOpenRead);
    s:=#0#0#0#0;
    Xs.Read(s[1],4);
    Xs.Free;

    if s='MAD4' then Result:=LoadMAD4(pFilename)
    else if s='PREG' then Result:=LoadPREG(pFilename)
    else begin
      Log.LogError('Unknown file format! (FourCC="'+s+'")',Istr);
      Result:=false;
    end;
  end;
end;

function TPlayerRegistry.LoadPREG(pFilename:string):boolean;
var Xs:TStream;j:shortint;i,k:integer;
    atmP:TPlayerData;
    atmL:TLevelPackData;
    atmD:TDataData;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead);

  Xs.Read(fNextID,4);  // PREG
  Xs.Read(fNextID,4);

  j:=0;
  Xs.Read(j,2);
  for i:=j-1 downto 0 do begin
    atmP:=TPlayerData.Create;
    with atmP do begin
      Xs.Read(_ID,4);
      k:=0;
      Xs.Read(k,2);
      SetLength(_Name,k);
      if k>0 then Xs.Read(_Name[1],k);
      Xs.Read(k,2);
      SetLength(_Password,k);
      if k>0 then Xs.Read(_Password[1],k);
    end;
    fPlayers.AddObject(atmP._name,atmP);
  end;

  Xs.Read(j,2);
  for i:=j-1 downto 0 do begin
    atmL:=TLevelPackData.Create;
    with atmL do begin
      Xs.Read(_ID,4);
      k:=0;
      Xs.Read(k,2);
      SetLength(_Name,k);
      if k>0 then Xs.Read(_Name[1],k);
    end;
    fLevelPacks.AddObject(atmL._Name,atmL);
  end;

  Xs.Read(j,2);
  for i:=j-1 downto 0 do begin
    atmD:=TDataData.Create;
    with atmD do begin
      Xs.Read(_PlayerID,4);
      Xs.Read(_LevelPackID,4);
      Xs.Read(k,4);
      _Data:=TMemoryStream.Create;
      if k>0 then _Data.CopyFrom(Xs,k);
    end;
    fDatas.AddObject(inttostr(atmD._PlayerID)+'.'+inttostr(atmD._LevelPackID),atmD);
  end;

  Xs.Free;
  Result:=true;
end;

function TPlayerRegistry.LoadMAD4(pFilename:string):boolean;
var Stream:TStream;
    i,k:integer;j:word;
    atmP:TPlayerData;
    atmL:TLevelPackData;
    atmD:TDataData;
    MAD4:TMAD4MidLevel;
begin
  MAD4:=TMAD4MidLevel.Create(pFilename);
//  MAD4.Mount(pFilename);
  try
    if MAD4.FileExists('nextid.dat') then begin
      Stream:=MAD4.OpenStream('nextid.dat');
      Stream.Read(fNextID,4);
      Stream.Free;
    end else fNextID:=0;

    if MAD4.FileExists('players.dat') then begin
      Stream:=MAD4.OpenStream('players.dat');
      j:=0;
      Stream.Read(j,2);
      for i:=j-1 downto 0 do begin
        atmP:=TPlayerData.Create;
        with atmP do begin
          Stream.Read(_ID,4);
          k:=0;
          Stream.Read(k,2);
          SetLength(_Name,k);
          Stream.Read(_Name[1],k);
          Stream.Read(k,2);
          SetLength(_Password,k);
          Stream.Read(_Password[1],k);
        end;
        fPlayers.AddObject(atmP._Name,atmP);
      end;
      Stream.Free;
    end;

    if MAD4.FileExists('levelpacks.dat') then begin
      Stream:=MAD4.OpenStream('levelpacks.dat');
      Stream.Read(j,2);
      for i:=j-1 downto 0 do begin
        atmL:=TLevelPackData.Create;
        with atmL do begin
          Stream.Read(_ID,4);
          k:=0;
          Stream.Read(k,2);
          SetLength(_Name,k);
          Stream.Read(_Name[1],k);
        end;
        fLevelPacks.AddObject(atmL._Name,atmL);
      end;
      Stream.Free;
    end;

    if MAD4.FileExists('datas.dat') then begin
      Stream:=MAD4.OpenStream('datas.dat');
      Stream.Read(j,2);
      for i:=j-1 downto 0 do begin
        atmD:=TDataData.Create;
        with atmD do begin
          Stream.Read(_PlayerID,4);
          Stream.Read(_LevelPackID,4);
          Stream.Read(k,4);
          _Data:=TMemoryStream.Create;
          if k>0 then _Data.CopyFrom(Stream,k);
        end;
        fDatas.AddObject(inttostr(atmD._PlayerID)+'.'+inttostr(atmD._LevelPackID),atmD);
      end;
      Stream.Free;
    end;

  finally
    MAD4.Free;
  end;
  Result:=true;
end;

{function TPlayerRegistry.PlayerCount:integer;
begin
  PlayerCount:=fPlayers.Count;
end; }

procedure TPlayerRegistry.List;
var i,j,iP,iL:integer;s1,s2:string;b:Byte;
begin
  Log.WriteLN('----=== PlayerRegistry content listing ===----');
  Log.WriteLN('Players:');
  for i:=0 to fPlayers.Count-1 do with fPlayers[i] do
    Log.Writeln(Format(st(i,2,' ')+'. %s (ID=%d)',[_Name,_ID]));

  Log.WriteLN('LevelPacks:');
  for i:=0 to fLevelPacks.Count-1 do with fLevelPacks[i] do
    Log.Writeln(Format(st(i,2,' ')+'. %s (ID=%d)',[_Name,_ID]));

  Log.WriteLN('Data blocks:');
  b:=0;
  for i:=0 to fDatas.Count-1 do with fDatas[i] do begin
    iP:=IndexOfPlayerID(_PlayerID);
    iL:=IndexOfLevelPackID(_LevelPackID);
    if iP>-1 then s1:=fPlayers[iP]._Name else s1:='NIL';
    s1+=' (ID='+inttostr(_PlayerID)+')';
    if iL>-1 then s2:=fLevelPacks[iL]._Name else s2:='NIL';
    s2+=' (ID='+inttostr(_LevelPackID)+')';
    Log.Writeln(st(i,2,' ')+'. Player='+s1+', LevelPack='+s2+', Data Length='+inttostr(_Data.Size));
    _Data.Seek(0,soFromBeginning);
    Log.Write('Data: ');
    for j:=0 to _Data.Size-1 do begin
      _Data.Read(b,1);
      Log.Write(inttohex(b,2)+' ');
    end;
    Log.WriteLN('');
  end;
  Log.WriteLN('- -- --- PlayerRegistry listing ends. --- -- -');
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
//  PlayerRegistry:=TPlayerRegistry.Create;

{finalization
  PlayerRegistry.Free;}

end.

{
  0    4     'PREG'

  4    4     fNextID
  
  8    2     PlayerCount
  (
    0  4     ID
    4  2     length(Name)
    6  .     Name
    .  2     length(Password)
    .  .     Password
  ) PlayerCount times
  
  .    2     LevelPackCount
  (
    0  4     ID
    4  2     length(Name)
    6  .     Name
  ) LevelPack times
  
  .    2     DataCount
  (
    0  4     PlayerID
    4  4     LevelPackID
    8  2     DataLength
   10  .     Data
  ) DataCount times

}

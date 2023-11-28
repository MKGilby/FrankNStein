{ -[Name]-------------------------------------------

             MKSZTSZ LogicalSprite class
           
  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2017-

  -[Description]------------------------------------

    Logic of a sprite. No screen output, you have to
    create a descendant and override Draw or use a
    TSimpleSprite.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     - Initial creation from SpriteUnit
//  V1.01: Gilby - 2017.10.31
//     - Added .BoundingRect property for collision detection purposes

{$ifdef fpc}
  {$smartlink on}
  {$mode delphi}
{$endif}

unit LogicalSpriteUnit;

interface

uses Classes;

type
  TLogicalSprite=class
    constructor Create(x,y,w,h:integer);
    procedure Move;  virtual;
    procedure Draw;  virtual;
    procedure MoveTo(iX,iY:integer); virtual;
    procedure MoveRel(idX,idY:integer); virtual;
    procedure Kill; virtual;
    procedure UnKill; virtual;
    procedure LogSpriteData;
  protected
    fX, fY, fZOrder : integer;
    fWidth, fHeight : integer;
    fDead, fVisible:boolean;
    fName:string;
    fRect:TRect;
  public
    property Visible:boolean read fVisible write fVisible;
    property Dead:boolean read fDead;
    property ZOrder:integer read fZOrder write fZOrder;
    property Name:string read fName write fName;
    property X:integer read fX write fX;
    property Y:integer read fY write fY;
    property BoundingRect:TRect read fRect;
  end;

implementation

uses Logger, SysUtils;

const Fstr='LogicalSpriteUnit.pas, ';
      Version='1.01';

constructor TLogicalSprite.Create(x,y,w,h:integer);
begin
//  fTargetWindow:=PrimaryWindow;
  fX:=x;
  fY:=y;
  fWidth:=w;
  fHeight:=h;
  fDead:=false;
  fVisible:=true;
  fRect:=Rect(x,y,x+w-1,y+h-1);
end;

procedure TLogicalSprite.Move;
begin
end;

procedure TLogicalSprite.Draw;
begin
end;

procedure TLogicalSprite.MoveTo(iX,iY:integer);
begin
  fX:=iX;
  fY:=iY;
end;

procedure TLogicalSprite.MoveRel(idX,idY:integer);
begin
  fX+=idX;
  fY+=idY;
end;

procedure TLogicalSprite.Kill;
begin
  fDead:=true;
end;

procedure TLogicalSprite.UnKill;
begin
  fDead:=false;
end;

procedure TLogicalSprite.LogSpriteData;
const Istr=Fstr+'TSprite.LogSpriteData';
var s:String;
begin
  Log.LogDebug('------ Sprite data start ------',Istr);
  Log.LogDebug('Name: '+fName,Istr);
  Log.LogDebug('Position: '+inttostr(fX)+', '+inttostr(fY),Istr);
  Log.LogDebug('Dimensions: '+inttostr(fWidth)+'x'+inttostr(fHeight),Istr);
  Log.LogDebug('Z-Order: '+inttostr(fZOrder),Istr);
  s:='[';
  if fVisible then s+='X' else s+=' ';
  s+='] Visible   [';
  if fDead then s+='X' else s+=' ';
  s+='] Dead';
  Log.LogDebug(s,Istr);
  Log.LogDebug('------- Sprite data end -------',Istr);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSProf;

{$mode delphi}{$H+}

interface

uses SysUtils, Animation2Unit, TileMapUnit, FNSDevice, FNSSpring;

type

  { TProf }

  TProf=class
    constructor Create(iMap:TTileMap;iDevice:TDevice);
    destructor Destroy; override;
    procedure Draw;
    procedure Move(pTimeUsed:double);  // in secs.
  private
    fX,fY:double;
    fDirX:integer;
    fRemainingDistance:double;
    fAnimLeft,fAnimRight:TAnimation;
    fMap:TTileMap;
    fState:(psIdle,psFalling,psJumping);
    fDevice:TDevice;
    fTempSpring:TSpring;
    // This one is called in small steps
//    procedure MoveEx(pTimeUsed:double);
  end;

implementation

uses FNSShared, mk_sdl2, sdl2, logger;

const
  SPEEDX=32;  // pixel / sec
  SPEEDY=64;  // pixel / sec
  SPEEDNORMX=SPEEDX/256;  // normalized to screen
  SPEEDNORMY=SPEEDY/192;  // normalized to screen
  JUMPHEIGHT=32/192;

{ TProf }

constructor TProf.Create(iMap:TTileMap; iDevice:TDevice);
begin
  fAnimLeft:=MM.Animations.ItemByName['ProfLeft'].SpawnAnimation;
  fAnimRight:=MM.Animations.ItemByName['ProfRight'].SpawnAnimation;
  fX:=3*8/256;
  fY:=2*8/192;
//  fY:=11*8/192;
  fDirX:=1;
  fMap:=iMap;
  fDevice:=iDevice;
  fTempSpring:=nil;
//  MAXTIMESLICE:=1/max(SPEEDX,SPEEDY);
end;

destructor TProf.Destroy;
begin
  if Assigned(fAnimLeft) then fAnimLeft.Free;
  if Assigned(fAnimRight) then fAnimRight.Free;
  inherited Destroy;
end;

procedure TProf.Draw;
var x,y:integer;
begin
  x:=trunc(fX*256);
  y:=trunc(fy*192);
  if Assigned(fTempSpring) then inc(y);
  if fDirX=1 then
    fAnimRight.PutFrame(x-1,y,(x div 2+2) mod 8)
  else
    fAnimLeft.PutFrame(x-1,y,(x div 2+2) mod 8);
end;

procedure TProf.Move(pTimeUsed:double);
var x,y,px,py:integer;
begin
  x:=trunc(fX*256);
  y:=trunc(fy*192);
  px:=x div 8;
  py:=y div 8;
//  Log.Trace(Format('x (px)=%d (%d), y (py)=%d (%d)',[x,px,y,py]));
  case fState of
    psIdle:begin
      if (keys[SDL_SCANCODE_RIGHT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT])
         and ((x mod 8>0)
         or (((x mod 8)=0) and (fMap.Tiles[px+1,py]=TILE_EMPTY) and (fMap.Tiles[px+1,py+1]=TILE_EMPTY))) then begin
        fX+=SPEEDNORMX*pTimeUsed;
        fDirX:=1;
      end;
      if (keys[SDL_SCANCODE_LEFT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT])
         and ((x mod 8>0)
         or (((x mod 8)=0) and (fMap.Tiles[px-1,py]=TILE_EMPTY) and (fMap.Tiles[px-1,py+1]=TILE_EMPTY))) then begin
        fX-=SPEEDNORMX*pTimeUsed;
        fDirX:=-1;
      end;
      if (x mod 8=0) then begin
        if (fMap.Tiles[px,py+2]=TILE_EMPTY) then fState:=psFalling
        else if fMap.Tiles[px,py+2]=TILE_PIECE+fDevice.NextPiece then begin
          fMap.Tiles[px,py+2]:=TILE_WALL;
          fDevice.PickupPiece;
        end
        else if fMap.Tiles[px,py+2]=TILE_SPRING then begin
          fTempSpring:=Springs.SpringAt(pX,pY+2);
          fTempSpring.PushedDown:=true;
          if (keys[SDL_SCANCODE_SPACE] or controllerbuttons[SDL_CONTROLLER_BUTTON_A]) then begin
            fState:=psJumping;
            fRemainingDistance:=JUMPHEIGHT;
            fTempSpring.Kick;
            fTempSpring:=nil;
          end;
        end
      end else begin
        if Assigned(fTempSpring) then begin
          fTempSpring.PushedDown:=false;
          fTempSpring:=nil;
        end;
      end;
    end;
    psFalling:begin
      if (y mod 8<>0) then begin
        fY+=SPEEDNORMY*pTimeUsed;
      end else begin
        if fMap.Tiles[pX,pY+2]<>TILE_EMPTY then
          fState:=psIdle
        else
          fY+=SPEEDNORMY*pTimeUsed;
      end;
    end;
    psJumping:begin
      if (SPEEDNORMY*pTimeUsed)<=fRemainingDistance then begin
        fY-=SPEEDNORMY*pTimeUsed;
        fRemainingDistance-=SPEEDNORMY*pTimeUsed;
      end else begin
        fY-=fRemainingDistance;
        fState:=psIdle;
      end;
    end;
  end;
end;

end.


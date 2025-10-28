{
  This file is part of the source code of Frank N Stein Refurbished.
  See "copyright.txt" for details.
}

unit FNSProf;

{$mode delphi}{$H+}

interface

uses SysUtils, Animation2Unit, TileMapUnit, FNSDevice, FNSSpring, FNSMeter;

type

  TPlayerMoveRes=(pmrNone,pmrPickedUpLastPiece,pmrBesideLever);

  { TProf }

  TProf=class
    constructor Create(iMap:TTileMap;iDevice:TDevice;iMeter:TMeter;iSprings:TSprings);
    destructor Destroy; override;
    procedure Draw;
    function Move(pElapsedTime:double):TPlayerMoveRes;  // elapsed time in secs
  private
    fX,fY:double;
    fDirX:integer;
    fRemainingDistance:double;
    fRemainingTime:double;
    fAnimLeft,fAnimRight,fSlideLeft,fSlideRight,fSlideDown,
    fShockingLeft,fShockingRight:TAnimation;
    fMap:TTileMap;
    fState:(psIdle,psFalling,psJumping,psSliding,psSlidingDown,psShockingOriginal,
      psShockingRebooted,psShockedRegenerating);
    fDevice:TDevice;
    fMeter:TMeter;
    fSprings:TSprings;
    fTempSpring:TSpring;
  end;

implementation

uses FNSShared, mk_sdl2, sdl2, logger;

const
  SPEEDX=32;  // pixels / sec
  SPEEDY=64;  // pixels / sec
  MUDSPEEDFACTOR=0.5;
  JUMPHEIGHT=32;        // pixels
  ICESLIDEDISTANCE=8;   // pixels
  POLESLIDEDISTANCE=8;  // pixels
  SHOCKINGTIME=2;       // secs
  SHOCKRECOVERTIME=2;   // secs
  SHOCKTIMEREFILL=60;   // secs


{ TProf }

constructor TProf.Create(iMap:TTileMap; iDevice:TDevice; iMeter:TMeter;
  iSprings:TSprings);
begin
  fAnimLeft:=MM.Animations.ItemByName['ProfLeft'].SpawnAnimation;
  fAnimRight:=MM.Animations.ItemByName['ProfRight'].SpawnAnimation;
  fSlideLeft:=MM.Animations.ItemByName['ProfSlideLeft'].SpawnAnimation;
  fSlideRight:=MM.Animations.ItemByName['ProfSlideRight'].SpawnAnimation;
  fSlideDown:=MM.Animations.ItemByName['ProfSlideDown'].SpawnAnimation;
  fShockingLeft:=MM.Animations.ItemByName['ShockingLeft'].SpawnAnimation;
  fShockingRight:=MM.Animations.ItemByName['ShockingRight'].SpawnAnimation;
  fShockingLeft.LogData;
  fShockingRight.LogData;
  fX:=3*8;
  fY:=2*8;
  fDirX:=1;
  fMap:=iMap;
  fDevice:=iDevice;
  fMeter:=iMeter;
  fSprings:=iSprings;
  fTempSpring:=nil;
end;

destructor TProf.Destroy;
begin
  fShockingRight.Free;
  fShockingLeft.Free;
  fSlideDown.Free;
  fSlideRight.Free;
  fSlideLeft.Free;
  fAnimRight.Free;
  fAnimLeft.Free;
  inherited Destroy;
end;

procedure TProf.Draw;
var x,y:integer;
begin
  x:=trunc(fX);
  y:=trunc(fy);
  if Assigned(fTempSpring) then inc(y);
  case fState of
    psSliding:begin
      if fDirX=1 then
        fSlideRight.PutFrame(x-1,y,(x div 2) mod 8)
      else
        fSlideLeft.PutFrame(x-1,y,(x div 2) mod 8);
    end;
    psSlidingDown:begin
      fSlideDown.PutFrame(x-1,y,0);
    end;
    psShockingOriginal:begin
      if fDirX=1 then
        fShockingRight.PutFrame(x-2,y)
      else
        fShockingLeft.PutFrame(x-2,y);
    end;
    psShockedRegenerating:begin
      if fDirX=1 then begin
        fAnimRight.PutFrame(x-1,y,2);
        fShockingRight.PutFrame(x-2,y)
      end else begin
        fAnimLeft.PutFrame(x-1,y,2);
        fShockingLeft.PutFrame(x-2,y)
      end;
    end
    else begin
      if fDirX=1 then
        fAnimRight.PutFrame(x-1,y,(x div 2+2) mod 8)
      else
        fAnimLeft.PutFrame(x-1,y,(x div 2+2) mod 8);
    end;
  end;
end;

function TProf.Move(pElapsedTime:double):TPlayerMoveRes;
var x,y,px,py,i:integer;
begin
  Result:=pmrNone;
  x:=trunc(fX);
  y:=trunc(fy);
  px:=x div 8;
  py:=y div 8;
  case fState of
    psIdle:begin
      if (keys[SDL_SCANCODE_RIGHT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_RIGHT])
         and ((x mod 8>0)
         or (((x mod 8)=0) and (fMap.Tiles[px+1,py]=TILE_EMPTY) and (fMap.Tiles[px+1,py+1]=TILE_EMPTY))) then begin
        if ((x mod 8>=4) and (fMap.Tiles[px+1,py+2]=TILE_MUD)) or
           ((x mod 8<4) and (fMap.Tiles[px,py+2]=TILE_MUD)) then
          fX+=SPEEDX*pElapsedTime*MUDSPEEDFACTOR
        else
          fX+=SPEEDX*pElapsedTime;
        fDirX:=1;
      end;
      if (keys[SDL_SCANCODE_LEFT] or controllerbuttons[SDL_CONTROLLER_BUTTON_DPAD_LEFT])
         and ((x mod 8>0)
         or (((x mod 8)=0) and (fMap.Tiles[px-1,py]=TILE_EMPTY) and (fMap.Tiles[px-1,py+1]=TILE_EMPTY))) then begin
        if ((x mod 8>=4) and (fMap.Tiles[px+1,py+2]=TILE_MUD)) or
           ((x mod 8<4) and (fMap.Tiles[px,py+2]=TILE_MUD)) then
          fX-=SPEEDX*pElapsedTime*MUDSPEEDFACTOR
        else
          fX-=SPEEDX*pElapsedTime;
        fDirX:=-1;
      end;
      if (x mod 8=0) then begin
        if (fMap.Tiles[px,py+2]=TILE_EMPTY) then fState:=psFalling
        else if fMap.Tiles[px,py+2]=TILE_PIECE+fDevice.NextPiece then begin
          fMap.Tiles[px,py+2]:=TILE_WALL;
          if fDevice.PickupPiece then Result:=pmrPickedUpLastPiece;
        end
        else if fMap.Tiles[px,py+2]=TILE_SPRING then begin
          fTempSpring:=fSprings.SpringAt(pX,pY+2);
          fTempSpring.PushedDown:=true;
          if (keys[SDL_SCANCODE_SPACE] or controllerbuttons[SDL_CONTROLLER_BUTTON_A]) then begin
            fState:=psJumping;
            fRemainingDistance:=JUMPHEIGHT;
            fTempSpring.Kick;
            fTempSpring:=nil;
          end;
        end
        else if fMap.Tiles[px,py+2]=TILE_ICE then begin
          fState:=psSliding;
          fRemainingDistance:=ICESLIDEDISTANCE;
        end
        else if fMap.Tiles[px,py+2]=TILE_ZAPPER then begin
          fState:=psShockingOriginal;
          fRemainingTime:=SHOCKINGTIME;
        end
        else if (fMap.Tiles[px,py+2]=TILE_POLE) and (keys[SDL_SCANCODE_SPACE] or controllerbuttons[SDL_CONTROLLER_BUTTON_A]) then begin
          fState:=psSlidingDown;
          i:=1;
          while (fMap.Tiles[px,py+2+i]=TILE_POLE) do inc(i);
          fRemainingDistance:=POLESLIDEDISTANCE*i;
        end else if (fMap.Tiles[px+1,py+1]=TILE_LEVER) then begin
          Result:=pmrBesideLever;
        end;
      end else begin
        if Assigned(fTempSpring) then begin
          fTempSpring.PushedDown:=false;
          fTempSpring:=nil;
        end;
      end;
    end;
    psFalling:begin
      if (y mod 8<>0) then begin
        fY+=SPEEDY*pElapsedTime;
      end else begin
        if fMap.Tiles[pX,pY+2]<>TILE_EMPTY then
          fState:=psIdle
        else
          fY+=SPEEDY*pElapsedTime;
      end;
    end;
    psJumping:begin
      if (SPEEDY*pElapsedTime)<=fRemainingDistance then begin
        fY-=SPEEDY*pElapsedTime;
        fRemainingDistance-=SPEEDY*pElapsedTime;
      end else begin
        fY-=fRemainingDistance;
        fState:=psIdle;
      end;
    end;
    psSliding:begin
      if (SPEEDX*pElapsedTime)<=fRemainingDistance then begin
        fX+=SPEEDX*pElapsedTime*fDirX;
        fRemainingDistance-=SPEEDX*pElapsedTime;
      end else begin
        fX+=fRemainingDistance*fDirX;
        x:=trunc(fX);
        px:=x div 8;
        if (x mod 8=0) and (fMap.Tiles[px,py+2]=TILE_ICE) then begin
          fState:=psSliding;
          fRemainingDistance:=ICESLIDEDISTANCE;
        end else fState:=psIdle;
      end;
    end;
    psSlidingDown:begin
      if (SPEEDY*pElapsedTime)<=fRemainingDistance then begin
        fY+=SPEEDY*pElapsedTime;
        fRemainingDistance-=SPEEDY*pElapsedTime;
      end else begin
        fY:=round(fy+fRemainingDistance);
        if fMap.Tiles[pX,pY+2]<>TILE_EMPTY then
          fState:=psIdle
        else
          fState:=psFalling;
      end;
    end;
    psShockingOriginal:begin
      fShockingLeft.Animate(pElapsedTime);
      fShockingRight.Animate(pElapsedTime);
      if pElapsedTime<fRemainingTime then begin
        fMeter.Refill(SHOCKTIMEREFILL/SHOCKINGTIME*pElapsedTime);
        fRemainingTime:=fRemainingTime-pElapsedTime;
      end else begin
        fMeter.Refill(SHOCKTIMEREFILL/SHOCKINGTIME*fRemainingTime);
        fRemainingTime:=SHOCKRECOVERTIME;
        fState:=psShockedRegenerating;
        fMap.Tiles[px,py+2]:=TILE_WALL;
        fShockingLeft.Timer.CurrentFrameIndex:=0;
        SDL_SetTextureAlphaMod(fShockingLeft.Texture.Texture,255);
        fShockingRight.Timer.CurrentFrameIndex:=0;
        SDL_SetTextureAlphaMod(fShockingRight.Texture.Texture,255);
      end;
    end;
    psShockedRegenerating:begin
      i:=trunc(255*fRemainingTime/SHOCKRECOVERTIME);
      SDL_SetTextureAlphaMod(fShockingLeft.Texture.Texture,i);
      SDL_SetTextureAlphaMod(fShockingRight.Texture.Texture,i);
      if pElapsedTime<fRemainingTime then begin
        fRemainingTime:=fRemainingTime-pElapsedTime;
      end else begin
        fRemainingTime:=0;
        fState:=psIdle;
        SDL_SetTextureAlphaMod(fShockingLeft.Texture.Texture,255);
        SDL_SetTextureAlphaMod(fShockingRight.Texture.Texture,255);
      end;
    end;
  end;
end;

end.


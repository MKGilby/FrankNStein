unit FNSMapSelect;

{$mode ObjFPC}{$H+}
{$define LimitFPS}

interface

uses FNSMapStatic;

type

  { TMapSelect }

  TMapSelect=class
    constructor Create(iStartMapNo:integer);
    destructor Destroy; override;
    function Run:integer;
  private
    fCurrentMapNo:integer;
    fMapStatic:TMapStatic;
  end;

implementation

uses mk_sdl2, sdl2;

{ TMapSelect }

constructor TMapSelect.Create(iStartMapNo:integer);
begin
  fCurrentMapNo:=iStartMapNo;
  fMapStatic:=TMapStatic.Create(fCurrentMapNo);
end;

destructor TMapSelect.Destroy;
begin
  if Assigned(fMapStatic) then fMapStatic.Free;
  inherited Destroy;
end;

function TMapSelect.Run:integer;
begin
  Result:=0;
  ClearKeys;
  repeat
    fMapStatic.Draw;

    {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    HandleMessages;
    if keys[SDL_SCANCODE_ESCAPE] then Result:=-1;
    if keys[SDL_SCANCODE_RETURN] or keys[SDL_SCANCODE_SPACE] then Result:=1;
    if controllerbuttons[SDL_CONTROLLER_BUTTON_A] then Result:=1;
    if controllerbuttons[SDL_CONTROLLER_BUTTON_B] then Result:=-1;
    if Terminate then Result:=-1;
  until Result<>0;
  ClearControllerButtons;
end;

end.


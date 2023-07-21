unit FNSSlotSelector;

{$mode ObjFPC}{$H+}

interface

uses
  Animation2Unit;

type

  { TSlotSelector }

  TSlotSelector=class
    constructor Create;
    destructor Destroy; override;
  private
    fProfs:array[0..2] of TAnimation;
  end;

implementation

uses FNSShared;

{ TSlotSelector }

constructor TSlotSelector.Create;
begin
  fProfs[0]:=MM.Animations.ItemByName['ProfRight'].SpawnAnimation;
  fProfs[1]:=MM.Animations.ItemByName['ProfRight'].SpawnAnimation;
  fProfs[2]:=MM.Animations.ItemByName['ProfRight'].SpawnAnimation;
end;

destructor TSlotSelector.Destroy;
begin
  fProfs[0].Free;
  fProfs[1].Free;
  fProfs[2].Free;
  inherited Destroy;
end;

end.


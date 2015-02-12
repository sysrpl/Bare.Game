unit Sprites.Cat;

{$mode delphi}

interface

uses
  Bare.System,
  Bare.Graphics,
  Bare.Geometry,
  Bare.Example;

{ TRunningCat }

type
  TRunningCat = class(TSprite)
  private
    class var Textures: TTextures;
    class var TexturesCount: Integer;
  private
    FFrame: Integer;
    procedure SetFrame(Value: Integer);
  protected
    procedure Render; override;
  public
    constructor Create(World: TWorld); override;
    destructor Destroy; override;
    property Frame: Integer read FFrame write SetFrame;
  end;

implementation

{ TRunningCat }

constructor TRunningCat.Create(World: TWorld);
const
  CatFile = 'images/cat.png';
  TexCat = 0;
begin
  inherited Create(World);
  if TexturesCount = 0 then
  begin
    Textures := TTextures.Create;
    Textures.Generate(1);
    Textures.Load(Download(CatFile), TexCat);
  end;
  Inc(TexturesCount);
  Texture := Textures[TexCat];
  Size := Vec2(512, 256);
end;

destructor TRunningCat.Destroy;
begin
  Dec(TexturesCount);
  if TexturesCount = 0 then
    Textures.Free;
  inherited Destroy;
end;

procedure TRunningCat.SetFrame(Value: Integer);
const
  MaxFrame = 8;
begin
  FFrame := Abs(Value mod MaxFrame);
end;

procedure TRunningCat.Render;
const
  VertFrame = 0.25;
var
  I: Integer;
begin
  if Odd(FFrame) then
  begin
    TexCoords.S0 := 0.5;
    TexCoords.S1 := 1;
  end
  else
  begin
    TexCoords.S0 := 0;
    TexCoords.S1 := 0.5;
  end;
  I := FFrame div 2;
  TexCoords.T0 := I * VertFrame;
  TexCoords.T1 := (I + 1) * VertFrame;
  inherited Render;
end;

end.


program draw;

{$mode delphi}

uses
  Bare.System,
  Bare.Game,
  Bare.Geometry,
  Bare.Graphics,
  Bare.Animation,
  Bare.Example,
  Bare.Interop.OpenGL;

{ TDrawExample }

type
  TDrawExample = class(TWorldWindow)
  private
    FBackground: TBackgroudSprite;
    FSprite: TSprite;
    FDrawing: Boolean;
    FEasing: TEasing;
    FPerspectiveView: Boolean;
    FPerspectiveTime: Float;
    FPerspectiveFactor: Float;
    FX, FY: Float;
  protected
    procedure RenderInitialize; override;
    procedure RenderFinalize; override;
    procedure Logic(Stopwatch: TStopwatch); override;
    procedure Render(Stopwatch: TStopwatch); override;
  end;

procedure TDrawExample.RenderInitialize;
begin
  inherited RenderInitialize;
  { Change the window caption }
  Caption := 'Drawing Demo';
  { We're drawing our own cursor }
  Mouse.Visible := False;
  { We're going to use an easing in this sample }
  Easings.RegisterDefaults;
  FEasing := Easings['Bounce'];
  FPerspectiveTime := -1;
  { Setup our pen }
  Pen.Color := clHotPink;
  Pen.Width := 10;
  { Make room for 3 images }
  Textures.Generate(3);
  Textures.Load('cracked.jpg', 0);
  Textures.Load('background.jpg', 1);
  Textures.Load('cursor.png', 2);
  { If you create it }
  FBackground := TBackgroudSprite.Create(World);
  FBackground.Origin := Vec(0, 0);
  FSprite := TSprite.Create(World);
  FSprite.Texture := Textures[2];
  FSprite.Size := Vec(100, 148);
  FSprite.Origin := Vec(0.2, 0.1);
end;

procedure TDrawExample.RenderFinalize;
begin
  { You must destroy it }
  FSprite.Free;
  FBackground.Free;
  inherited RenderFinalize;
end;

procedure TDrawExample.Logic(Stopwatch: TStopwatch);
begin
  { Place your game logic code here }
  if (Keyboard.Key[VK_F2]) and (not FPerspectiveView) then
  begin
    { If the F2 key is pressed switch to a perspective view }
    FPerspectiveView := True;
    FPerspectiveTime := Stopwatch.Time;
  end;
  if (Keyboard.Key[VK_F3]) and FPerspectiveView then
  begin
    { If the F3 key is pressed switch to am othographic view }
    FPerspectiveView := False;
    FPerspectiveTime := Stopwatch.Time;
  end;
  { Animate the view changes using an easing }
  FPerspectiveFactor := Interpolate(FEasing, Stopwatch.Time - FPerspectiveTime);
  if not FPerspectiveView then
    FPerspectiveFactor := 1 - FPerspectiveFactor;
  if mbLeft in Mouse.Buttons then
  begin
    { Draw when the left mouse button is down }
    if not FDrawing then
    begin
      Canvas.Path.Clear;
      Canvas.Path.MoveTo(Mouse.X, Mouse.Y)
    end
    else if (Mouse.X <> FX) or (Mouse.Y <> FY) then
      Canvas.Path.LineTo(Mouse.X, Mouse.Y);
    FDrawing := True;
  end
  else
    FDrawing := False;
  { Capture the last mouse X and Y coords }
  FX := Mouse.X;
  FY := Mouse.Y;
end;

procedure TDrawExample.Render(Stopwatch: TStopwatch);
const
  Instructions = 'Press the left mouse button to draw';
  Status = 'Time: %.2f'#13#10'FPS: %d';
  Help = 'Press ESC to terminate - F1 Fullscreen toggle'#13#10 +
    'F2 Perspective view - F3 Orthographic view';
begin
  { Place your game render code here }
  World.Update;
  { Make our cracked fill the screen }
  FBackground.Size.X := World.Width;
  FBackground.Size.Y := World.Height;
  FBackground.Texture := Textures[0];
  FBackground.Draw;
  { You can mix in opengl code if you want, here we add perspective manually,
    you could also use the camera class }
  glRotatef(20 * FPerspectiveFactor, 0, 1, 0);
  glRotatef(20 * FPerspectiveFactor, 1, 0, 0);
  glTranslatef(10 * FPerspectiveFactor, -8 * FPerspectiveFactor, -4 * FPerspectiveFactor);
  { Make our background fill the world }
  FBackground.Texture := Textures[1];
  FBackground.Draw;
  { Write some instructions }
  Font.Write(Instructions, 1, World.Width / 2, World.Height / 2, justifyCenter);
  { Stroke the current path }
  Canvas.Stroke(Pen, False);
  { Move the cursor sprite around }
  FSprite.Position := Vec(FX, FY, 0);
  FSprite.Rotation.Z := Sin(Stopwatch.Time * 4) * 20;
  FSprite.Draw;
  { Write some more text }
  Font.Write(Format(Status, [Stopwatch.Time, Stopwatch.Framerate]), 0.75, 1, 0);
  Font.Write(Help, 0.75, World.Width / 2, World.Height - 50, justifyCenter);
end;

begin
  Application.Run(TDrawExample);
end.

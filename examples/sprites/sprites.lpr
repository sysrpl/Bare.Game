program sprites;

{$mode delphi}

uses
  Bare.System,
  Bare.Types,
  Bare.Game,
  Bare.Geometry,
  Bare.Graphics,
  Bare.Example,
  Bare.Interop.OpenGL,
  Sprites.Web,
  Sprites.Cat;

{ TSpriteWindow }

type
  TSpriteWindow = class(TWorldWindow)
	private
    FBroken: TBackgroudSprite;
    FSky: TBackgroudSprite;
    FClouds: TBackgroudSprite;
    FMountains: TBackgroudSprite;
    FGround: TBackgroudSprite;
    FRunningCat: TRunningCat;
    FShow3D: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure RenderInitialize; override;
    procedure RenderFinalize; override;
    procedure Logic(Stopwatch: TStopwatch); override;
    procedure Render(Stopwatch: TStopwatch); override;
  end;

procedure TSpriteWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  { Setup our window options here }
  Params.Caption := 'Sprite Demo';
  Params.Width := 1280;
  Params.Height := 720;
  { Just for fun, we'll use seperate threads for UI, Logic, and Render }
  Params.Multithreaded := True;
end;

procedure TSpriteWindow.RenderInitialize;
const
  { Location to download resources }
  MusicUrl = 'sounds/mario.wav';
  { Image engine supports jpg, png, gif, and bmp formats }
  TexBrokenUrl = 'images/broken.jpg';
  TexSkyUrl = 'images/sky.gif';
  TexCloudsUrl = 'images/clouds.png';
  TexMountainsUrl = 'images/mountains.png';
  TexGroundUrl = 'images/ground.gif';
  TexBroken = 0;
  TexSky = TexBroken + 1;
  TexClouds = TexSky + 1;
  TexMountains = TexClouds + 1;
  TexGround = TexMountains + 1;
begin
  inherited RenderInitialize;
  { Hide the cursor }
  Mouse.Visible := False;
	{ If we draw something, let's use a 2 pixel wide yellow pen }
	Pen.Color := clYellow;
  Pen.Width := 2;
  { Load a looping audio track from the internet into bank zero }
	Audio.Banks[0].Looping := True;
	Audio.Samples.Add(WebGet(MusicUrl)).Play;
  { Make room for our textures }
  Textures.Generate(TexGround + 1);
  { Load a some textures from the internet }
  Textures.Load(WebGet(TexBrokenUrl), TexBroken);
  Textures.Load(WebGet(TexSkyUrl), TexSky);
  Textures.Load(WebGet(TexCloudsUrl), TexClouds);
  Textures.Load(WebGet(TexMountainsUrl), TexMountains);
  Textures.Magnify[TexMountains] := filterNearest;
  Textures.Load(WebGet(TexGroundUrl), TexGround);
  Textures.Magnify[TexGround] := filterNearest;
  { If you create it }
  FBroken := TBackgroudSprite.Create(World);
  FBroken.Texture := Textures[TexBroken];
  FBroken.Origin := Vec2(0, 0);
  FSky := TBackgroudSprite.Create(World);
  FSky.Texture := Textures[TexSky];
  FSky.Origin := Vec2(0, 0);
  FClouds := TBackgroudSprite.Create(World);
  FClouds.TileMode := tileRepeat;
  FClouds := TBackgroudSprite.Create(World);
  FClouds.TileMode := tileRepeat;
  FClouds.Texture := Textures[TexClouds];
  FMountains := TBackgroudSprite.Create(World);
  FMountains.Scale := Vec2(3);
  FMountains.TileMode := tileRepeat;
  FMountains.Texture := Textures[TexMountains];
  FMountains.Origin := Vec2(0, 0);
  FGround := TBackgroudSprite.Create(World);
  FGround.Scale := Vec2(4);
  FGround.TileMode := tileRepeat;
  FGround.Texture := Textures[TexGround];
  FGround.Origin := Vec2(0, 0);
  { Our running cat is derived from a sprite in Sprites.Cat }
  FRunningCat := TRunningCat.Create(World);
end;

procedure TSpriteWindow.RenderFinalize;
begin
  { You must destroy it }
  FSky.Free;
  FClouds.Free;
  FMountains.Free;
  FGround.Free;
  FRunningCat.Free;
  inherited RenderFinalize;
end;

procedure TSpriteWindow.Logic(Stopwatch: TStopwatch);
begin
  { Since we can use multiple threads for UI, Logic, and Render
    MessageQueue is the thread safe way to deal with reading
    joystick, keyboard, and mouse events }
  MessageQueue.Remove;
  if MessageQueue.KeyDown(VK_F2) then
    {  Toggle a 3D perspective when F2 is pressed }
		FShow3D := not FShow3D;
end;

procedure TSpriteWindow.Render(Stopwatch: TStopwatch);
const
  Message = 'Time: %.2f'#10'Framerate: %d';
var
  P: TPolygon;
  S: string;
begin
  World.Update;
  FBroken.Size := Vec2(World.Width, World.Height);
  FBroken.Draw;
  if FShow3D then
  begin
    {  A 3D perspective when F2 is toggled }
  	glTranslatef(0, 0, -4);
	  glRotatef(Sin(Stopwatch.Time * 2) * 20, 1, 0, 0);
  end;
  { Draw some sprites }
  FSky.Size := FBroken.Size;
  FSky.Draw;
  { Clouds drift as time passes }
  FClouds.Size := FBroken.Size;
  FClouds.Origin.X := -Stopwatch.Time / 30;
  FClouds.Origin.Y := Sin(Stopwatch.Time / 3) / 13;
  FClouds.Scale := Vec2(1.5);
  FClouds.Draw;
  { Add a second layer of clouds at a different scale }
  FClouds.Scale := Vec2(1);
  FClouds.Origin.Y := Sin(Stopwatch.Time / 2) / 10;
  FClouds.Origin.X := -Stopwatch.Time / 23 + 0.2;
  FClouds.Draw;
  { Draw the mountains }
  FMountains.Origin.X := -Stopwatch.Time / 10;
  FMountains.Size := Vec2(World.Width, 480);
  FMountains.Position.Y := World.Height - 480;
  FMountains.Draw;
  { And the ground }
  FGround.Origin.X := -Stopwatch.Time / 1.75;
  FGround.Size := Vec2(World.Width, 48 * 2);
  FGround.Position.Y := World.Height - 48 * 2;
  FGround.Draw;
  { Write out some text }
  S := Format(Message, [Stopwatch.Time, Stopwatch.Framerate]);
  Font.Write(S, 1, 1, 0);
  Font.Write('Press F1 to toggle fullscreen - Press F2 to toggle 3D', 1,
  	World.Width / 2, World.Height - 30, justifyCenter);
  if FShow3D then
  begin
    { If we're showing in a 3D perspective, move things around in 3D }
    FRunningCat.Position.Z := Sin(Stopwatch.Time * 2) * 3 + 4;
  	FRunningCat.Rotation.Y := Sin(Stopwatch.Time * 4) * 20;
  end
  else
  begin
    FRunningCat.Position.Z := 0;
    FRunningCat.Rotation.Y := 0;
  end;
  FRunningCat.Position.X := World.Width / 2;
  FRunningCat.Position.Y := World.Height - 215;
  { Our cat is running based on the current time }
  FRunningCat.Frame := Round(Stopwatch.Time * 12);
  FRunningCat.Draw;
  if FShow3D then
  begin
    { If we're showing in a 3D perspective, stroke a yellow box around the cat }
		FRunningCat.GetPolygon(P);
	  Canvas.Path.Polygon(P);
  	Canvas.Stroke(Pen);
  end;
end;

begin
  Application.Run(TSpriteWindow);
end.


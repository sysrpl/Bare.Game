(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.example.txt> }
unit Bare.Example;

{$mode delphi}

interface

uses
  Bare.System,
  Bare.Game,
  Bare.Geometry,
  Bare.Graphics,
  Bare.Graphics.Drawing,
  Bare.Networking.Web,
  Bare.Interop.OpenGL;

{ TWorldWindow }

type
  TWorldWindow = class(TWindow)
  private
    FWorld: TWorld;
    FCanvas: TCanvas;
    FPen: TPen;
    FFont: TFontWriter;
    FTextures: TTextures;
    FWebStream: TStream;
    FWebUrl: string;
    FWebCounter: Integer;
    procedure WebProgress(Sender: TObject; var Args: TWebProgressArgs);
    function WebLoad(const Url: string): TStream;
    procedure SetPen(Value: TPen);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoKeyUp(var Args: TKeyboardArgs); override;
    procedure RenderInitialize; override;
    procedure RenderFinalize; override;
  public
    property World: TWorld read FWorld;
    property Canvas: TCanvas read FCanvas;
    property Pen: TPen read FPen write SetPen;
    property Font: TFontWriter read FFont;
    property Textures: TTextures read FTextures;
  end;

var
  DownloadSource: string;
  Download: function(const Url: string): TStream of object;

implementation

{ TWorldWindow }

procedure TWorldWindow.WebProgress(Sender: TObject; var Args: TWebProgressArgs);
var
  X, Y, Percent: Float;
begin
  Inc(FWebCounter);
  if (FWebCounter mod 2) = 0 then
    Exit;
  World.Update;
  if Args.ContentLength > 0 then
	begin
    Percent := Args.ReadLength * 1.0 / Args.ContentLength;
    Font.Write(Format('Loading %s %d% complete', [FWebUrl,
      Round(Percent * 100)]), 1, 1, 0)
  end
  else
  begin
    Percent := 0;
    Font.Write(Format('Loading %s (%d bytes read)', [FWebUrl, Args.ReadLength]), 1, 1, 0);
  end;
  X := World.Width;
  Y := World.Height / 2;
  Canvas.Path.MoveTo(20, Y - 20);
  Canvas.Path.LineTo(X - 20, Y - 20);
  Canvas.Path.LineTo(X - 20, Y + 20);
  Canvas.Path.LineTo(20, Y + 20);
  Canvas.Path.LineTo(20, Y - 20);
  Canvas.Path.Close;
  Canvas.Path.MoveTo(30, Y - 10);
  Canvas.Path.LineTo(30 + Percent * (X - 60), Y - 10);
  Canvas.Path.LineTo(30 + Percent * (X - 60), Y + 10);
  Canvas.Path.LineTo(30, Y + 10);
  Canvas.Path.LineTo(30, Y - 10);
  Canvas.Stroke(Pen);
  SwapBuffers;
end;

function TWorldWindow.WebLoad(const Url: string): TStream;
var
  Client: TWebClient;
  S: string;
begin
 	FWebUrl := Url;
  FWebStream.Free;
  FWebStream := TMemoryStream.Create;
  S := PathAdjustDelimiters(Url);
  if FileExists(S) then
    FWebStream.LoadFromFile(S)
  else
  begin
    Client := TWebClient.Create;
    try
      Client.OnProgress := WebProgress;
    finally
      Client.Get(DownloadSource + Url, FWebStream);
    end;
    DirCreate(FileExtractPath(S));
    FWebStream.SaveToFile(S);
  end;
  FWebStream.Position := 0;
  Result := FWebStream;
end;

procedure TWorldWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style + [wsResizeable];
end;

procedure TWorldWindow.DoKeyUp(var Args: TKeyboardArgs);
begin
  inherited DoKeyUp(Args);
  if Args.Key = VK_F1 then
		Fullscreen := not Fullscreen;
end;

procedure TWorldWindow.RenderInitialize;
begin
  Download := WebLoad;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_SMOOTH);
  glDisable(GL_TEXTURE_2D);
  glClearColor(0, 0, 1, 1.0);
  glColor4f(1.0, 1.0, 1.0, 1.0);
  FWorld := TWorld.Create(Self);
  FCanvas := TCanvas.Create(FWorld);
  FPen := TPen.Create(clBlack);
  FFont := TFontWriter.Create(FWorld);
  FontLoadDefault(FFont);
  FTextures := TTextures.Create;
end;

procedure TWorldWindow.RenderFinalize;
begin
  FTextures.Free;
  FFont.Free;
  FPen.Free;
  FCanvas.Free;
  FWorld.Free;
  FWebStream.Free;
end;

procedure TWorldWindow.SetPen(Value: TPen);
begin
  Assign(Value, Pen);
end;

end.


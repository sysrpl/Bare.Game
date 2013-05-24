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

implementation

{ TWorldWindow }

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
end;

procedure TWorldWindow.SetPen(Value: TPen);
begin
  Assign(Value, Pen);
end;

end.


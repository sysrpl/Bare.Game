(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.graphics.drawing.txt> }
unit Bare.Graphics.Drawing;

{$i bare.inc}

interface

{TODO: Consider using interfaces}
{TODO: Define brushes of type solid, gradient, pattern}
{TODO: Give pen a brush option}
{TODO: Write two pass shaders for rendering
  1st pass draw the brush writing to depth buffer
  2nd pass draw only anti-aliased edges beyond the bounds of the depth buffer}

uses
  Bare.System,
  Bare.Types,
  Bare.Animation,
  Bare.Geometry,
  Bare.Graphics,
  Bare.Graphics.Imaging,
  Bare.Interop.OpenGL;

{ TSubPath is a container for vector lines and curves
  See also
  <link Overview.Bare.Graphics.Drawing.TSubPath, TSubPath members>
  <link Bare.Graphics.Drawing.TPath, TPath class> }

type
  TSubPath = class(TPersistsObject)
  private
    FClosed: Boolean;
    FCurves: TList<TCurve2>;
    function GetDistance: Float;
  protected
    { Copy source to the sub path }
    function AssignFrom(Source: TObject): Boolean; override;
  public
    {doc ignore}
    constructor Create;
    destructor Destroy; override;
    { Create a copy of the sub path }
    function Clone: TSubPath;
    { Find a heading normal at a distance along the sub path }
    function FindNormal(Dist: Float): TVec2;
    { Find a point at a distance along the sub path }
    function FindPoint(Dist: Float): TVec2;
    { Total distance over the sub path }
    property Distance: Float read GetDistance;
  end;

  {doc ignore}
  TSubPaths = TObjectList<TSubPath>;

{ TPath is a tool for creating vector lines and curves
  See also
  <link Overview.Bare.Graphics.Drawing.TPath, TPath members> }

  TPath = class(TPersistsObject)
  private
    FSubPaths: TSubPaths;
    FPosition: TVec2;
    function GetCurrent: TSubPath;
    function GetSubPath(Index: Integer): TSubPath;
    function GetCount: Integer;
  protected
    { Copy source to the path }
    function AssignFrom(Source: TObject): Boolean; override;
  public
    { Create an empty path object }
    constructor Create;
    destructor Destroy; override;
    { The enumerator type }
    type TEnumerator = TSubPaths.TEnumerator;
    { Returns a sub path enumerator }
    function GetEnumerator: TEnumerator;
    { Create a copy of the path }
    function Clone: TPath;
    { Create a new sub path leaving the previous path opened }
    procedure Open;
    { Create a new sub path leaving the previous path closed }
    procedure Close;
    { Remove all sub paths }
    procedure Clear;
    { Create a new elliptical sub path given a rect }
    procedure Ellipse(const Rect: TRectF); overload;
    { Create a new rectangular sub path given four components }
    procedure Rectangle(X, Y, Width, Height: Float); overload;
    { Create a new retangular sub path given a rect }
    procedure Rectangle(const Rect: TRectF); overload;
    { Create a new polygon sub path and optionally close it }
    procedure Polygon(Shape: TPolygon; Closed: Boolean = True); overload;
    { Create a new sub path with a starting point x, y }
    procedure MoveTo(X, Y: Float); overload;
    { Create a new sub path with a starting point p }
    procedure MoveTo(const P: TVec2); overload;
    { Add a line to x, y }
    procedure LineTo(X, Y: Float); overload;
    { Add a line to point p }
    procedure LineTo(const P: TVec2); overload;
    { Add a curve from using two control points and an end point }
    procedure CurveTo(const C0, C1, P1: TVec2);
    { The current sub path }
    property Current: TSubPath read GetCurrent;
    { The last used point }
    property Position: TVec2 read FPosition;
    { The default indexer of type <link Bare.Graphics.Drawing.TSubPath, TSubPath> }
    property SubPath[Index: Integer]: TSubPath read GetSubPath; default;
    { The number of sub paths }
    property Count: Integer read GetCount;
  end;

{ TPen is used to stroke paths
  <link Overview.Bare.Graphics.Drawing.TPen, TPen members>
  <link Bare.Graphics.Drawing.TCanvas, TCanvas class> }

  TPen = class(TPersistsObject)
  private
    FColor: TVec4Prop;
    FWidth: TVec1Prop;
    procedure SetColor(const Value: TVec4Prop);
    procedure SetWidth(const Value: TVec1Prop);
  protected
    { Copy source to the pen }
    function AssignFrom(Source: TObject): Boolean; override;
  public
    { Create a pen with a color and optional width }
    constructor Create(Color: TColorF; Width: Float = 1);
    { Create a copy of the pen }
    function Clone: TPen;
    { Color used when stroking paths }
    property Color: TVec4Prop read FColor write SetColor;
    { Width of the stroke }
    property Width: TVec1Prop read FWidth write SetWidth;
  end;

{TODO: Make brush abstract and add TSolidBrush, TGradientBrush, and TPatternBrush }

{ TBrush is used to fill paths
  <link Overview.Bare.Graphics.Drawing.TBrush, TBrush members>
  <link Bare.Graphics.Drawing.TCanvas, TCanvas class> }

  TBrush = class(TPersistsObject)
  private
    FColor: TVec4Prop;
    procedure SetColor(const Value: TVec4Prop);
  protected
    { Copy source to the brush }
    function AssignFrom(Source: TObject): Boolean; override;
  public
    { Create a pen with a color  }
    constructor Create(Color: TColorF);
    { Create a copy of the brush }
    function Clone: TBrush;
    { Color used when filling paths }
    property Color: TVec4Prop read FColor write SetColor;
  end;

{ TCanvas is a 2d vector graphics drawing surface
  See also
  <link Overview.Bare.Graphics.Drawing.TCanvas, TCanvas members> }

  TCanvas = class
  private
    FWorld: TWorld;
    FPath: TPath;
    FTextures: TTextures;
  public
    { Create a canvas for a world }
    constructor Create(World: TWorld);
    destructor Destroy; override;
    { Stroke an outline of the path using a pen and optionally clear the path
      Remarks
      The depth buffer will be cleared after this method is called }
    procedure Stroke(Pen: TPen; Clear: Boolean = True);
    { Fill the interior of the path using a brush and optionally clear the path
      Remarks
      The depth buffer will be cleared after this method is called }
    procedure Fill(Brush: TBrush; Clear: Boolean = True);
    { Path contains all line and curve data }
    property Path: TPath read FPath;
  end;

implementation

const
  Sigma = 0.001;

{ TSubPath }

constructor TSubPath.Create;
begin
  inherited Create;
  FCurves := TList<TCurve2>.Create;
end;

destructor TSubPath.Destroy;
begin
  FCurves.Free;
  inherited Destroy;
end;

function TSubPath.AssignFrom(Source: TObject): Boolean;
var
  SubPath: TSubPath;
  Curve: TCurve2;
  I: Integer;
begin
  if Source is TSubPath then
  begin
    SubPath := Source as TSubPath;
    FClosed := SubPath.FClosed;
    FCurves.Clear;
    FCurves.Capacity := SubPath.FCurves.Count;
    for I := 0 to SubPath.FCurves.Count do
    begin
			Curve := FCurves[I];
      { This makes the arrays unique ... don't ask me why there isn't more ergonomic way }
      SetLength(Curve.D, Length(Curve.D));
      SetLength(Curve.P, Length(Curve.P));
      FCurves.Add(Curve);
    end;
    Result := True;
  end
  else
    Result := inherited AssignFrom(Source)
end;

function TSubPath.Clone: TSubPath;
begin
  Result := TSubPath.Create;
  Assign(Self, Result);
end;

function TSubPath.FindNormal(Dist: Float): TVec2;
var
  Curve: TCurve2;
begin
  {TODO: Consider weighing neighboring normals}
  if FCurves.Count < 1 then
    Exit(Vec2(0, 1));
  if Dist < Sigma then
    Exit(FCurves[0].FindNormal(0));
  for Curve in FCurves do
    if Dist < Curve.Distance then
      Exit(Curve.FindNormal(Dist))
    else
      Dist := Dist - Curve.Distance;
  Curve := FCurves[FCurves.Count - 1];
  Result := Curve.FindNormal(Curve.Distance);
end;

function TSubPath.FindPoint(Dist: Float): TVec2;
var
  Curve: TCurve2;
begin
  if FCurves.Count < 1 then
    Exit(Vec2(0, 0));
  if Dist < Sigma then
    Exit(FCurves[0].P[0]);
  for Curve in FCurves do
    if Dist < Curve.Distance then
      Exit(Curve.FindPoint(Dist))
    else
      Dist := Dist - Curve.Distance;
  Curve := FCurves[FCurves.Count - 1];
  Result := Curve.P[High(Curve.P)];
end;

function TSubPath.GetDistance: Float;
var
  C: TCurve2;
begin
  Result := 0;
  for C in FCurves do
    Result := Result + C.Distance;
end;

{ TPath }

constructor TPath.Create;
begin
  inherited Create;
  FSubPaths := TSubPaths.Create(True);
end;

destructor TPath.Destroy;
begin
  FSubPaths.Free;
  inherited Destroy;
end;

function TPath.GetEnumerator: TEnumerator;
begin
  Result := FSubPaths.GetEnumerator;
end;

function TPath.AssignFrom(Source: TObject): Boolean;
var
  Path: TPath;
  SubPath: TSubPath;
begin
  if Source is TPath then
  begin
    Path := Source as TPath;
    FSubPaths.Clear;
    FSubPaths.Capacity := Path.FSubPaths.Count;
    for SubPath in Path.FSubPaths do
      FSubPaths.Add(SubPath.Clone);
    FPosition := Path.Position;
    Result := True;
  end
  else
    Result := inherited AssignFrom(Source);
end;

function TPath.Clone: TPath;
begin
  Result := TPath.Create;
  Assign(Self, Result);
end;

procedure TPath.Open;
var
  SubPath: TSubPath;
begin
  SubPath := Current;
  if (SubPath = nil) or (SubPath.FCurves.Count > 0) then
    FSubPaths.Add(TSubPath.Create);
end;

procedure TPath.Close;
var
  SubPath: TSubPath;
  Curve: TCurve2;
  V: TVec2;
  D: Float;
  I: Integer;
begin
  SubPath := Current;
  if SubPath = nil then
    Exit;
  I := SubPath.FCurves.Count - 1;;
  if I < 0 then
    Exit;
  if Length(SubPath.FCurves[I].P) > 2 then
  begin
    V := SubPath.FCurves[I].P[0];
    D := FPosition.Distance(V);
    if D > Sigma then
    begin
      SetLength(Curve.P, 2);
      Curve.P[0] := FPosition;
      Curve.P[1] := V;
      SetLength(Curve.D, 1);
      Curve.D[0] := D;
      Curve.Distance := D;
      SubPath.FCurves.Add(Curve);
    end;
    SubPath.FClosed := True;
    FPosition := V;
  end;
  FSubPaths.Add(TSubPath.Create);
end;

procedure TPath.Clear;
begin
  FSubPaths.Clear;
end;

procedure TPath.Ellipse(const Rect: TRectF);
const
  OneThird = 1.333;
var
  SubPath: TSubPath;
  Bezier: TBezier2;
  V: TVec2;
begin
  Open;
  SubPath := Current;
  V := Vec2(OneThird * Rect.Width / 2, 0);
  Bezier := Bezier2(Rect.MidTop, V.Move(Rect.MidTop), V.Move(Rect.MidBottom), Rect.MidBottom);
  SubPath.FCurves.Add(Bezier.Flatten);
  V := -V;
  Bezier := Bezier2(Rect.MidBottom, V.Move(Rect.MidBottom), V.Move(Rect.MidTop), Rect.MidTop);
  SubPath.FCurves.Add(Bezier.Flatten);
  FPosition := Rect.MidTop;
  Close;
end;

procedure TPath.Rectangle(X, Y, Width, Height: Float);
begin
  Rectangle(TRectF.Create(X, Y, Width, Height));
end;

procedure TPath.Rectangle(const Rect: TRectF);
var
  SubPath: TSubPath;
  Curve: TCurve2;
begin
  Open;
  SubPath := Current;
  SetLength(Curve.P, 2);
  SetLength(Curve.D, 1);
  Curve.P[0] := Rect.TopLeft;
  Curve.P[1] := Rect.TopRight;
  Curve.D[0] := Abs(Rect.Width);
  Curve.Distance := Curve.D[0];
  SubPath.FCurves.Add(Curve);
  SetLength(Curve.P, 2);
  SetLength(Curve.D, 1);
  Curve.P[0] := Rect.TopRight;
  Curve.P[1] := Rect.BottomRight;
  Curve.D[0] := Abs(Rect.Height);
  Curve.Distance := Curve.D[0];
  SubPath.FCurves.Add(Curve);
  SetLength(Curve.P, 2);
  SetLength(Curve.D, 1);
  Curve.P[0] := Rect.BottomRight;
  Curve.P[1] := Rect.BottomLeft;
  Curve.D[0] := Abs(Rect.Width);
  Curve.Distance := Curve.D[0];
  SubPath.FCurves.Add(Curve);
  FPosition := Rect.BottomLeft;
  Close;
end;

procedure TPath.Polygon(Shape: TPolygon; Closed: Boolean = True);
var
  SubPath: TSubPath;
begin
  if Length(Shape) < 2 then
    Exit;
  Open;
  SubPath := Current;
  SubPath.FCurves.Add(Shape);
  FPosition := Shape[High(Shape)];
  if Closed then
    Close
  else
    Open;
end;

procedure TPath.MoveTo(X, Y: Float);
begin
  Open;
  FPosition.X := X;
  FPosition.Y := Y;
end;

procedure TPath.MoveTo(const P: TVec2);
begin
  Open;
  FPosition := P;
end;

procedure TPath.LineTo(X, Y: Float);
begin
  LineTo(Vec2(X, Y));
end;

procedure TPath.LineTo(const P: TVec2);
var
  Curve: TCurve2;
  SubPath: TSubPath;
begin
  if FPosition.Distance(P) < Sigma then
    Exit;
  SetLength(Curve.P, 2);
  Curve.P[0] := FPosition;
  Curve.P[1] := P;
  SetLength(Curve.D, 1);
  Curve.D[0] := FPosition.Distance(P);
  Curve.Distance := Curve.D[0];
  SubPath := Current;
  if SubPath = nil then
  begin
    SubPath := TSubPath.Create;
    FSubPaths.Add(SubPath);
  end;
  SubPath.FCurves.Add(Curve);
  FPosition := P;
end;

procedure TPath.CurveTo(const C0, C1, P1: TVec2);
var
  Bezier: TBezier2;
  Curve: TCurve2;
  SubPath: TSubPath;
begin
  Bezier := Bezier2(FPosition, C0, C1, P1);
  Curve := Bezier.Flatten;
  if Curve.Distance < Sigma then
    Exit;
  SubPath := Current;
  if SubPath = nil then
  begin
    SubPath := TSubPath.Create;
    FSubPaths.Add(SubPath);
  end;
  SubPath.FCurves.Add(Curve);
  FPosition := P1;
end;

function TPath.GetCurrent: TSubPath;
begin
  if FSubPaths.Count < 1 then
    Result := nil
  else
    Result := FSubPaths[FSubPaths.Count - 1];
end;

function TPath.GetSubPath(Index: Integer): TSubPath;
begin
  Result := FSubPaths[Index];
end;

function TPath.GetCount: Integer;
begin
  Result := FSubPaths.Count;
end;

{ TPen }

constructor TPen.Create(Color: TColorF; Width: Float = 1);
begin
  inherited Create;
  FColor := TVec4Prop.Create;
  FColor.Value := Color;
  FWidth := TVec1Prop.Create;
  FWidth.Value := Width;
end;

function TPen.AssignFrom(Source: TObject): Boolean;
var
  Pen: TPen;
begin
  if Source is TPen then
  begin
    Pen := Source as TPen;
    FColor.Value := Pen.FColor.Value;
    FWidth.Value := Pen.FWidth.Value;
    Result := True;
  end
  else
    Result := inherited AssignFrom(Source)
end;

function TPen.Clone: TPen;
begin
  Result := TPen.Create(FColor);
  Assign(Self, Result);
end;

procedure TPen.SetColor(const Value: TVec4Prop);
begin
  FColor.Value := Value.Value;
end;

procedure TPen.SetWidth(const Value: TVec1Prop);
begin
  FWidth.Value := Value.Value;
end;

{ TBrush }

constructor TBrush.Create(Color: TColorF);
begin
  inherited Create;
  FColor := TVec4Prop.Create;
  FColor.Value := Color;
end;

function TBrush.AssignFrom(Source: TObject): Boolean;
var
  Brush: TBrush;
begin
  if Source is TBrush then
  begin
    Brush := Source as TBrush;
    FColor.Value := Brush.FColor.Value;
    Result := True;
  end
  else
    Result := inherited AssignFrom(Source)
end;

function TBrush.Clone: TBrush;
begin
  Result := TBrush.Create(FColor);
  Assign(Self, Result);
end;

procedure TBrush.SetColor(const Value: TVec4Prop);
begin
  FColor.Value := Value.Value;
end;

{ TCanvas }
const
  TexStroke = 0;
  TexEndCap = TexStroke + 1;

constructor TCanvas.Create(World: TWorld);
const
  White: TColorB = (R: $FF; G: $FF; B: $FF; A: $FF);
  Size = 32;
  CA = 8;
  CB = 15;
  CC = CB - CA;
var
  B: IBitmap;
  P: PPixel;
  Mid: TVec2;
  Dist: Float;
  X, Y: Integer;
begin
  inherited Create;
  FWorld := World;
  FPath := TPath.Create;
  FTextures :=  TTextures.Create(TexEndCap + 1);
  B := CreateBitmap(Size, Size);
  Mid := Vec2(Size - 1, Size - 1) / 2;
  P := B.Pixels;
  for Y := 0 to Size - 1 do
    for X := 0 to Size - 1 do
    begin
      P^ := White;
      Dist := Mid.Distance(X, Y);
      if Dist > CB then
        P.A := 0
      else if Dist > CA then
        P.A := Round($FF * (1 - (Dist - CA) / CC));
      Inc(P);
    end;
  FTextures.Load(B, TexEndCap);
  FTextures.GenerateMipmaps(TexEndCap);
  P := B.Pixels;
  for Y := 0 to Size - 1 do
  begin
    for X := 0 to Size - 1 do
    begin
      P^ := White;
      Mid.X := X;
      Dist := Mid.Distance(X, Y);
      if Dist > CB then
        P.A := 0
      else if Dist > CA then
        P.A := Round($FF * (1 - (Dist - CA) / CC));
      Inc(P);
    end;
  end;
  FTextures.Load(B, TexStroke);
  FTextures.GenerateMipmaps(TexStroke);
end;

destructor TCanvas.Destroy;
begin
  FTextures.Free;
  FPath.Free;
  inherited Destroy;
end;

procedure TCanvas.Stroke(Pen: TPen; Clear: Boolean = True);

  procedure DrawLine(const A, B: TVec2; Width: Float);
  var
    N: TVec2;
  begin
    N := (B - A).Normal.Binormal * Width;
    FWorld.TexVertex(A + N, Vec2(0, 0));
    FWorld.TexVertex(A - N, Vec2(0, 1));
    FWorld.TexVertex(B - N, Vec2(1, 1));
    FWorld.TexVertex(B + N, Vec2(1, 0));
  end;

  procedure DrawCap(const A: TVec2; Width: Float);
  begin
    FWorld.TexVertex(Vec2(A.X - Width, A.Y - Width), Vec2(0, 0));
    FWorld.TexVertex(Vec2(A.X - Width, A.Y + Width), Vec2(0, 1));
    FWorld.TexVertex(Vec2(A.X + Width, A.Y + Width), Vec2(1, 1));
    FWorld.TexVertex(Vec2(A.X + Width, A.Y - Width), Vec2(1, 0));
  end;

var
  SubPath: TSubPath;
  Curve: TCurve2;
  Width: Float;
  I, J, K: Integer;
begin
  FWorld.Color(Pen.Color);
  Width := Pen.Width;
  glDisable(GL_DEPTH_TEST);
  glDepthMask(False);
  for I := 0 to FPath.FSubPaths.Count - 1 do
  begin
    FWorld.BindTex(FTextures[TexStroke]);
    FWorld.BeginQuads;
    SubPath := FPath.FSubPaths[I];
    for J := 0 to SubPath.FCurves.Count - 1 do
    begin
      Curve := SubPath.FCurves[J];
      if Length(Curve.P) < 2 then
        Continue;
      for K := 1 to Length(Curve.P) - 1 do
        DrawLine(Curve.P[K - 1], Curve.P[K], Width);
    end;
    FWorld.EndQuads;
    FWorld.UnbindTex;
    FWorld.BindTex(FTextures[TexEndCap]);
    FWorld.BeginQuads;
    for J := 0 to SubPath.FCurves.Count - 1 do
    begin
      Curve := SubPath.FCurves[J];
      if Length(Curve.P) < 2 then
        Continue;
      for K := 0 to Length(Curve.P) - 1 do
        DrawCap(Curve.P[K], Width * 0.8);
    end;
    FWorld.EndQuads;
    FWorld.UnbindTex;
  end;
  glDepthMask(True);
  glEnable(GL_DEPTH_TEST);
  if Clear then
    FPath.Clear;
end;

procedure TCanvas.Fill(Brush: TBrush; Clear: Boolean = True);
begin

end;

end.


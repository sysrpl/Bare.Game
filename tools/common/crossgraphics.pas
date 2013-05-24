unit CrossGraphics;

{$i cross.inc}

interface

uses
  SysUtils, Classes, Graphics;

function Darken(Color: TColor; Percent: Single): TColor;
function Mix(A, B: TColor; Percent: Single): TColor;
procedure InflateRect(var Rect: TRect; X, Y: Integer);
procedure OffsetRect(var Rect: TRect; X, Y: Integer);
procedure DrawEmpty(Canvas: TCanvas; Rect: TRect; Color: TColor);
procedure DrawShadow(Canvas: TCanvas; Rect: TRect; Color: TColor);
function CreateShadowBitmap(Canvas: TCanvas; Rect: TRect; Color: TColor): TBitmap;

implementation  

type
  TRGBA = record R, G, B, A: Byte; end;

function Darken(Color: TColor; Percent: Single): TColor;
var
  Pixel: TRGBA absolute Color;
begin
  Color := ColorToRGB(Color);
  Pixel.R := Round(Pixel.R * Percent) and $FF;
  Pixel.G := Round(Pixel.G * Percent) and $FF;
  Pixel.B := Round(Pixel.B * Percent) and $FF;
  Result := Color;
end;

function Mix(A, B: TColor; Percent: Single): TColor;
var
  PixA: TRGBA absolute A;
  PixB: TRGBA absolute B;
  PixC: TRGBA absolute Result;
  Comp: Single;
begin
  A := ColorToRGB(A);
  B := ColorToRGB(B);
  Result := clBlack;
  Comp := 1 - Percent;
  PixC.R := Round(PixA.R * Percent + PixB.R * Comp) and $FF;
  PixC.G := Round(PixA.G * Percent + PixB.G * Comp) and $FF;
  PixC.B := Round(PixA.B * Percent + PixB.B * Comp) and $FF;
end;

procedure InflateRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left - X;
  Rect.Top := Rect.Top - Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure OffsetRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure DrawEmpty(Canvas: TCanvas; Rect: TRect; Color: TColor);
var
  A, B: TColor;
begin
  A := Canvas.Brush.Color;
  B := ColorToRGB(Color);
  Canvas.Brush.Color := B;
  Canvas.FillRect(Rect);
  Canvas.Brush.Color := A;
end;

procedure DrawShadow(Canvas: TCanvas; Rect: TRect; Color: TColor);
const
  ShadowHeight = 15;
var
  R: TRect;
  A, B, C: TColor;
  I: Integer;
begin
  A := Canvas.Brush.Color;
  B := ColorToRGB(Color);
  R := Rect;
  R.Bottom := R.Top + 1;
  C := Darken(B, 0.925);
  for I := 0 to ShadowHeight do
  begin
    Canvas.Brush.Color := Mix(B, C, I / ShadowHeight);
    OffsetRect(R, 0, 1);
    Canvas.FillRect(R);
  end;
  Canvas.Brush.Color := A;
end;

function CreateShadowBitmap(Canvas: TCanvas; Rect: TRect; Color: TColor): TBitmap;
const
  ShadowHeight = 20;
var
  A, B: TColor;
  I: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := Rect.Right - Rect.Left;
  Result.Height := Rect.Bottom - Rect.Top;
  A := ColorToRGB(Color);
  B := Darken(A, 0.9);
  Canvas := Result.Canvas;
  Canvas.Brush.Color := A;
  Canvas.Pen.Style := psClear;
  OffsetRect(Rect, -Rect.Left, -Rect.Top);
  Canvas.FillRect(Rect);
  OffsetRect(Rect, 0, Rect.Bottom div -2);
  for I := 0 to ShadowHeight do
  begin
    Canvas.Brush.Color := Mix(B, A, I / ShadowHeight);
    Canvas.Ellipse(Rect);
    InflateRect(Rect, -8, -1);
  end;
end;    

end.

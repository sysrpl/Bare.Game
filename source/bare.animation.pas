(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.animation.txt> }
unit Bare.Animation;

{$i bare.inc}

interface

{TODO: Convert animation and storyboard engine from old code base}

uses
  Bare.System,
  Bare.Types,
  Bare.Geometry;

type
  TEasing = function(Percent: Float): Float;

  TEasingDefaults = record
  public
    class function Linear(Percent: Float): Float; static;
    class function Easy(Percent: Float): Float; static;
    class function EasySlow(Percent: Float): Float; static;
    class function Extend(Percent: Float): Float; static;
    class function Drop(Percent: Float): Float; static;
    class function DropSlow(Percent: Float): Float; static;
    class function Snap(Percent: Float): Float; static;
    class function Bounce(Percent: Float): Float; static;
    class function Bouncy(Percent: Float): Float; static;
    class function Rubber(Percent: Float): Float; static;
    class function Spring(Percent: Float): Float; static;
    class function Boing(Percent: Float): Float; static;
  end;

  TEasings = class(TDictionary<string, TEasing>)
  public
    procedure RegisterDefaults;
  end;

  TEasingItem = TEasings.TKeyValue;

function Easings: TEasings;
function Interpolate(Easing: TEasing; Percent: Float; Reverse: Boolean = False): Float;

{ Dependency property related types and routines }

type
  IDependencyProperty = interface
  ['{E021AD95-9985-48AB-B29F-8D25A7BBE10E}']
    function GetCount: Integer;
    function GetValue(Index: Integer): Float;
    procedure SetValue(Value: Float; Index: Integer);
    property Count: Integer read GetCount;
  end;

  TDependencyChangeNotify = procedure(Prop: IDependencyProperty; Index: Integer) of object;

{ Properties }

  TVec1Prop = record
  private
    function GetValue: TVec1;
    procedure SetValue(Value: TVec1);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetLength: Integer;
    function GetProp: IDependencyProperty; inline;
    function GetIndex: LongInt;
  public
    class operator Implicit(const Value: TVec1): TVec1Prop;
    class operator Implicit(const Value: TVec1Prop): TVec1;
    class operator Negative(const A: TVec1Prop): TVec1;
    class operator Positive(const A: TVec1Prop): TVec1;
    class operator Equal(const A, B: TVec1Prop) : Boolean;
    class operator NotEqual(const A, B: TVec1Prop): Boolean;
    class operator GreaterThan(const A, B: TVec1Prop): Boolean;
    class operator GreaterThanOrEqual(const A, B: TVec1Prop): Boolean;
    class operator LessThan(const A, B: TVec1Prop): Boolean;
    class operator LessThanOrEqual(const A, B: TVec1Prop): Boolean;
    class operator Add(const A, B: TVec1Prop): TVec1;
    class operator Subtract(const A, B: TVec1Prop): TVec1;
    class operator Multiply(const A, B: TVec1Prop): TVec1;
    class operator Divide(const A, B: TVec1Prop): TVec1;
    class function Create(OnChange: TDependencyChangeNotify = nil): TVec1Prop; overload; static;
    class function Create(Prop: IDependencyProperty; Index: LongInt): TVec1Prop; static;
    function Equals(const A: TVec1Prop): Boolean;
    function Same(const A: TVec1Prop): Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Value: TVec1 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
    property Length: LongInt read GetLength;
    property Prop: IDependencyProperty read GetProp;
    property Index: LongInt read GetIndex;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec1);
  end;

  TVec2Prop = record
  private
    function GetValue: TVec2;
    procedure SetValue(const Value: TVec2);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetLength: Integer;
    function GetProp: IDependencyProperty; inline;
    function GetIndex: LongInt;
  public
    class operator Implicit(const Value: TVec2Prop): TVec2;
    class operator Implicit(const Value: TVec2): TVec2Prop;
    class operator Implicit(const Value: TPoint): TVec2Prop;
    class operator Explicit(const Value: TVec2Prop): TPoint;
    class operator Implicit(const Value: TPointI): TVec2Prop;
    class operator Explicit(const Value: TVec2Prop): TPointI;
    class operator Implicit(const Value: TPointF): TVec2Prop;
    class operator Implicit(const Value: TVec2Prop): TPointF;
    class function Create(OnChange: TDependencyChangeNotify = nil): TVec2Prop; overload; static;
    class function Create(Prop: IDependencyProperty; Index: LongInt): TVec2Prop; overload; static;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property Value: TVec2 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
    property Length: LongInt read GetLength;
    property Prop: IDependencyProperty read GetProp;
    property Index: LongInt read GetIndex;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec2);
  end;

  TVec3Prop = record
  private
    function GetValue: TVec3;
    procedure SetValue(const Value: TVec3);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetAsVec2: TVec2Prop;
    procedure SetAsVec2(const Value: TVec2Prop);
    function GetLength: Integer;
    function GetProp: IDependencyProperty; inline;
    function GetIndex: LongInt;
  public
    class operator Implicit(const Value: TVec3Prop): TVec3;
    class operator Implicit(const Value: TVec3): TVec3Prop;
    class function Create(OnChange: TDependencyChangeNotify = nil): TVec3Prop; overload; static;
    class function Create(Prop: IDependencyProperty; Index: LongInt): TVec3Prop; overload; static;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property Z: TVec1Prop index 2 read GetVec write SetVec;
    property Heading: TVec1Prop index 0 read GetVec write SetVec;
    property Pitch: TVec1Prop index 1 read GetVec write SetVec;
    property Roll: TVec1Prop index 2 read GetVec write SetVec;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property AsVec2: TVec2Prop read GetAsVec2 write SetAsVec2;
    property Value: TVec3 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
    property Length: LongInt read GetLength;
    property Prop: IDependencyProperty read GetProp;
    property Index: LongInt read GetIndex;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec3);
  end;

  TVec4Prop = record
  private
    function GetValue: TVec4;
    procedure SetValue(const Value: TVec4);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetAsVec2: TVec2Prop;
    procedure SetAsVec2(const Value: TVec2Prop);
    function GetAsVec3: TVec3Prop;
    procedure SetAsVec3(const Value: TVec3Prop);
    function GetLength: Integer;
    function GetProp: IDependencyProperty; inline;
    function GetIndex: LongInt;
  public
    class operator Implicit(const Value: TVec4): TVec4Prop;
    class operator Implicit(const Value: TVec4Prop): TVec4;
    class operator Implicit(Value: TColorB): TVec4Prop;
    class operator Explicit(const Value: TVec4Prop): TColorB;
    class operator Implicit(const Value: TColorF): TVec4Prop;
    class operator Implicit(const Value: TVec4Prop): TColorF;
    class function Create(OnChange: TDependencyChangeNotify = nil): TVec4Prop; overload; static;
    class function Create(Prop: IDependencyProperty; Index: LongInt): TVec4Prop; overload; static;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property Z: TVec1Prop index 2 read GetVec write SetVec;
    property W: TVec1Prop index 3 read GetVec write SetVec;
    property Blue: TVec1Prop index 0 read GetVec write SetVec;
    property Green: TVec1Prop index 1 read GetVec write SetVec;
    property Red: TVec1Prop index 2 read GetVec write SetVec;
    property Alpha: TVec1Prop index 3 read GetVec write SetVec;
    property S0: TVec1Prop index 0 read GetVec write SetVec;
    property T0: TVec1Prop index 1 read GetVec write SetVec;
    property S1: TVec1Prop index 2 read GetVec write SetVec;
    property T1: TVec1Prop index 3 read GetVec write SetVec;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property AsVec2: TVec2Prop read GetAsVec2 write SetAsVec2;
    property AsVec3: TVec3Prop read GetAsVec3 write SetAsVec3;
    property Value: TVec4 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
    property Length: LongInt read GetLength;
    property Prop: IDependencyProperty read GetProp;
    property Index: LongInt read GetIndex;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec4);
  end;

procedure DependencyLink(var Prop: IDependencyProperty; Count: Integer; OnChange: TDependencyChangeNotify);
procedure DependencyUnlink(var Prop: IDependencyProperty);

implementation

const
  NegCosPi = 1.61803398874989; { 2 / -Cos(Pi * 1.2) }

class function TEasingDefaults.Linear(Percent: Float): Float;
begin
  Result := Percent;
end;

class function TEasingDefaults.Easy(Percent: Float): Float;
begin
  Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.EasySlow(Percent: Float): Float;
begin
  Percent := Easy(Percent);
  Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.Extend(Percent: Float): Float;
begin
  Percent := (Percent * 1.4) - 0.2;
  Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function Power(const Base, Exponent: Float): Float;
begin
  if Exponent = 0 then
    Result := 1
  else if (Base = 0) and (Exponent > 0) then
    Result := 0
  else
    Result := Exp(Exponent * Ln(Base));
end;

class function TEasingDefaults.Drop(Percent: Float): Float;
begin
  Result := Percent * Percent;
end;

class function TEasingDefaults.DropSlow(Percent: Float): Float;
begin
  Result := Percent * Percent * Percent;
end;

class function TEasingDefaults.Snap(Percent: Float): Float;
begin
  Percent := Percent * Percent;
  Percent := (Percent * 1.4) - 0.2;
  Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function TEasingDefaults.Bounce(Percent: Float): Float;
begin
  if Percent > 0.9 then
  begin
    Result := Percent - 0.95;
    Result := 1 + Result * Result * 20 - (0.05 * 0.05 * 20);
  end
  else if Percent > 0.75 then
  begin
    Result := Percent - 0.825;
    Result := 1 + Result * Result * 16 - (0.075 * 0.075 * 16);
  end
  else if Percent > 0.5 then
  begin
    Result := Percent - 0.625;
    Result := 1 + Result * Result * 12 - (0.125 * 0.125 * 12);
  end
  else
  begin
    Percent := Percent * 2;
    Result := Percent * Percent;
  end;
end;

class function TEasingDefaults.Bouncy(Percent: Float): Float;
var
  Scale, Start, Step: Float;
begin
  Result := 1;
  Scale := 5;
  Start := 0.5;
  Step := 0.2;
  if Percent < Start then
  begin
    Result := Percent / Start;
    Result :=  Result * Result;
  end
  else
  while Step > 0.01 do
    if Percent < Start + Step then
    begin
      Step := Step / 2;
      Result := (Percent - (Start + Step)) * Scale;
      Result :=  Result * Result;
      Result := Result + 1 - Power(Step * Scale, 2);
      Break;
    end
    else
    begin
      Start := Start + Step;
      Step := Step * 0.6;
    end;
end;

class function TEasingDefaults.Rubber(Percent: Float): Float;
begin
  if Percent > 0.9 then
  begin
    Result := Percent - 0.95;
    Result := 1 - Result * Result * 20 + (0.05 * 0.05 * 20);
  end
  else if Percent > 0.75 then
  begin
    Result := Percent - 0.825;
    Result := 1 + Result * Result * 18 - (0.075 * 0.075 * 18);
  end
  else if Percent > 0.5 then
  begin
    Result := Percent - 0.625;
    Result := 1 - Result * Result * 14 + (0.125 * 0.125 * 14);
  end
  else
  begin
    Percent := Percent * 2;
    Result := Percent * Percent;
  end;
end;

class function TEasingDefaults.Spring(Percent: Float): Float;
begin
  Percent := Percent * Percent;
  Result := Sin(PI * Percent * Percent * 10 - PI / 2) / 4;
  Result := Result * (1 - Percent) + 1;
  if Percent < 0.3 then
    Result := Result * Easy(Percent / 0.3);
end;

class function TEasingDefaults.Boing(Percent: Float): Float;
begin
  Percent := Power(Percent, 1.5);
  Result := Sin(PI * Power(Percent, 2) * 20 - PI / 2) / 4;
  Result := Result * (1 - Percent) + 1;
  if Percent < 0.2 then
    Result := Result * Easy(Percent / 0.2);
end;

procedure TEasings.RegisterDefaults;
begin
  Self['Linear'] := @TEasingDefaults.Linear;
  Self['Easy'] := @TEasingDefaults.Easy;
  Self['Easy Slow'] := @TEasingDefaults.EasySlow;
  Self['Extend'] := @TEasingDefaults.Extend;
  Self['Drop'] := @TEasingDefaults.Drop;
  Self['Drop Slow'] := @TEasingDefaults.DropSlow;
  Self['Snap'] := @TEasingDefaults.Snap;
  Self['Bounce'] := @TEasingDefaults.Bounce;
  Self['Bouncy'] := @TEasingDefaults.Bouncy;
  Self['Rubber'] := @TEasingDefaults.Rubber;
  Self['Spring'] := @TEasingDefaults.Spring;
  Self['Boing'] := @TEasingDefaults.Boing;
end;

var
  EasingsInstance: TObject;

function EasingKeyCompare(const A, B: AnsiString): Integer;
begin
  Result := StrCompare(A, B, True);
end;

function Easings: TEasings;
begin
  if EasingsInstance = nil then
  begin
    EasingsInstance := TEasings.Create;
    TEasings(EasingsInstance).Comparer := EasingKeyCompare;
  end;
  Result := TEasings(EasingsInstance);
end;

function Interpolate(Easing: TEasing; Percent: Float; Reverse: Boolean = False): Float;
begin
	if Percent < 0 then
  	if Reverse then
	  	Exit(1)
		else
	  	Exit(0);
  if Percent > 1 then
	  if Reverse then
  	  Exit(0)
	  else
  	  Exit(1);
  if Reverse then
    Result := 1 - Easing(1 - Percent)
  else
    Result := Easing(Percent);
end;

{ TVec1Prop }

class operator TVec1Prop.Implicit(const Value: TVec1Prop): TVec1;
begin
  Result := Value.Value;
end;

class operator TVec1Prop.Implicit(const Value: TVec1): TVec1Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FIndex := 0;
  Result.FValue := Value;
end;

class operator TVec1Prop.Negative(const A: TVec1Prop): TVec1;
begin
  Result := -A.Value;
end;

class operator TVec1Prop.Positive(const A: TVec1Prop): TVec1;
begin
  Result := A.Value;
end;

class operator TVec1Prop.Equal(const A, B: TVec1Prop) : Boolean;
begin
  Result := A.Value = B.Value;
end;

class operator TVec1Prop.NotEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value <> B.Value;
end;

class operator TVec1Prop.GreaterThan(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value > B.Value;
end;

class operator TVec1Prop.GreaterThanOrEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value >= B.Value;
end;

class operator TVec1Prop.LessThan(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value < B.Value;
end;

class operator TVec1Prop.LessThanOrEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value <= B.Value;
end;

class operator TVec1Prop.Add(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value + B.Value;
end;

class operator TVec1Prop.Subtract(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value - B.Value;
end;

class operator TVec1Prop.Multiply(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value * B.Value;
end;

class operator TVec1Prop.Divide(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value / B.Value;
end;

class function TVec1Prop.Create(OnChange: TDependencyChangeNotify = nil): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  DependencyLink(V.FProp, 1, OnChange);
  V.FIndex := 0;
  Exit(V);
end;

class function TVec1Prop.Create(Prop: IDependencyProperty; Index: LongInt): TVec1Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FProp := Prop;
  Result.FIndex := Index;
end;

function TVec1Prop.Equals(const A: TVec1Prop): Boolean;
begin
  Result := Value = A.Value;
end;

function TVec1Prop.Same(const A: TVec1Prop): Boolean;
begin
  if FProp = nil then
    Result := False
  else if FProp = A.FProp then
    Result := FIndex = A.FIndex
  else
    Result := False;
end;

function TVec1Prop.GetValue: TVec1;
begin
  if FProp = nil then
    Result := FValue
  else
    Result := FProp.GetValue(FIndex);
end;

procedure TVec1Prop.SetValue(Value: TVec1);
begin
  if FProp = nil then
    FValue := Value
  else
    FProp.SetValue(Value, FIndex);
end;

function TVec1Prop.GetVec(Index: Integer): TVec1Prop;
begin
  Exit(Self);
end;

procedure TVec1Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if not Same(Value) then
    SetValue(Value.Value);
end;

function TVec1Prop.GetLength: Integer;
begin
  Result := 1;
end;

function TVec1Prop.GetProp: IDependencyProperty;
begin
  Result := FProp;
end;

function TVec1Prop.GetIndex: LongInt;
begin
  if FProp = nil then
    Result := FIndex
  else
    Result := -1;
end;

{ TVec2Prop }

class operator TVec2Prop.Implicit(const Value: TVec2Prop): TVec2;
begin
  Result := Value.Value;
end;

class operator TVec2Prop.Implicit(const Value: TVec2): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FIndex := 0;
  Result.FValue := Value;
end;

class operator TVec2Prop.Implicit(const Value: TPoint): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue.X := Value.X;
  Result.FValue.Y := Value.Y;
end;

class operator TVec2Prop.Explicit(const Value: TVec2Prop): TPoint;
var
  V: TVec2;
begin
  V := Value.Value;
  Result.X := Round(V.X);
  Result.Y := Round(V.Y);
end;

class operator TVec2Prop.Implicit(const Value: TPointI): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue.X := Value.X;
  Result.FValue.Y := Value.Y;
end;

class operator TVec2Prop.Explicit(const Value: TVec2Prop): TPointI;
var
  V: TVec2;
begin
  V := Value.Value;
  Result.X := Round(V.X);
  Result.Y := Round(V.Y);
end;

class operator TVec2Prop.Implicit(const Value: TPointF): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue.X := Value.X;
  Result.FValue.Y := Value.Y;
end;

class operator TVec2Prop.Implicit(const Value: TVec2Prop): TPointF;
var
  V: TVec2;
begin
  V := Value.Value;
  Result.X := V.X;
  Result.Y := V.Y;
end;

class function TVec2Prop.Create(OnChange: TDependencyChangeNotify = nil): TVec2Prop; overload; static;
var
  V: TVec2Prop;
begin
  UIntPtr(V.FProp) := 0;
  DependencyLink(V.FProp, 2, OnChange);
  V.FIndex := 0;
  Exit(V);
end;

class function TVec2Prop.Create(Prop: IDependencyProperty; Index: LongInt): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FProp := Prop;
  Result.FIndex := Index;
end;

function TVec2Prop.GetValue: TVec2;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
  end;
end;

procedure TVec2Prop.SetValue(const Value: TVec2);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
  end;
end;

function TVec2Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
      V.FValue := FValue.X
    else
      V.FValue := FValue.Y;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
      V.FIndex := FIndex
    else
      V.FIndex := FIndex + 1;
  end;
  Exit(V);
end;

procedure TVec2Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
      FValue.X := Value.Value
    else
      FValue.Y := Value.Value;
  end
  else
  begin
    if Index < 1 then
      FProp.SetValue(Value.Value, FIndex)
    else
      FProp.SetValue(Value.Value, FIndex + 1);
  end;
end;

function TVec2Prop.GetLength: Integer;
begin
  Result := 2;
end;

function TVec2Prop.GetProp: IDependencyProperty;
begin
  Result := FProp;
end;

function TVec2Prop.GetIndex: LongInt;
begin
  if FProp = nil then
    Result := FIndex
  else
    Result := -1;
end;

{ TVec3Prop }

class operator TVec3Prop.Implicit(const Value: TVec3): TVec3Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FIndex := 0;
  Result.FValue := Value;
end;

class operator TVec3Prop.Implicit(const Value: TVec3Prop): TVec3;
begin
  Result := Value.Value;
end;

class function TVec3Prop.Create(OnChange: TDependencyChangeNotify = nil): TVec3Prop;
var
  V: TVec3Prop;
begin
  UIntPtr(V.FProp) := 0;
  DependencyLink(V.FProp, 3, OnChange);
  V.FIndex := 0;
  Exit(V);
end;

class function TVec3Prop.Create(Prop: IDependencyProperty; Index: LongInt): TVec3Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FProp := Prop;
  Result.FIndex := Index;
end;

function TVec3Prop.GetValue: TVec3;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
    Result.Z := FProp.GetValue(FIndex + 2);
  end;
end;

procedure TVec3Prop.SetValue(const Value: TVec3);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
  end;
end;

function TVec3Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
      V.FValue := FValue.X
    else if Index < 2 then
      V.FValue := FValue.Y
    else
      V.FValue := FValue.Z;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
      V.FIndex := FIndex
    else if Index < 2 then
      V.FIndex := FIndex + 1
    else
      V.FIndex := FIndex + 2;
  end;
  Exit(V);
end;

procedure TVec3Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
      FValue.X := Value.Value
    else if Index < 2 then
      FValue.Y := Value.Value
    else
      FValue.Z := Value.Value;
  end
  else
  begin
    if Index < 1 then
      FProp.SetValue(Value.Value, FIndex)
    else if Index < 2 then
      FProp.SetValue(Value.Value, FIndex + 1)
    else
      FProp.SetValue(Value.Value, FIndex + 2);
  end;
end;

function TVec3Prop.GetAsVec2: TVec2Prop;
begin
  Result := TVec2Prop.Create(FProp, 0);
end;

procedure TVec3Prop.SetAsVec2(const Value: TVec2Prop);
var
  V: TVec2;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
end;

function TVec3Prop.GetLength: Integer;
begin
  Result := 3;
end;

function TVec3Prop.GetProp: IDependencyProperty;
begin
  Result := FProp;
end;

function TVec3Prop.GetIndex: LongInt;
begin
  if FProp = nil then
    Result := FIndex
  else
    Result := -1;
end;

{ TVec4Prop }

class operator TVec4Prop.Implicit(const Value: TVec4): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FIndex := 0;
  Result.FValue := Value;
end;

class operator TVec4Prop.Implicit(const Value: TVec4Prop): TVec4;
begin
  Result := Value.Value;
end;

class operator TVec4Prop.Implicit(Value: TColorB): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec4Prop.Explicit(const Value: TVec4Prop): TColorB;
begin
  Result := TColorB(Value.Value);
end;

class operator TVec4Prop.Implicit(const Value: TColorF): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := TVec4(Value);
end;

class operator TVec4Prop.Implicit(const Value: TVec4Prop): TColorF;
begin
  Result := TColorF(Value.Value);
end;

class function TVec4Prop.Create(OnChange: TDependencyChangeNotify = nil): TVec4Prop;
var
  V: TVec4Prop;
begin
  UIntPtr(V.FProp) := 0;
  DependencyLink(V.FProp, 4, OnChange);
  V.FIndex := 0;
  Exit(V);
end;

class function TVec4Prop.Create(Prop: IDependencyProperty; Index: LongInt): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FProp := Prop;
  Result.FIndex := Index;
end;

function TVec4Prop.GetValue: TVec4;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
    Result.Z := FProp.GetValue(FIndex + 2);
    Result.W := FProp.GetValue(FIndex + 3);
  end;
end;

procedure TVec4Prop.SetValue(const Value: TVec4);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
    FProp.SetValue(Value.Z, FIndex + 2);
    FProp.SetValue(Value.W, FIndex + 3);
  end;
end;

function TVec4Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
      V.FValue := FValue.X
    else if Index < 2 then
      V.FValue := FValue.Y
    else if Index < 3 then
      V.FValue := FValue.Z
    else
      V.FValue := FValue.W;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
      V.FIndex := FIndex
    else if Index < 2 then
      V.FIndex := FIndex + 1
    else if Index < 3 then
      V.FIndex := FIndex + 2
    else
      V.FIndex := FIndex + 3;
  end;
  Exit(V);
end;

procedure TVec4Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
      FValue.X := Value.Value
    else if Index < 2 then
      FValue.Y := Value.Value
    else if Index < 3 then
      FValue.Z := Value.Value
    else
      FValue.W := Value.Value;
  end
  else
  begin
    if Index < 1 then
      FProp.SetValue(Value.Value, FIndex)
    else if Index < 2 then
      FProp.SetValue(Value.Value, FIndex + 1)
    else if Index < 3 then
      FProp.SetValue(Value.Value, FIndex + 2)
    else
      FProp.SetValue(Value.Value, FIndex + 3);
  end;
end;

function TVec4Prop.GetAsVec2: TVec2Prop;
begin
  Result := TVec2Prop.Create(FProp, 0);
end;

procedure TVec4Prop.SetAsVec2(const Value: TVec2Prop);
var
  V: TVec2;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
end;

function TVec4Prop.GetAsVec3: TVec3Prop;
begin
  Result := TVec3Prop.Create(FProp, 0);
end;

procedure TVec4Prop.SetAsVec3(const Value: TVec3Prop);
var
  V: TVec3;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
  Z := V.Z;
end;

function TVec4Prop.GetLength: Integer;
begin
  Result := 4;
end;

function TVec4Prop.GetProp: IDependencyProperty;
begin
  Result := FProp;
end;

function TVec4Prop.GetIndex: LongInt;
begin
  if FProp = nil then
    Result := FIndex
  else
    Result := -1;
end;

{ TDependencyProperty }

type
  TPropertyValues = array of Float;

  TDependencyProperty = class(TInterfacedObject, IDependencyProperty)
  private
    FValues: TPropertyValues;
    FOnChange: TDependencyChangeNotify;
  public
    function GetCount: Integer;
    function GetValue(Index: Integer): Float;
    procedure SetValue(Value: Float; Index: Integer);
  end;

function TDependencyProperty.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TDependencyProperty.GetValue(Index: Integer): Float;
begin
  Result := FValues[Index];
end;

procedure TDependencyProperty.SetValue(Value: Float; Index: Integer);
begin
  if FValues[Index] <> Value then
  begin
    FValues[Index] := Value;
    if Assigned(FOnChange) then
      FOnChange(Self, Index);
  end;
end;

procedure DependencyLink(var Prop: IDependencyProperty; Count: Integer; OnChange: TDependencyChangeNotify);
var
  Dependency: TDependencyProperty;
begin
  if Prop = nil then
    Dependency := TDependencyProperty.Create
  else
    Dependency := Prop as TDependencyProperty;
  SetLength(Dependency.FValues, Count);
  Dependency.FOnChange := OnChange;
  Prop := Dependency;
end;

procedure DependencyUnlink(var Prop: IDependencyProperty);
var
  Dependency: TDependencyProperty;
begin
  if Prop = nil then
    Exit;
  Dependency := Prop as TDependencyProperty;
  Dependency.FOnChange := nil;
  Prop := nil;
end;

finalization
  EasingsInstance.Free;
end.


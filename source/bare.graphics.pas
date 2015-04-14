(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.graphics.txt> }
unit Bare.Graphics;

{$i bare.inc}

interface

{TODO: Write 2d vector drawing system}
{TODO: Write sprite class possibly in another namespace}
{TODO: Include per pixel collision detection in sprite class}

uses
  Bare.System,
  Bare.Types,
  Bare.Animation,
  Bare.Geometry,
  Bare.Graphics.Imaging,
  Bare.Text,
  Bare.Interop.OpenGL;

{doc off}
const
  clAliceBlue: TColorF = (R: $F0 / $FF; G: $F8 / $FF; B: $FF / $FF; A: 1.0);
  clAntiqueWhite: TColorF = (R: $FA / $FF; G: $EB / $FF; B: $D7 / $FF; A: 1.0);
  clAqua: TColorF = (R: $00 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  clAquamarine: TColorF = (R: $7F / $FF; G: $FF / $FF; B: $D4 / $FF; A: 1.0);
  clAzure: TColorF = (R: $F0 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  clBeige: TColorF = (R: $F5 / $FF; G: $F5 / $FF; B: $DC / $FF; A: 1.0);
  clBisque: TColorF = (R: $FF / $FF; G: $E4 / $FF; B: $C4 / $FF; A: 1.0);
  clBlack: TColorF = (R: $00 / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  clBlanchedAlmond: TColorF = (R: $FF / $FF; G: $EB / $FF; B: $CD / $FF; A: 1.0);
  clBlue: TColorF = (R: $00 / $FF; G: $00 / $FF; B: $FF / $FF; A: 1.0);
  clBlueViolet: TColorF = (R: $8A / $FF; G: $2B / $FF; B: $E2 / $FF; A: 1.0);
  clBrown: TColorF = (R: $A5 / $FF; G: $2A / $FF; B: $2A / $FF; A: 1.0);
  clBurlyWood: TColorF = (R: $DE / $FF; G: $B8 / $FF; B: $87 / $FF; A: 1.0);
  clCadetBlue: TColorF = (R: $5F / $FF; G: $9E / $FF; B: $A0 / $FF; A: 1.0);
  clChartreuse: TColorF = (R: $7F / $FF; G: $FF / $FF; B: $00 / $FF; A: 1.0);
  clChocolate: TColorF = (R: $D2 / $FF; G: $69 / $FF; B: $1E / $FF; A: 1.0);
  clCoral: TColorF = (R: $FF / $FF; G: $7F / $FF; B: $50 / $FF; A: 1.0);
  clCornflowerBlue: TColorF = (R: $64 / $FF; G: $95 / $FF; B: $ED / $FF; A: 1.0);
  clCornsilk: TColorF = (R: $FF / $FF; G: $F8 / $FF; B: $DC / $FF; A: 1.0);
  clCrimson: TColorF = (R: $DC / $FF; G: $14 / $FF; B: $3C / $FF; A: 1.0);
  clCyan: TColorF = (R: $00 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  clDarkBlue: TColorF = (R: $00 / $FF; G: $00 / $FF; B: $8B / $FF; A: 1.0);
  clDarkCyan: TColorF = (R: $00 / $FF; G: $8B / $FF; B: $8B / $FF; A: 1.0);
  clDarkGoldenrod: TColorF = (R: $B8 / $FF; G: $86 / $FF; B: $0B / $FF; A: 1.0);
  clDarkGray: TColorF = (R: $A9 / $FF; G: $A9 / $FF; B: $A9 / $FF; A: 1.0);
  clDarkGreen: TColorF = (R: $00 / $FF; G: $64 / $FF; B: $00 / $FF; A: 1.0);
  clDarkKhaki: TColorF = (R: $BD / $FF; G: $B7 / $FF; B: $6B / $FF; A: 1.0);
  clDarkMagenta: TColorF = (R: $8B / $FF; G: $00 / $FF; B: $8B / $FF; A: 1.0);
  clDarkOliveGreen: TColorF = (R: $55 / $FF; G: $6B / $FF; B: $2F / $FF; A: 1.0);
  clDarkOrange: TColorF = (R: $FF / $FF; G: $8C / $FF; B: $00 / $FF; A: 1.0);
  clDarkOrchid: TColorF = (R: $99 / $FF; G: $32 / $FF; B: $CC / $FF; A: 1.0);
  clDarkRed: TColorF = (R: $8B / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  clDarkSalmon: TColorF = (R: $E9 / $FF; G: $96 / $FF; B: $7A / $FF; A: 1.0);
  clDarkSeaGreen: TColorF = (R: $8F / $FF; G: $BC / $FF; B: $8B / $FF; A: 1.0);
  clDarkSlateBlue: TColorF = (R: $48 / $FF; G: $3D / $FF; B: $8B / $FF; A: 1.0);
  clDarkSlateGray: TColorF = (R: $2F / $FF; G: $4F / $FF; B: $4F / $FF; A: 1.0);
  clDarkTurquoise: TColorF = (R: $00 / $FF; G: $CE / $FF; B: $D1 / $FF; A: 1.0);
  clDarkViolet: TColorF = (R: $94 / $FF; G: $00 / $FF; B: $D3 / $FF; A: 1.0);
  clDeepPink: TColorF = (R: $FF / $FF; G: $14 / $FF; B: $93 / $FF; A: 1.0);
  clDeepSkyBlue: TColorF = (R: $00 / $FF; G: $BF / $FF; B: $FF / $FF; A: 1.0);
  clDimGray: TColorF = (R: $69 / $FF; G: $69 / $FF; B: $69 / $FF; A: 1.0);
  clDodgerBlue: TColorF = (R: $1E / $FF; G: $90 / $FF; B: $FF / $FF; A: 1.0);
  clFirebrick: TColorF = (R: $B2 / $FF; G: $22 / $FF; B: $22 / $FF; A: 1.0);
  clFloralWhite: TColorF = (R: $FF / $FF; G: $FA / $FF; B: $F0 / $FF; A: 1.0);
  clForestGreen: TColorF = (R: $22 / $FF; G: $8B / $FF; B: $22 / $FF; A: 1.0);
  clFuchsia: TColorF = (R: $FF / $FF; G: $00 / $FF; B: $FF / $FF; A: 1.0);
  clGainsboro: TColorF = (R: $DC / $FF; G: $DC / $FF; B: $DC / $FF; A: 1.0);
  clGhostWhite: TColorF = (R: $F8 / $FF; G: $F8 / $FF; B: $FF / $FF; A: 1.0);
  clGold: TColorF = (R: $FF / $FF; G: $D7 / $FF; B: $00 / $FF; A: 1.0);
  clGoldenrod: TColorF = (R: $DA / $FF; G: $A5 / $FF; B: $20 / $FF; A: 1.0);
  clGray: TColorF = (R: $80 / $FF; G: $80 / $FF; B: $80 / $FF; A: 1.0);
  clGreen: TColorF = (R: $00 / $FF; G: $80 / $FF; B: $00 / $FF; A: 1.0);
  clGreenYellow: TColorF = (R: $AD / $FF; G: $FF / $FF; B: $2F / $FF; A: 1.0);
  clHoneydew: TColorF = (R: $F0 / $FF; G: $FF / $FF; B: $F0 / $FF; A: 1.0);
  clHotPink: TColorF = (R: $FF / $FF; G: $69 / $FF; B: $B4 / $FF; A: 1.0);
  clIndianRed: TColorF = (R: $CD / $FF; G: $5C / $FF; B: $5C / $FF; A: 1.0);
  clIndigo: TColorF = (R: $4B / $FF; G: $00 / $FF; B: $82 / $FF; A: 1.0);
  clIvory: TColorF = (R: $FF / $FF; G: $FF / $FF; B: $F0 / $FF; A: 1.0);
  clKhaki: TColorF = (R: $F0 / $FF; G: $E6 / $FF; B: $8C / $FF; A: 1.0);
  clLavender: TColorF = (R: $E6 / $FF; G: $E6 / $FF; B: $FA / $FF; A: 1.0);
  clLavenderBlush: TColorF = (R: $FF / $FF; G: $F0 / $FF; B: $F5 / $FF; A: 1.0);
  clLawnGreen: TColorF = (R: $7C / $FF; G: $FC / $FF; B: $00 / $FF; A: 1.0);
  clLemonChiffon: TColorF = (R: $FF / $FF; G: $FA / $FF; B: $CD / $FF; A: 1.0);
  clLightBlue: TColorF = (R: $AD / $FF; G: $D8 / $FF; B: $E6 / $FF; A: 1.0);
  clLightCoral: TColorF = (R: $F0 / $FF; G: $80 / $FF; B: $80 / $FF; A: 1.0);
  clLightCyan: TColorF = (R: $E0 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  clLightGoldenrodYellow: TColorF = (R: $FA / $FF; G: $FA / $FF; B: $D2 / $FF; A: 1.0);
  clLightGray: TColorF = (R: $D3 / $FF; G: $D3 / $FF; B: $D3 / $FF; A: 1.0);
  clLightGreen: TColorF = (R: $90 / $FF; G: $EE / $FF; B: $90 / $FF; A: 1.0);
  clLightPink: TColorF = (R: $FF / $FF; G: $B6 / $FF; B: $C1 / $FF; A: 1.0);
  clLightSalmon: TColorF = (R: $FF / $FF; G: $A0 / $FF; B: $7A / $FF; A: 1.0);
  clLightSeaGreen: TColorF = (R: $20 / $FF; G: $B2 / $FF; B: $AA / $FF; A: 1.0);
  clLightSkyBlue: TColorF = (R: $87 / $FF; G: $CE / $FF; B: $FA / $FF; A: 1.0);
  clLightSlateGray: TColorF = (R: $77 / $FF; G: $88 / $FF; B: $99 / $FF; A: 1.0);
  clLightSteelBlue: TColorF = (R: $B0 / $FF; G: $C4 / $FF; B: $DE / $FF; A: 1.0);
  clLightYellow: TColorF = (R: $FF / $FF; G: $FF / $FF; B: $E0 / $FF; A: 1.0);
  clLime: TColorF = (R: $00 / $FF; G: $FF / $FF; B: $00 / $FF; A: 1.0);
  clLimeGreen: TColorF = (R: $32 / $FF; G: $CD / $FF; B: $32 / $FF; A: 1.0);
  clLinen: TColorF = (R: $FA / $FF; G: $F0 / $FF; B: $E6 / $FF; A: 1.0);
  clMagenta: TColorF = (R: $FF / $FF; G: $00 / $FF; B: $FF / $FF; A: 1.0);
  clMaroon: TColorF = (R: $80 / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  clMediumAquamarine: TColorF = (R: $66 / $FF; G: $CD / $FF; B: $AA / $FF; A: 1.0);
  clMediumBlue: TColorF = (R: $00 / $FF; G: $00 / $FF; B: $CD / $FF; A: 1.0);
  clMediumOrchid: TColorF = (R: $BA / $FF; G: $55 / $FF; B: $D3 / $FF; A: 1.0);
  clMediumPurple: TColorF = (R: $93 / $FF; G: $70 / $FF; B: $DB / $FF; A: 1.0);
  clMediumSeaGreen: TColorF = (R: $3C / $FF; G: $B3 / $FF; B: $71 / $FF; A: 1.0);
  clMediumSlateBlue: TColorF = (R: $7B / $FF; G: $68 / $FF; B: $EE / $FF; A: 1.0);
  clMediumSpringGreen: TColorF = (R: $00 / $FF; G: $FA / $FF; B: $9A / $FF; A: 1.0);
  clMediumTurquoise: TColorF = (R: $48 / $FF; G: $D1 / $FF; B: $CC / $FF; A: 1.0);
  clMediumVioletRed: TColorF = (R: $C7 / $FF; G: $15 / $FF; B: $85 / $FF; A: 1.0);
  clMidnightBlue: TColorF = (R: $19 / $FF; G: $19 / $FF; B: $70 / $FF; A: 1.0);
  clMintCream: TColorF = (R: $F5 / $FF; G: $FF / $FF; B: $FA / $FF; A: 1.0);
  clMistyRose: TColorF = (R: $FF / $FF; G: $E4 / $FF; B: $E1 / $FF; A: 1.0);
  clMoccasin: TColorF = (R: $FF / $FF; G: $E4 / $FF; B: $B5 / $FF; A: 1.0);
  clNavajoWhite: TColorF = (R: $FF / $FF; G: $DE / $FF; B: $AD / $FF; A: 1.0);
  clNavy: TColorF = (R: $00 / $FF; G: $00 / $FF; B: $80 / $FF; A: 1.0);
  clOldLace: TColorF = (R: $FD / $FF; G: $F5 / $FF; B: $E6 / $FF; A: 1.0);
  clOlive: TColorF = (R: $80 / $FF; G: $80 / $FF; B: $00 / $FF; A: 1.0);
  clOliveDrab: TColorF = (R: $6B / $FF; G: $8E / $FF; B: $23 / $FF; A: 1.0);
  clOrange: TColorF = (R: $FF / $FF; G: $A5 / $FF; B: $00 / $FF; A: 1.0);
  clOrangeRed: TColorF = (R: $FF / $FF; G: $45 / $FF; B: $00 / $FF; A: 1.0);
  clOrchid: TColorF = (R: $DA / $FF; G: $70 / $FF; B: $D6 / $FF; A: 1.0);
  clPaleGoldenrod: TColorF = (R: $EE / $FF; G: $E8 / $FF; B: $AA / $FF; A: 1.0);
  clPaleGreen: TColorF = (R: $98 / $FF; G: $FB / $FF; B: $98 / $FF; A: 1.0);
  clPaleTurquoise: TColorF = (R: $AF / $FF; G: $EE / $FF; B: $EE / $FF; A: 1.0);
  clPaleVioletRed: TColorF = (R: $DB / $FF; G: $70 / $FF; B: $93 / $FF; A: 1.0);
  clPapayaWhip: TColorF = (R: $FF / $FF; G: $EF / $FF; B: $D5 / $FF; A: 1.0);
  clPeachPuff: TColorF = (R: $FF / $FF; G: $DA / $FF; B: $B9 / $FF; A: 1.0);
  clPeru: TColorF = (R: $CD / $FF; G: $85 / $FF; B: $3F / $FF; A: 1.0);
  clPink: TColorF = (R: $FF / $FF; G: $C0 / $FF; B: $CB / $FF; A: 1.0);
  clPlum: TColorF = (R: $DD / $FF; G: $A0 / $FF; B: $DD / $FF; A: 1.0);
  clPowderBlue: TColorF = (R: $B0 / $FF; G: $E0 / $FF; B: $E6 / $FF; A: 1.0);
  clPurple: TColorF = (R: $80 / $FF; G: $00 / $FF; B: $80 / $FF; A: 1.0);
  clRed: TColorF = (R: $FF / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  clRosyBrown: TColorF = (R: $BC / $FF; G: $8F / $FF; B: $8F / $FF; A: 1.0);
  clRoyalBlue: TColorF = (R: $41 / $FF; G: $69 / $FF; B: $E1 / $FF; A: 1.0);
  clSaddleBrown: TColorF = (R: $8B / $FF; G: $45 / $FF; B: $13 / $FF; A: 1.0);
  clSalmon: TColorF = (R: $FA / $FF; G: $80 / $FF; B: $72 / $FF; A: 1.0);
  clSandyBrown: TColorF = (R: $F4 / $FF; G: $A4 / $FF; B: $60 / $FF; A: 1.0);
  clSeaGreen: TColorF = (R: $2E / $FF; G: $8B / $FF; B: $57 / $FF; A: 1.0);
  clSeaShell: TColorF = (R: $FF / $FF; G: $F5 / $FF; B: $EE / $FF; A: 1.0);
  clSienna: TColorF = (R: $A0 / $FF; G: $52 / $FF; B: $2D / $FF; A: 1.0);
  clSilver: TColorF = (R: $C0 / $FF; G: $C0 / $FF; B: $C0 / $FF; A: 1.0);
  clSkyBlue: TColorF = (R: $87 / $FF; G: $CE / $FF; B: $EB / $FF; A: 1.0);
  clSlateBlue: TColorF = (R: $6A / $FF; G: $5A / $FF; B: $CD / $FF; A: 1.0);
  clSlateGray: TColorF = (R: $70 / $FF; G: $80 / $FF; B: $90 / $FF; A: 1.0);
  clSnow: TColorF = (R: $FF / $FF; G: $FA / $FF; B: $FA / $FF; A: 1.0);
  clSpringGreen: TColorF = (R: $00 / $FF; G: $FF / $FF; B: $7F / $FF; A: 1.0);
  clSteelBlue: TColorF = (R: $46 / $FF; G: $82 / $FF; B: $B4 / $FF; A: 1.0);
  clTan: TColorF = (R: $D2 / $FF; G: $B4 / $FF; B: $8C / $FF; A: 1.0);
  clTeal: TColorF = (R: $00 / $FF; G: $80 / $FF; B: $80 / $FF; A: 1.0);
  clThistle: TColorF = (R: $D8 / $FF; G: $BF / $FF; B: $D8 / $FF; A: 1.0);
  clTomato: TColorF = (R: $FF / $FF; G: $63 / $FF; B: $47 / $FF; A: 1.0);
  clTransparent: TColorF = (R: $FF / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  clTurquoise: TColorF = (R: $40 / $FF; G: $E0 / $FF; B: $D0 / $FF; A: 1.0);
  clViolet: TColorF = (R: $EE / $FF; G: $82 / $FF; B: $EE / $FF; A: 1.0);
  clWheat: TColorF = (R: $F5 / $FF; G: $DE / $FF; B: $B3 / $FF; A: 1.0);
  clWhite: TColorF = (R: $FF / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  clWhiteSmoke: TColorF = (R: $F5 / $FF; G: $F5 / $FF; B: $F5 / $FF; A: 1.0);
  clYellow: TColorF = (R: $FF / $FF; G: $FF / $FF; B: $00 / $FF; A: 1.0);
  clYellowGreen: TColorF = (R: $9A / $FF; G: $CD / $FF; B: $32 / $FF; A: 1.0);
{doc on}

{ Returns true if a value is a power of two }
function IsPowerOfTwo(Value: Integer): Boolean;
{ Returns true if both w and h are powers of two }
function IsPowerOfTwos(W, H: Integer): Boolean;
{ Returns the next lowest power given a value  }
function PowerOfTwo(Value: Integer): Integer;

type
  { Specifies if textures edges are clamped or if they wrap }
  TTextureWrap = (wrapNone, wrapS, wrapT, wrapST);
  { Specifies how texture pixels are interpolated }
  TTextureFilter = (filterNearest, filterLinear);

{ TTextures is a simple container which can manage OpenGL texture units
  Remarks
  Texture units must be allocated with generate before loading data or binding
  See also
  <link Overview.Bare.Graphics.TTextures, TTextures members> }

  TTextures = class
  private
    FData: TArray<GLuint>;
    function GetCount: Integer;
    function GetTexture(Index: Integer): GLuint;
    function GetWrap(Index: Integer): TTextureWrap;
    procedure SetWrap(Index: Integer; Value: TTextureWrap);
    function GetMinify(Index: Integer): TTextureFilter;
    procedure SetMinify(Index: Integer; Value: TTextureFilter);
    function GetMagnify(Index: Integer): TTextureFilter;
    procedure SetMagnify(Index: Integer; Value: TTextureFilter);
  public
    { Create storage and optionally allocated count number of OpenGL texture unit slots }
    constructor Create(Count: Integer = 0);
    destructor Destroy; override;
    { Allocate slots containing count number of OpenGL texture units }
    procedure Generate(Count: Integer);
    { Generate mipmaps for OpenGL texture unit stored at index
      See also
      <exref target="http://en.wikipedia.org/wiki/mipmap">External: Mipmaps on wikipedia</exref> }
    procedure GenerateMipmaps(Index: Integer);
    { Delete slots releasing OpenGL texture units and setting count to zero }
    procedure Delete;
    { Make texture unit in slot index current }
    procedure Bind(Index: Integer);
    { Load pixels into a texture unit slot }
    function Load(Width, Height: Integer; Pixels: Pointer; Index: Integer): Boolean; overload;
    { Load a bitmap into a texture unit slot
      <link Bare.Graphics.IsPowerOfTwos, IsPowerOfTwos function>
      <link Bare.Graphics.Imaging.IBitmap.Resample, IBitmap.Resample method> }
    function Load(Bitmap: IBitmap; Index: Integer): Boolean; overload;
    { Load a stream contain bitmap data into a texture unit slot }
    function Load(Stream: TStream; Index: Integer): Boolean; overload;
    { Load a bitmap from the file system into a texture unit slot }
    function Load(const FileName: string; Index: Integer): Boolean; overload;
    { The number of texture unit slots }
    property Count: Integer read GetCount;
    { Get the OpenGL texture unit }
    property Texture[Index: Integer]: GLuint read GetTexture; default;
    { Specify wrapping mode for OpenGL texture unit stored at index }
    property Wrap[Index: Integer]: TTextureWrap read GetWrap write SetWrap;
    { Specify minification filter for OpenGL texture unit stored at index }
    property Minify[Index: Integer]: TTextureFilter read GetMinify write SetMinify;
    { Specify magnification filter for OpenGL texture unit stored at index }
    property Magnify[Index: Integer]: TTextureFilter read GetMagnify write SetMagnify;
  end;

{ TWorld manages a viewport and translates vertices between 2d world coordinates and
  3d space coordinates
  See also
  <link Overview.Bare.Graphics.TWorld, TWorld members> }

  TWorld = class
  private
    FWindow: IWindow;
    FFieldOfView: Float;
    FDepth: Float;
    FRatio: Float;
    FNearPlane: Float;
    FFarPlane: Float;
    FWidth: Float;
    FHeight: Float;
    FHalfWidth: Float;
    FHalfHeight: Float;
    FTangent: Float;
    FOrthoWidth: Integer;
    FOrthoHeight: Integer;
  public
    { Create a 2d world based on a rectangular area }
    constructor Create(Window: IWindow);
    { Capture a snapshot the of the 2d world into an IBitmap }
    function Snapshot: IBitmap;
    { Clears the drawing buffers, update the viewport and perspective }
    procedure Update(FieldOfView: Float = 60; Depth: Float = -10; NearPlane: Float = 0.1; FarPlane: Float = 1000);
    { Convert x and y from 2d world coordinates to 3d space coordinates }
    function WorldToSpace(X, Y: Float): TVec3; overload;
    { Convert a vec2 from 2d world coordinates to 3d space coordinates }
    function WorldToSpace(const V: TVec2): TVec3; overload;
    { Convert x, y, and z from 3d space coordinates to 2d world coordinates }
    function SpaceToWorld(X, Y, Z: Float): TVec2; overload;
    { Convert a vec3 from 3d space coordinates to 2d world coordinates }
    function SpaceToWorld(const V: TVec3): TVec2; overload;
    { Maps a window to emulate a specific resolution }
    procedure OrthoMap(Width, Height: Integer);
    { Maps a window to its own size }
    procedure OrthoUnmap;
    { Convert a window x, y to a resolution specific point }
    function OrthoPoint(X, Y: Float): TVec2; overload;
    { Convert a window point to a resolution specific point }
    function OrthoPoint(V: TVec2): TVec2; overload;
    { Set the current clear color }
    procedure ClearColor(R, G, B: Float; A: Float = 1.0); overload;
    { Set the current clear color }
    procedure ClearColor(const Color: TColorF); overload;
    { Set the current drawing color }
    procedure Color(R, G, B: Float; A: Float = 1.0); overload;
    { Set the current drawing color }
    procedure Color(const Color: TColorF); overload;
    { Begin a series of vertex quad commands }
    procedure BeginQuads;
    { End a series of vertex quad commands }
    procedure EndQuads;
    { Begin a series of vertex point sprite commands }
    procedure BeginPoints(Size: Float);
    { End a series of vertex point sprite commands }
    procedure EndPoints;
    { Bind a texture to the current render context saving the previous state }
    procedure BindTex(Texture: GLuint);
    { Unbind a texture restoring the previous state }
    procedure UnbindTex;
    { Add a 2d textured vertex to the current drawing command with texture coordinates
      s and t }
    procedure TexVertex(X, Y, S, T: Float); overload;
    { Add a 2d textured vertex to the current drawing command with texture coordinates
      contained in t }
    procedure TexVertex(const V, T: TVec2); overload;
    { Add a 3d textured vertex to the current drawing command with texture coordinates
      contained in t }
    procedure TexVertex(const V: TVec3; const T: TVec2); overload;
    { Add a 2d vertex to the current drawing command }
    procedure Vertex(X, Y: Float); overload;
    { Add a 2d vertex to the current drawing command }
    procedure Vertex(const V: TVec2); overload;
    { Add a 3d vertex to the current drawing command }
    procedure Vertex(const V: TVec3); overload;
    { The z distance in space units of the 2d plane }
    property Depth: Float read FDepth;
    { The ratio of world pixels to space units }
    property Ratio: Float read FRatio;
    { The width of the world in pixels }
    property Width: Float read FWidth;
    { The height of the world in pixels }
    property Height: Float read FHeight;
  end;

  {doc ignore}
  TCustomSprite = class;

{ TCustomSprite is a class for defining 2d sprites
  Remarks
  You may derive a sprite from this class to define your own sprite logic
  See also
  <link Overview.Bare.Graphics.TCustomSprite, TCustomSprite members> }

  TCustomSprite = class(TPersistsObject)
  private
    FWorld: TWorld;
    FTexture: GLuint;
    FMatrixChanged: Boolean;
    FGeometryChanged: Boolean;
    FColor: TVec4Prop;
    FTexCoords: TVec4Prop;
    { Causes geometry to change }
    FOrigin: TVec2Prop;
    FSize: TVec2Prop;
    { Causes matrix to change }
    FPosition: TVec3Prop;
    FRotation: TVec3Prop;
    FScale: TVec2Prop;
    procedure GeometryChange(Prop: IDependencyProperty; Index: Integer);
    procedure MatrixChange(Prop: IDependencyProperty; Index: Integer);
    procedure SetColor(const Value: TVec4Prop);
    procedure SetOrigin(const Value: TVec2Prop);
    procedure SetPosition(const Value: TVec3Prop);
    procedure SetRotation(const Value: TVec3Prop);
    procedure SetScale(const Value: TVec2Prop);
    procedure SetSize(const Value: TVec2Prop);
    procedure SetTexCoords(const Value: TVec4Prop);
    procedure SetTexture(Value: GLuint);
  protected
    { Provide a default transform matrix based on orientation }
    procedure DefaultMatrix(out Matrix: TMatrix);
    { Provide four default vertices given a matrix }
    procedure DefaultTransform(var Matrix: TMatrix; out Quad: TQuad);
    { Calculate a new transform matrix }
    procedure UpdateMatrix; virtual;
    { Calculate new vertices }
    procedure UpdateGeometry; virtual;
    { Calculate texture details }
    procedure UpdateTexture; virtual;
    { Renders the sprite to the screen }
    procedure Render; virtual;
    { Copy the source into the custom sprite }
    function AssignFrom(Source: TObject): Boolean; override;
    { The color of the sprite, defaults to white }
    property Color: TVec4Prop read FColor write SetColor;
    { The origin of the sprite, defaults to the center }
    property Origin: TVec2Prop read FOrigin write SetOrigin;
    { The size in pixels of the sprite, defaults to 32 }
    property Size: TVec2Prop read FSize write SetSize;
    { The position of the sprite, defaults to 0 }
    property Position: TVec3Prop read FPosition write SetPosition;
    { The rotation angles of the sprite, defaults to 0 }
    property Rotation: TVec3Prop read FRotation write SetRotation;
    { The scale of the sprite, defaults to 1 }
    property Scale: TVec2Prop read FScale write SetScale;
    { The texture coords of the sprite, defaults to (S0: 0; T0 0; S1: 1; T1: 1)) }
    property TexCoords: TVec4Prop read FTexCoords write SetTexCoords;
    { The texure unit associated with the sprite }
    property Texture: GLuint read FTexture write SetTexture;
    { The 2d worldin which the sprite exists }
    property World: TWorld read FWorld;
  public
    { Create a sprite in a 2d world }
    constructor Create(World: TWorld); virtual;
    { Create a copy of the sprite }
    function Clone: TCustomSprite;
    { Draw the sprite by updating and rendering }
    procedure Draw;
    { Update the sprite matrix and geometry }
    procedure Update;
  end;

{ Class of custom sprite }

  TSpriteClass = class of TCustomSprite;

{ TSprite is a simple sprite class
  See also
  <link Overview.Bare.Graphics.TSprite, TSprite members> }

  TSprite = class(TCustomSprite)
  private
    FMatrix: TMatrix;
    FQuads: TQuad;
    FPolygon: TPolygon;
  protected
    { Recreate the matrix }
    procedure UpdateMatrix; override;
    { Recreate the geometry }
    procedure UpdateGeometry; override;
    { Render the sprite into the 2d world }
    procedure Render; override;
  public
    { Get a 3d copy of the transformed geometry }
    procedure GetGeometry(out Quad: TQuad);
    { Get a flattened 2d copy of the transformed geometry }
    procedure GetPolygon(out Polygon: TPolygon);
    { Size of the sprite }
    property Size;
    { Color of the sprite }
    property Color;
    { Origin which effects positioning, scaling, and rotation }
    property Origin;
    { Position of the sprite including z distance }
    property Position;
    { Rotation of the sprite }
    property Rotation;
    { Scale of the sprite }
    property Scale;
    { Texture coordinates defining the sprite image }
    property TexCoords;
    { The OpenGL texture unit from which a sprite is taken }
    property Texture;
  end;

{ TTileMode describes if a background should be stretched or repeated }

  TTileMode = (tileStretch, tileRepeat);

{ TBackgroudSprite draws tiled backgrounds
  See also
  <link Overview.Bare.Graphics.TBackgroudSprite, TBackgroudSprite members> }

  TBackgroudSprite = class(TCustomSprite)
  private
    FWidth: GLuint;
    FHeight: GLuint;
    FTileMode: TTileMode;
    function GetAngle: TVec1Prop;
    procedure SetAngle(const Value: TVec1Prop);
    function GetPosition: TVec2Prop;
    procedure SetPosition(const Value: TVec2Prop);
  protected
    { Update texture details }
    procedure UpdateTexture; override;
    { Render the background into the 2d world }
    procedure Render; override;
  public
    { Tile mode controls switches between background stretching or repeating modes }
    property TileMode: TTileMode read FTileMode write FTileMode;
    { The color of the background }
    property Color;
    { The top left position of the bacground in pixels }
    property Position: TVec2Prop read GetPosition write SetPosition;
    { The width and height of the background in pixels }
    property Size;
    { The angle of the texture }
    property Angle: TVec1Prop read GetAngle write SetAngle;
    { The origin of the texture }
    property Origin;
    { The scale of the texture }
    property Scale;
    { The OpenGL texture unit which is stretched or repeated in the background }
    property Texture;
  end;

  { Horizontal alignment of a font }
  TFontJustify = (justifyLeft, justifyCenter, justifyRight);
  { Texture coordinates of a font character }
  TFontCoord = TVec2;

{ Information about a font character }

  TFontChar = record
    Left: Float;
    Top: Float;
    Right: Float;
    Bottom: Float;
    Width: Float;
    Height: Float;
    A: TFontCoord;
    B: TFontCoord;
  end;
  PFontChar = ^TFontChar;

{ Character mapping }

  TFontMap = array[$20..$7D] of TFontChar;

{ Information about a font }

  TFontStore = record
    Face: string[50];
    Size: Integer;
    Kerning: Integer;
    Map: TFontMap;
  end;

{ TFontWriter writes text using textured quads }

  TFontWriter = class
  private
    FWorld: TWorld;
    FStore: TFontStore;
    FTextures: TTextures;
    FBlendedState: GLboolean;
    FCulledState: GLboolean;
    FDepthState: GLboolean;
    FColorState: TVec4;
    FLines: TStringList;
    FOpacity: TVec1Prop;
    FOrigin: TVec2Prop;
    FShadows: Boolean;
    FLeadColor: TVec4Prop;
    FBaseColor: TVec4Prop;
    FShadowColor: TVec4Prop;
    FShadowOffset: TVec2Prop;
    procedure SaveState;
    procedure RestoreState;
    procedure InternalWrite(const Line: string; Scale, Col, Row: Float);
    function GetFace: string;
    function GetKerning: Integer;
    function GetSize: Integer;
    function GetTextHeight: Float;
    procedure SetOpacity(const Value: TVec1Prop);
    procedure SetOrigin(const Value: TVec2Prop);
    procedure SetLeadColor(const Value: TVec4Prop);
    procedure SetBaseColor(const Value: TVec4Prop);
    procedure SetShadowColor(const Value: TVec4Prop);
    procedure SetShadowOffset(const Value: TVec2Prop);
  public
    constructor Create(World: TWorld);
    destructor Destroy; override;
    procedure LoadFromStore(var Store: TFontStore; Bits: Pointer);
    procedure LoadFromResource(const Resource: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    function Measure(const S: string; Scale: Float): TVec2;
    procedure Write(const S: string; Scale, Col, Row: Float); overload;
    procedure Write(const S: string; Scale, X, Y: Float; Justify: TFontJustify); overload;
    property Face: string read GetFace;
    property Kerning: Integer read GetKerning;
    property Size: Integer read GetSize;
    property TextHeight: Float read GetTextHeight;
    property Opacity: TVec1Prop read FOpacity write SetOpacity;
    property Origin: TVec2Prop read FOrigin write SetOrigin;
    property LeadColor: TVec4Prop read FLeadColor write SetLeadColor;
    property BaseColor: TVec4Prop read FBaseColor write SetBaseColor;
    property Shadows: Boolean read FShadows write FShadows;
    property ShadowColor: TVec4Prop read FShadowColor write SetShadowColor;
    property ShadowOffset: TVec2Prop read FShadowOffset write SetShadowOffset;
  end;

function FontToBitmap(const FileName: string): IBitmap;
function FontToStr(const FileName: string): string;
procedure FontLoadDefault(Font: TFontWriter);

implementation

function IsPowerOfTwo(Value: Integer): Boolean;
var
  T: Integer;
begin
  T := $1;
  repeat
    Result := Value = T;
    T := T shl 1;
  until Result or (T = $10000000);
end;

function IsPowerOfTwos(W, H: Integer): Boolean;
begin
  Result := IsPowerOfTwo(W) and IsPowerOfTwo(H);
end;

function PowerOfTwo(Value: Integer): Integer;
begin
  Result := Value;
  if IsPowerOfTwo(Result) then Exit;
  Result := $1;
  while Value > Result do
    Result := Result shl 1;
  Result := Result shr 1;
end;

{ TTextures }

constructor TTextures.Create(Count: Integer = 0);
begin
  inherited Create;
  Generate(Count);
end;

destructor TTextures.Destroy;
begin
  Delete;
  inherited Destroy;
end;

procedure TTextures.Generate(Count: Integer);
begin
  Delete;
  if Count > 0 then
  begin
    SetLength(FData, Count);
    glGenTextures(Count, FData[0]);
  end;
end;

procedure TTextures.GenerateMipmaps(Index: Integer);
begin
  Bind(Index);
  if @glGenerateMipmap <> nil then
    glGenerateMipmap(GL_TEXTURE_2D)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
end;

procedure TTextures.Delete;
begin
  if Length(FData) > 0 then
    glDeleteTextures(Length(FData), FData[0]);
  FData := nil;
end;

procedure TTextures.Bind(Index: Integer);
begin
  glBindTexture(GL_TEXTURE_2D, FData[Index]);
end;

function TTextures.Load(Width, Height: Integer; Pixels: Pointer; Index: Integer): Boolean;
begin
  Bind(Index);
  glGetError;
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, Pixels);
  if glGetError = GL_NO_ERROR then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end
  else
    Result := False;
end;

function TTextures.Load(Bitmap: IBitmap; Index: Integer): Boolean;
begin
  if (Bitmap = nil) or Bitmap.Empty then
    Exit(False);
  Result := Load(Bitmap.Width, Bitmap.Height, Bitmap.Pixels, Index);
end;

function TTextures.Load(Stream: TStream; Index: Integer): Boolean;
begin
  Result := Load(CreateBitmap(Stream), Index);
end;

function TTextures.Load(const FileName: string; Index: Integer): Boolean;
begin
  if FileExists(FileName) then
    Result := Load(CreateBitmap(FileName), Index)
  else
    Result := False;
end;

function TTextures.GetCount: Integer;
begin
  Result := Length(FData);
end;

function TTextures.GetTexture(Index: Integer): GLuint;
begin
  Result := FData[Index];
end;

function TTextures.GetWrap(Index: Integer): TTextureWrap;
var
  S, T: GLuint;
begin
  Bind(Index);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, S);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, T);
  if S and T and GL_REPEAT = GL_REPEAT then
    Result := wrapST
  else if S and GL_REPEAT = GL_REPEAT then
    Result := wrapS
  else if T and GL_REPEAT = GL_REPEAT then
    Result := wrapT
  else
    Result := wrapNone;
end;

procedure TTextures.SetWrap(Index: Integer; Value: TTextureWrap);
var
  S, T: GLuint;
begin
  Bind(Index);
  if (Value = wrapS) or (Value = wrapST)then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  if (Value = wrapT) or (Value = wrapST)then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
end;

function TTextures.GetMinify(Index: Integer): TTextureFilter;
var
  M: GLuint;
begin
  Bind(Index);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, M);
  if M and GL_NEAREST = GL_NEAREST then
    Result := filterNearest
  else
    Result := filterLinear;
end;

procedure TTextures.SetMinify(Index: Integer; Value: TTextureFilter);
begin
  Bind(Index);
  if Value = filterNearest then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
end;

function TTextures.GetMagnify(Index: Integer): TTextureFilter;
var
  M: GLuint;
begin
  Bind(Index);
  glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, M);
  if M and GL_NEAREST = GL_NEAREST then
    Result := filterNearest
  else
    Result := filterLinear;
end;

procedure TTextures.SetMagnify(Index: Integer; Value: TTextureFilter);
begin
  Bind(Index);
  if Value = filterNearest then
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
  else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

{ TWorld }

constructor TWorld.Create(Window: IWindow);
begin
  inherited Create;
  FWindow := Window;
end;

function TWorld.Snapshot: IBitmap;

  procedure Flip(P: PPixel; W, H: Integer);
  var
    Pixel: PPixel;
    Stride: LongWord;
    Line: PByte;

    function Scanline(I: Integer): PByte;
    begin
      Result := PByte(P);
      Inc(Result, I * Stride);
    end;

  var
    I, J: Integer;
  begin
    Pixel := P;
    Stride := W * SizeOf(TPixel);
    GetMem(Line, Stride);
    try
      I := 0;
      J := H - 1;
      while I < J do
      begin
        Move(Scanline(I)^, Line^, Stride);
        Move(Scanline(J)^, Scanline(I)^, Stride);
        Move(Line^, Scanline(J)^, Stride);
        Inc(I);
        Dec(J);
      end;
    finally
      FreeMem(Line);
    end;
  end;

var
  W, H: Integer;
  P: PPixel;
begin
  W := Round(FWidth);
  H := Round(FHeight);
  Result := CreateBitmap(W, H);
  if Result.Empty then
    Exit;
  P := Result.Pixels;
  glReadPixels(0, 0, W, H, GL_RGBA, GL_UNSIGNED_BYTE, P);
  Flip(P, W, H);
end;

procedure TWorld.Update(FieldOfView: Float = 60; Depth: Float = -10; NearPlane: Float = 0.1; FarPlane: Float = 1000);
var
  Changed: Boolean;
  W, H: Integer;
begin
  W := FWindow.Width;
  H := FWindow.Height;
  Changed := (W <> FWidth) or (H <> FHeight);
  Changed := Changed or (FieldOfView <> FFieldOfView) or (Depth <> FDepth) or
    (NearPlane <> FNearPlane) or (FarPlane <> FFarPlane);
  if Changed then
  begin
    FWidth := W;
    FHeight := H;
    FFieldOfView := FieldOfView;
    FDepth := Depth;
    FNearPlane := NearPlane;
    FFarPlane := FarPlane;
    FTangent := Tan(FFieldOfView / 360 * Pi);
    FHalfWidth := W / 2;
    FHalfHeight := H / 2;
    if FHalfHeight > 0 then
      FRatio := (Abs(FDepth) * FTangent) / FHalfHeight
    else
      FRatio := 0;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glViewport(0, 0, W, H);
    gluPerspective(FFieldOfView, W / H, FNearPlane, FFarPlane);
  end;
  glMatrixMode(GL_MODELVIEW);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
end;

{ Convert 2d window coordinates to 3d world coordinates }

function TWorld.WorldToSpace(X, Y: Float): TVec3;
begin
  if FOrthoWidth > 0 then
  begin
    X := X / FOrthoWidth * FWidth;
    Y := Y / FOrthoHeight * FHeight;
  end;
  Result := Vec3(X - FHalfWidth, FHalfHeight - Y, FDepth);
  Result.X := Result.X * FRatio;
  Result.Y := Result.Y * FRatio;
end;

function TWorld.WorldToSpace(const V: TVec2): TVec3;
begin
  Result := WorldToSpace(V.X, V.Y);
end;

{ Convert 3d world coordinates to 2d window coordinates }

function TWorld.SpaceToWorld(X, Y, Z: Float): TVec2;
begin
  if (Z < 0) and (FTangent > 0) then
  begin
    Result.X := (X / Z / FTangent) * FHalfHeight;
    Result.X := FHalfWidth - Result.X;
    Result.Y := (Y / Z / FTangent) * FHalfHeight;
    Result.Y := FHalfHeight + Result.Y;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
  if FOrthoWidth > 0 then
  begin
    Result.X := Result.X / FWidth * FOrthoWidth;
    Result.Y := Result.Y / FHeight * FOrthoHeight;
  end;
end;

function TWorld.SpaceToWorld(const V: TVec3): TVec2;
begin
  Result := SpaceToWorld(V.X, V.Y, V.Z);
end;

procedure TWorld.OrthoMap(Width, Height: Integer);
begin
  FOrthoWidth := Width;
  FOrthoHeight := Height;
  if (FOrthoWidth < 1) or (FOrthoHeight < 0) then
  begin
    FOrthoWidth := 0;
    FOrthoHeight := 0;
  end;
end;

procedure TWorld.OrthoUnmap;
begin
  FOrthoWidth := 0;
  FOrthoHeight := 0;
end;

function TWorld.OrthoPoint(X, Y: Float): TVec2;
begin
  if (FWidth > 0) and (FOrthoWidth > 0) then
  begin
    Result.X := X / FWidth * FOrthoWidth;
    Result.Y := Y / FHeight * FOrthoHeight
  end
  else
  begin
    Result.X := X;
    Result.Y := Y;
  end;
end;

function TWorld.OrthoPoint(V: TVec2): TVec2;
begin
  Result := OrthoPoint(V.X, V.Y);
end;

procedure TWorld.ClearColor(R, G, B: Float; A: Float = 1.0);
begin
  glClearColor(R, G, B, A);
end;

procedure TWorld.ClearColor(const Color: TColorF);
begin
  with Color do
    glClearColor(R, G, B, A);
end;

procedure TWorld.Color(R, G, B: Float; A: Float = 1.0);
begin
  glColor4f(R, G, B, A);
end;

procedure TWorld.Color(const Color: TColorF);
begin
  with Color do
    glColor4f(R, G, B, A);
end;

procedure TWorld.BeginQuads;
begin
  { Abstracting quads in preparation for OpenGL ES }
  glBegin(GL_QUADS);
end;

procedure TWorld.EndQuads;
begin
  glEnd;
end;

procedure TWorld.BeginPoints(Size: Float);
begin
  glEnable(GL_POINT_SPRITE);
  glTexEnvi(GL_POINT_SPRITE, GL_COORD_REPLACE, GL_TRUE);
  glDepthMask(False);
  glPointSize(Size);
  glBegin(GL_POINTS);
end;

procedure TWorld.EndPoints;
begin
  glEnd;
  glDepthMask(True);
  glTexEnvi(GL_POINT_SPRITE, GL_COORD_REPLACE, GL_FALSE);
  glDisable(GL_POINT_SPRITE);
end;

procedure TWorld.BindTex(Texture: GLuint);
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, Texture);
end;

procedure TWorld.UnbindTex;
begin
  glDisable(GL_TEXTURE_2D);
end;

procedure TWorld.TexVertex(X, Y, S, T: Float);
begin
  { Abstracting textured verticies verticies. May use VBOs soon. }
  TexVertex(Vec(X, Y), Vec(S, T));
end;

procedure TWorld.TexVertex(const V, T: TVec2);
begin
  glTexCoord2d(T.S, T.T);
  with WorldToSpace(V) do
    glVertex3f(X, Y, Z);
end;

procedure TWorld.TexVertex(const V: TVec3; const T: TVec2);
begin
  glTexCoord2d(T.S, T.T);
  with V do
    glVertex3f(X, Y, Z);
end;

procedure TWorld.Vertex(X, Y: Float);
begin
  { Abstracting verticies in preparation for OpenGL ES }
  Vertex(Vec(X, Y));
end;

procedure TWorld.Vertex(const V: TVec2);
begin
  with WorldToSpace(V) do
    glVertex3f(X, Y, Z);
end;

procedure TWorld.Vertex(const V: TVec3);
begin
  with V do
    glVertex3f(X, Y, Z);
end;

{ TCustomSprite }

constructor TCustomSprite.Create(World: TWorld);
begin
  inherited Create;
  FWorld := World;
  FColor := TVec4Prop.Create(GeometryChange);
  FColor.Value := clWhite;
  FTexCoords := TVec4Prop.Create;
  FTexCoords.Value := Vec4(0, 0, 1, 1);
  FSize := TVec2Prop.Create(GeometryChange);
  FSize.Value := Vec2(32, 32);
  FOrigin := TVec2Prop.Create(GeometryChange);
  FOrigin.Value := Vec2(0.5, 0.5);
  FGeometryChanged := True;
  FPosition := TVec3Prop.Create(MatrixChange);
  FRotation := TVec3Prop.Create(MatrixChange);
  FScale := TVec2Prop.Create(MatrixChange);
  FScale.Value := Vec2(1, 1);
  FMatrixChanged := True;
end;

function TCustomSprite.AssignFrom(Source: TObject): Boolean;
var
  Sprite: TCustomSprite;
begin
  if Source is TCustomSprite then
  begin
    Sprite := Source as TCustomSprite;
    FWorld := Sprite.FWorld;
    FColor.Value := Sprite.FColor.Value;
    FOrigin.Value := Sprite.FOrigin.Value;
    FPosition.Value := Sprite.FPosition.Value;
    FRotation.Value := Sprite.FRotation.Value;
    FScale.Value := Sprite.FScale.Value;
    FSize.Value := Sprite.FSize.Value;
    FTexCoords.Value := Sprite.FTexCoords.Value;
    Result := True;
  end
  else
    Result := inherited AssignFrom(Source);
end;

function TCustomSprite.Clone: TCustomSprite;
var
  SpriteClass: TSpriteClass;
begin
  SpriteClass :=  TSpriteClass(ClassType);
  Result := SpriteClass.Create(FWorld);
  Assign(Self, Result);
end;

procedure TCustomSprite.DefaultMatrix(out Matrix: TMatrix);
var
  V: TVec3;
begin
  Matrix := StockMatrix;
  V := FWorld.WorldToSpace(Vec2(FPosition.X, FPosition.Y));
  V.Z := FWorld.Depth + FPosition.Z;
  Matrix.Translate(V.X, V.Y, V.Z);
  Matrix.Rotate(FRotation.X, FRotation.Y, FRotation.Z);
  Matrix.Scale(FScale.X, FScale.Y, 1);
end;

procedure TCustomSprite.DefaultTransform(var Matrix: TMatrix; out Quad: TQuad);
var
  I: Integer;
begin
  Quad[0].X := -FOrigin.X * FSize.X * FWorld.Ratio;
  Quad[0].Y := -FOrigin.Y * FSize.Y * -FWorld.Ratio;
  Quad[0].Z := 0;
  Quad[2].X := (1 - FOrigin.X) * FSize.X * FWorld.Ratio;
  Quad[2].Y := (1 - FOrigin.Y) * FSize.Y * -FWorld.Ratio;
  Quad[2].Z := 0;
  Quad[1].X := Quad[0].X;
  Quad[1].Y := Quad[2].Y;
  Quad[1].Z := 0;
  Quad[3].X := Quad[2].X;
  Quad[3].Y := Quad[0].Y;
  Quad[3].Z := 0;
  for I := Low(Quad) to High(Quad) do
    Quad[I] := Matrix * Quad[I];
end;

procedure TCustomSprite.GeometryChange(Prop: IDependencyProperty; Index: Integer);
begin
  FGeometryChanged := True;
end;

procedure TCustomSprite.MatrixChange(Prop: IDependencyProperty; Index: Integer);
begin
  FMatrixChanged := True;
end;

procedure TCustomSprite.UpdateMatrix;
begin
end;

procedure TCustomSprite.UpdateGeometry;
begin
end;

procedure TCustomSprite.UpdateTexture;
begin
end;

procedure TCustomSprite.Render;
begin
end;


procedure TCustomSprite.Draw;
begin
  Update;
  Render;
end;

procedure TCustomSprite.Update;
begin
  if FMatrixChanged then
  begin
    UpdateMatrix;
    UpdateGeometry;
    FMatrixChanged := False;
    FGeometryChanged := False;
  end
  else if FGeometryChanged then
  begin
    UpdateGeometry;
    FGeometryChanged := False;
  end;
end;

procedure TCustomSprite.SetColor(const Value: TVec4Prop);
begin
  FColor.Value := Value;
end;

procedure TCustomSprite.SetTexCoords(const Value: TVec4Prop);
begin
  FTexCoords.Value := Value;
end;

procedure TCustomSprite.SetOrigin(const Value: TVec2Prop);
begin
  FOrigin.Value := Value;
end;

procedure TCustomSprite.SetSize(const Value: TVec2Prop);
begin
  FSize.Value := Value;
end;

procedure TCustomSprite.SetPosition(const Value: TVec3Prop);
begin
  FPosition.Value := Value;
end;

procedure TCustomSprite.SetRotation(const Value: TVec3Prop);
begin
  FRotation.Value := Value;
end;

procedure TCustomSprite.SetScale(const Value: TVec2Prop);
begin
  FScale.Value := Value;
end;

procedure TCustomSprite.SetTexture(Value: GLuint);
begin
  FTexture := Value;
  UpdateTexture;
end;

{ TSprite }

procedure TSprite.UpdateMatrix;
begin
  DefaultMatrix(FMatrix);
end;

procedure TSprite.UpdateGeometry;
var
  I: Integer;
begin
  DefaultTransform(FMatrix, FQuads);
  if FPolygon = nil then
    SetLength(FPolygon, 4);
  for I := Low(FPolygon) to High(FPolygon) do
    FPolygon[I] := FWorld.SpaceToWorld(FQuads[I]);
end;

procedure TSprite.Render;
var
  T: TVec4;
begin
  T := FTexCoords;
  FWorld.BindTex(FTexture);
  FWorld.Color(Color);
  FWorld.BeginQuads;
  FWorld.TexVertex(FQuads[0], Vec2(T.S0, T.T0));
  FWorld.TexVertex(FQuads[1], Vec2(T.S0, T.T1));
  FWorld.TexVertex(FQuads[2], Vec2(T.S1, T.T1));
  FWorld.TexVertex(FQuads[3], Vec2(T.S1, T.T0));
  FWorld.EndQuads;
  FWorld.UnbindTex;
end;

procedure TSprite.GetGeometry(out Quad: TQuad);
begin
  Quad := FQuads;
end;

procedure TSprite.GetPolygon(out Polygon: TPolygon);
begin
  Polygon := FPolygon;
  SetLength(Polygon, Length(FPolygon));
end;

{ TBackgroudSprite }

procedure TBackgroudSprite.UpdateTexture;
begin
  World.BindTex(FTexture);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, FWidth);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, FHeight);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  World.UnbindTex;
end;

procedure TBackgroudSprite.Render;
const
  Sigma = 0.001;
var
  P, S, O: TVec2;
  Q: TQuad;
begin
  if (FWidth < 1) or (FHeight < 1) then
    Exit;
  P := Position;
  S := Size;
  O := Origin;
  if (Abs(S.X) < Sigma) or (Abs(S.Y) < Sigma) then
    Exit;
  glDisable(GL_DEPTH_TEST);
  glMatrixMode(GL_TEXTURE);
  if FTileMode = tileRepeat then
  begin
    glRotatef(-Angle.Value, 0, 0, 1);
    glScalef(1 / Scale.X, 1 / Scale.Y, 1);
    glTranslatef(S.X / FWidth * -O.X, S.Y / FHeight * -O.Y, 0);
    glScalef(S.X / FWidth, S.Y / FHeight, 1);
  end
  else
  begin
    glRotatef(-Angle.Value, 0, 0, 1);
    glScalef(1 / Scale.X, 1 / Scale.Y, 1);
    glTranslatef(-O.X, -O.Y, 0);
  end;
  glMatrixMode(GL_MODELVIEW);
  Q[0] := World.WorldToSpace(P);
  Q[1] := World.WorldToSpace(P.X, P.Y + S.Y);
  Q[2] := World.WorldToSpace(P + S);
  Q[3] := World.WorldToSpace(P.X + S.X, P.Y);
  FWorld.BindTex(FTexture);
  FWorld.Color(Color);
  FWorld.BeginQuads;
  FWorld.TexVertex(Q[0], Vec2(0, 0));
  FWorld.TexVertex(Q[1], Vec2(0, 1));
  FWorld.TexVertex(Q[2], Vec2(1, 1));
  FWorld.TexVertex(Q[3], Vec2(1, 0));
  FWorld.EndQuads;
  FWorld.UnbindTex;
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);
  glEnable(GL_DEPTH_TEST);
end;

function TBackgroudSprite.GetPosition: TVec2Prop;
begin
  Result := (inherited Position).AsVec2;
end;

procedure TBackgroudSprite.SetPosition(const Value: TVec2Prop);
begin
  (inherited Position).AsVec2 := Value;
end;

function TBackgroudSprite.GetAngle: TVec1Prop;
begin
  Result := Rotation.Z;
end;

procedure TBackgroudSprite.SetAngle(const Value: TVec1Prop);
begin
  Rotation.Z := Value;
end;

{ TFontWriter }

const
  FontTexDim = 256;
  FontTexSize = FontTexDim * FontTexDim * SizeOf(TRGBA);

constructor TFontWriter.Create(World: TWorld);
begin
  inherited Create;
  FWorld := World;
  FLines := TStringList.Create;
  FTextures := TTextures.Create(1);
  FShadows := True;
  FOpacity := TVec1Prop.Create;
  FLeadColor := TVec4Prop.Create;
  FBaseColor := TVec4Prop.Create;
  FShadowColor := TVec4Prop.Create;
  FShadowOffset := TVec2Prop.Create;
  Opacity := 1;
  LeadColor := clWhite;
  BaseColor := clSilver;
  ShadowColor := clBlack;
  ShadowOffset := Vec2(1, 1);
end;

destructor TFontWriter.Destroy;
begin
  FTextures.Free;
  FLines.Free;
  inherited Destroy;;
end;

procedure TFontWriter.LoadFromStore(var Store: TFontStore; Bits: Pointer);
begin
  FStore := Store;
  FTextures.Load(FontTexDim, FontTexDim, Bits, 0);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
end;

procedure BitmapTransparent(Bitmap: IBitmap);
var
  RGBA: PRGBA;
  I: Integer;
begin
  RGBA := Bitmap.Pixels;
  I := Bitmap.Width * Bitmap.Height;
  while I > 0 do
  begin
    RGBA.A := RGBA.R;
    RGBA.R := $FF;
    RGBA.G := $FF;
    RGBA.B := $FF;
    Inc(RGBA);
    Dec(I);
  end;
end;

procedure TFontWriter.LoadFromResource(const Resource: string);
var
  Buffer: TBuffer;
  Bits: PByte;
  BitSize: Integer;
  Stream: TMemoryStream;
  Store: TFontStore;
  Bitmap: IBitmap;
begin
  Buffer := Base64Decode(Resource);
  Bits := Buffer.Data;
  BitSize := Buffer.Size;
  if BitSize < SizeOf(Store) then
    Exit;
  Move(Bits^, Store, SizeOf(Store));
  Inc(Bits, SizeOf(Store));
  Dec(BitSize, SizeOf(Store));
  Stream := TMemoryStream.Create(BitSize);
  try
    Move(Bits^, Stream.Memory^, BitSize);
    Bitmap := CreateBitmap(Stream);
    if (Bitmap.Width <> FontTexDim) or (Bitmap.Width <> FontTexDim) then
      Exit;
     BitmapTransparent(Bitmap);
    LoadFromStore(Store, Bitmap.Pixels);
  finally
    Stream.Free;
  end;
end;

procedure TFontWriter.LoadFromStream(Stream: TStream);
var
  Bits: Pointer;

  procedure SaveBits;
  var
    B: IBitmap;
  begin
    B := CreateBitmap($100, $100);
    Move(Bits^, B.Pixels^, $100 * $100 * $04);
    B.SaveToFile('font.png');
  end;

begin
  if Stream.Cancelled then
    Exit;
   Stream.Read(FStore, SizeOf(FStore));
  if Stream.Cancelled then
    Exit;
  GetMem(Bits, FontTexSize);
  try
    Stream.Read(Bits^, FontTexSize);
    if Stream.Cancelled then
      Exit;
    SaveBits;
    FTextures.Load(FontTexDim, FontTexDim, Bits, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  finally
    FreeMem(Bits);
  end;
end;

procedure TFontWriter.LoadFromFile(const FileName: string);
var
  S: TStream;
begin
  if not FileExists(FileName) then
    Exit;
  S := TFileStream.Create(FileName, fmOpen);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

function TFontWriter.Measure(const S: string; Scale: Float): TVec2;
var
  T, W: Float;
  I, J: Integer;
begin
  if (Scale = 0) or (S = '') then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;
  T := 0;
  W := 0;
  J := 1;
  for I := 1 to Length(S) do
    if S[I] = #10 then
    begin
      if T > W then
        W := T;
      Inc(J);
      Continue;
    end
    else if S[I] = #13 then
      Continue
    else if (S[I] < '!') or (S[I] > '}') then
      T := T + FStore.Map[$20].Width * Scale
    else
      T := T + FStore.Map[Ord(S[I])].Width * Scale;
  if T > W then
    Result.X:= T
  else
    Result.X:= W;
  Result.Y := FStore.Map[$20].Height * Scale * J;
end;

procedure TFontWriter.SaveState;
begin
  glGetBooleanv(GL_BLEND, FBlendedState);
  glEnable(GL_BLEND);
  glGetBooleanv(GL_CULL_FACE, FCulledState);
  glDisable(GL_CULL_FACE);
  glGetBooleanv(GL_DEPTH_TEST, FDepthState);
  glDisable(GL_DEPTH_TEST);
  glGetFloatv(GL_CURRENT_COLOR, FColorState.V[0]);
end;

procedure TFontWriter.RestoreState;
begin
  if FCulledState then
    glEnable(GL_CULL_FACE);
  if not FBlendedState then
    glDisable(GL_BLEND);
  if FDepthState then
    glEnable(GL_DEPTH_TEST);
  glColor3fv(FColorState.V[0])
end;

procedure TFontWriter.InternalWrite(const Line: string; Scale, Col, Row: Float);

  procedure Draw(const Lead, Base: TColorF; OX, OY: Float);
  var
    O, K, CX, CY: Float;
    I: Integer;
  begin
    O := Opacity;
    K := (FStore.Kerning * Scale) / 2;
    OX := OX * Scale + Origin.X;
    OY := OY * Scale + Origin.Y;
    CX := Col * FStore.Map[$20].Width * Scale + OX;
    CY := Row * FStore.Map[$20].Height * Scale + OY;
    for I := 1 to Length(Line) do
    begin
      if CY > FWorld.Height then Break;
      if (Line[I] < '!') or (Line[I] > '}') then
      begin
        CX := CX + FStore.Map[$20].Width * Scale;
        Continue;
      end;
      with FStore.Map[Ord(Line[I])] do
      begin
        if CY + Height * Scale < 0 then
          Continue;
        if CY * Scale > FWorld.Height + CY then
          Continue;
        if CX + Width * Scale < 0 then
        begin
          CX := CX + Width * Scale;
          Continue;
        end;
        if CX + Width * Scale > FWorld.Width + CX then
          Continue;
        FWorld.Color(Lead.Red, Lead.Green, Lead.Blue, Lead.Alpha * O);
        FWorld.TexVertex(CX + Width * Scale + K, CY, B.X, A.Y);
        FWorld.TexVertex(CX - K, CY, A.X, A.Y);
        FWorld.Color(Base.Red, Base.Green, Base.Blue, Base.Alpha * O);
        FWorld.TexVertex(CX - K, CY + Height * Scale, A.X, B.Y);
        FWorld.TexVertex(CX + Width * Scale + K, CY + Height * Scale, B.X, B.Y);
        CX := CX + Width * Scale;
      end;
    end;
  end;

begin
  if (Scale = 0) or (Line = '') then Exit;
  if FShadows then
    Draw(ShadowColor, ShadowColor, ShadowOffset.X, ShadowOffset.Y);
  Draw(LeadColor, BaseColor, 0, 0);
end;

procedure TFontWriter.Write(const S: string; Scale, Col, Row: Float);
var
  I: Integer;
begin
  if (Scale = 0) or (S = '') then Exit;
  FLines.Text := S;
  SaveState;
  FWorld.BindTex(FTextures[0]);
  FWorld.BeginQuads;
  for I := 0 to FLines.Count - 1 do
    InternalWrite(FLines[I], Scale, Col, Row + I);
  FWorld.EndQuads;
  FWorld.UnbindTex;
  RestoreState;
end;

procedure TFontWriter.Write(const S: string; Scale, X, Y: Float; Justify: TFontJustify);
var
  CX, CY, Col, Row: Float;
  Line: string;
  Size: TVec2;
  I: Integer;
begin
  if (Scale = 0) or (S = '') then Exit;
  FLines.Text := S;
  SaveState;
  FWorld.BindTex(FTextures[0]);
  FWorld.BeginQuads;
  CX := FStore.Map[$20].Width * Scale;
  CY := FStore.Map[$20].Height * Scale;
  for I := 0 to FLines.Count - 1 do
  begin
    Line := FLines[I];
    Size := Measure(Line, Scale);
    Row := Y - Size.Y / 2 + CY * I;
    case Justify of
      justifyLeft:
        Col := X;
      justifyCenter:
        Col := X - Size.X / 2;
      justifyRight:
        Col := X - Size.X;
    else
      Col := 1;
    end;
    InternalWrite(Line, Scale, Col / CX, Row / CY);
  end;
  FWorld.EndQuads;
  FWorld.UnbindTex;
  RestoreState;
end;

function TFontWriter.GetFace: string;
begin
  Result := FStore.Face;
end;

function TFontWriter.GetKerning: Integer;
begin
  Result := FStore.Kerning;
end;

function TFontWriter.GetSize: Integer;
begin
  Result := FStore.Size;
end;

function TFontWriter.GetTextHeight: Float;
begin
  Result := FStore.Map[$20].Height;
end;

procedure TFontWriter.SetOpacity(const Value: TVec1Prop);
begin
  FOpacity.Value := Value;
end;

procedure TFontWriter.SetOrigin(const Value: TVec2Prop);
begin
  FOrigin.Value := Value.Value;
end;

procedure TFontWriter.SetLeadColor(const Value: TVec4Prop);
begin
  FLeadColor.Value := Value.Value;
end;

procedure TFontWriter.SetBaseColor(const Value: TVec4Prop);
begin
  FBaseColor.Value := Value.Value;
end;

procedure TFontWriter.SetShadowColor(const Value: TVec4Prop);
begin
  FShadowColor.Value := Value.Value;
end;

procedure TFontWriter.SetShadowOffset(const Value: TVec2Prop);
begin
  FShadowOffset.Value := Value.Value;
end;

procedure BitmapOpaque(Bitmap: IBitmap);
var
  RGBA: PRGBA;
  I: Integer;
begin
  RGBA := Bitmap.Pixels;
  I := Bitmap.Width * Bitmap.Height;
  while I > 0 do
  begin
    RGBA.R := RGBA.A;
    RGBA.G := RGBA.A;
    RGBA.B := RGBA.A;
    RGBA.A := $FF;
    Inc(RGBA);
    Dec(I);
  end;
end;

function FontToBitmap(const FileName: string): IBitmap;
var
  Source: TStream;
  Store: TFontStore;
  Bitmap: IBitmap;
begin
  Result := nil;
  Source := TFileStream.Create(FileName, fmOpen);
  try
     if Source.Read(Store, SizeOf(Store)) <> SizeOf(Store) then
      Exit;
    Bitmap := CreateBitmap(FontTexDim, FontTexDim);
    if Source.Read(Bitmap.Pixels^, FontTexSize) <> FontTexSize then
      Exit;
    BitmapOpaque(Bitmap);
    Bitmap.Format := fmPng;
  finally
    Source.Free;
  end;
  Result := Bitmap;
end;

function FontToStr(const FileName: string): string;
const
  ColumnWidth = 80;
var
  Source: TStream;
  Dest, Buffer: TMemoryStream;
  Store: TFontStore;
  Bitmap: IBitmap;
  S: string;
  I: Integer;
begin
  Result := '';
  Source := TFileStream.Create(FileName, fmOpen);
  Dest := TMemoryStream.Create;
  try
     if Source.Read(Store, SizeOf(Store)) <> SizeOf(Store) then
      Exit;
    Bitmap := CreateBitmap(FontTexDim, FontTexDim);
    if Source.Read(Bitmap.Pixels^, FontTexSize) <> FontTexSize then
      Exit;
    BitmapOpaque(Bitmap);
    Bitmap.Format := fmPng;
    Bitmap.SaveToStream(Dest);
    Buffer := TMemoryStream.Create;
    try
      Buffer.Write(Store, SizeOf(Store));
      Buffer.Write(Dest.Memory, Dest.Size);
      S := Base64Encode(Buffer.Memory, Buffer.Size);
    finally
      Buffer.Free;
    end;
    Result := '';
    I := 1;
    while I < Length(S) + 1 do
    begin
      Result := Result + StrCopy(S, I, ColumnWidth) + LineEnding;
      Inc(I, ColumnWidth);
    end;
  finally
    Dest.Free;
    Source.Free;
  end;
end;

procedure FontLoadDefault(Font: TFontWriter);
const
  Resource = {$i resources/font.res};
begin
  Font.LoadFromResource(Resource);
end;

end.

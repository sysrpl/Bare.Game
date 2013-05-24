  (********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.graphics.imaging.linux.txt> }
unit Bare.Graphics.Imaging.Linux;

{$i bare.inc}

interface

uses
  Bare.System,
  Bare.Types,
  Bare.Graphics.Imaging;

{$ifdef linux}
type
  TBitmap = class(TInterfacedObject, IBitmap)
  private
    FBuffer: Pointer;
    FFormat: TImageFormat;
    procedure SetBuffer(Value: Pointer);
  public
    constructor Create(B: Pointer = nil);
    destructor Destroy; override;
    function Clone: IBitmap;
    function GetEmpty: Boolean;
    function GetClientRect: TRectI;
    function GetFormat: TImageFormat;
    procedure SetFormat(Value: TImageFormat);
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixels: PPixel;
    function GetPixel(X, Y: Integer): TPixel;
    procedure SetPixel(X, Y: Integer; const Value: TPixel);
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
    procedure SetSize(Width, Height: Integer);
    property Empty: Boolean read GetEmpty;
    property ClientRect: TRectI read GetClientRect;
    property Format: TImageFormat read GetFormat write SetFormat;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Pixels: PPixel read GetPixels;
  end;
{$endif}

implementation

{$ifdef linux}
  {$define libgobject := external 'libgobject-2.0.so'}
  {$define libgdkpixbuf := external 'libgdk_pixbuf-2.0.so'}
type
  PGChar = PChar;
  PPGChar = ^PGChar;
  GUChar = Byte;
  PGUChar = ^GUChar;
  GUInt32 = LongWord;
  GBoolean = LongBool;
  GSize = SizeUInt;
  GPointer = Pointer;
  PGError = Pointer;
  PPGError = ^PGError;
  PGdkPixbuf = Pointer;
  PGdkPixbufLoader = Pointer;

  GdkPixbufModulePattern = GSize;
  PGdkPixbufModulePattern = ^GdkPixbufModulePattern;

  GdkPixbufFormat = record
    name: PGChar;
    signature: PGdkPixbufModulePattern;
    domain: PGChar;
    description: PGChar;
    mime_types: PPGChar;
    extensions: PPGChar;
    flags: GUInt32;
    disabled: GBoolean;
    license: PGChar;
  end;
  PGdkPixbufFormat = ^GdkPixbufFormat;

  GdkPixbufSaveFunc = function(buffer: PGChar; count: GSize; error: PPGError;
    data: GPointer): GBoolean; cdecl;

  TGdkInterpType = (
    GDK_INTERP_NEAREST,
    GDK_INTERP_TILES,
    GDK_INTERP_BILINEAR,
    GDK_INTERP_HYPER
  );

  TGdkColorspace = (GDK_COLORSPACE_RGB);

procedure g_type_init; cdecl; libgobject;
function g_object_ref(obj: GPointer): GPointer; cdecl; libgobject;
procedure g_object_unref(obj: GPointer); cdecl; libgobject;
function gdk_pixbuf_new(colorspace: TGdkColorspace; has_alpha: GBoolean; bits_per_sample, width, height: LongInt): PGdkPixbuf; cdecl; libgdkpixbuf;
function gdk_pixbuf_new_from_file(filename: PChar; error: PPGError): PGdkPixbuf; cdecl; libgdkpixbuf;
function gdk_pixbuf_get_has_alpha(pixbuf: PGdkPixbuf): GBoolean; cdecl; libgdkpixbuf;
function gdk_pixbuf_add_alpha(pixbuf: PGdkPixbuf; substitute_color: GBoolean; r, g, b: GUChar): PGdkPixbuf; cdecl; libgdkpixbuf;
function gdk_pixbuf_get_pixels(pixbuf: PGdkPixbuf): PGUchar; cdecl; libgdkpixbuf;
function gdk_pixbuf_get_width(pixbuf: PGdkPixbuf): LongInt; cdecl; libgdkpixbuf;
function gdk_pixbuf_get_height(pixbuf: PGdkPixbuf): LongInt; cdecl; libgdkpixbuf;
function gdk_pixbuf_get_rowstride(pixbuf: PGdkPixbuf): LongInt; cdecl; libgdkpixbuf;
function gdk_pixbuf_copy(pixbuf: PGdkPixbuf): PGdkPixbuf; cdecl; libgdkpixbuf;
function gdk_pixbuf_scale_simple(src: PGdkPixbuf; dest_width, dest_height: LongInt; interp_type: TGdkInterpType): PGdkPixbuf; cdecl; libgdkpixbuf;
function gdk_pixbuf_loader_new: PGdkPixbufLoader; cdecl; libgdkpixbuf;
function gdk_pixbuf_loader_write(loader: PGdkPixbufLoader; buf: PGUChar; count: GSize; error: PPGError): GBoolean; cdecl; libgdkpixbuf;
function gdk_pixbuf_loader_get_pixbuf(loader: PGdkPixbufLoader): PGdkPixbuf; cdecl; libgdkpixbuf;
function gdk_pixbuf_loader_close(loader:PGdkPixbufLoader; error: PPGError): GBoolean; cdecl; libgdkpixbuf;
function gdk_pixbuf_loader_get_format(loader: PGdkPixbufLoader): PGdkPixbufFormat; cdecl; libgdkpixbuf;
function gdk_pixbuf_save(pixbuf: PGdkPixbuf; filename, _type: PGChar; error: PPGError): GBoolean; cdecl; libgdkpixbuf;
function gdk_pixbuf_save_to_callback(pixbuf: PGdkPixbuf; save_func: GdkPixbufSaveFunc; data: GPointer; _type: PGChar; error: PPGError): GBoolean; cdecl; libgdkpixbuf;

{ TBitmap }

constructor TBitmap.Create(B: PGdkPixbuf = nil);
const
  Initialized: Boolean = False;
begin
  if not Initialized then
  begin
    g_type_init;
    Initialized := True;
  end;
  inherited Create;
  SetBuffer(B);
end;

destructor TBitmap.Destroy;
begin
  SetBuffer(nil);
  inherited Destroy;
end;

procedure TBitmap.SetBuffer(Value: Pointer);
begin
  if Value = FBuffer then
    Exit;
  if FBuffer <> nil then
    g_object_unref(FBuffer);
  FBuffer := Value;
end;

function TBitmap.Clone: IBitmap;
begin
  if FBuffer = nil then
    Result := TBitmap.Create
  else
    Result := TBitmap.Create(gdk_pixbuf_copy(FBuffer));
  (Result as TBitmap).FFormat := FFormat;
end;

function TBitmap.GetEmpty: Boolean;
begin
  Result := FBuffer = nil;
end;

function TBitmap.GetClientRect: TRectI;
begin
  Result := TRectI.Create(Width, Height);
end;

function TBitmap.GetFormat: TImageFormat;
begin
  Result := FFormat;
end;

procedure TBitmap.SetFormat(Value: TImageFormat);
begin
  FFormat := Value;
end;

function TBitmap.GetHeight: Integer;
begin
  if FBuffer = nil then
    Result := 0
  else
    Result := gdk_pixbuf_get_height(FBuffer);
end;

function TBitmap.GetWidth: Integer;
begin
  if FBuffer = nil then
    Result := 0
  else
    Result := gdk_pixbuf_get_width(FBuffer);
end;

function TBitmap.GetPixels: PPixel;
begin
  if FBuffer = nil then
    Result := nil
  else
    Result := Pointer(gdk_pixbuf_get_pixels(FBuffer));
end;

function TBitmap.GetPixel(X, Y: Integer): TPixel;
const
  Transparent: TPixel = ();
var
  W, H: Integer;
  P: PPixel;
begin
  W := Width;
  H := Height;
  P := GetPixels;
  if (P = nil) or (X < 0) or (Y < 0) or (X >= W) or (Y >= H) then
    Exit(Transparent);
  Inc(P, X + Y * W);
  Result := P^;
end;

procedure TBitmap.SetPixel(X, Y: Integer; const Value: TPixel);
var
  W, H: Integer;
  P: PPixel;
begin
  W := Width;
  H := Height;
  P := GetPixels;
  if (P = nil) or (X < 0) or (Y < 0) or (X >= W) or (Y >= H) then
    Exit;
  Inc(P, X + Y * W);
  P^ := Value;
end;

procedure TBitmap.Clear;
begin
  SetBuffer(nil);
end;

function TBitmap.Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
const
  Sampling: array[TResampleQuality] of TGdkInterpType =
    (GDK_INTERP_NEAREST, GDK_INTERP_BILINEAR, GDK_INTERP_HYPER);
var
  B: PGdkPixbuf;
begin
  if Empty then
    Exit(nil);
  B := gdk_pixbuf_scale_simple(FBuffer, Width, Height, Sampling[Quality]);
  if B = nil then
    Exit(nil);
  Result := TBitmap.Create(B);
end;

procedure TBitmap.SetSize(Width, Height: Integer);
var
  B: PGdkPixbuf;
begin
  if (Width = GetWidth) and (Height = GetHeight) then
    Exit;
  if (Width < 1) or (Height < 1) then
    B := nil
  else if FBuffer = nil then
    B := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, Width, Height)
  else if (Width <> GetWidth) or (Height <> GetHeight) then
    B := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, Width, Height)
  else
    Exit;
  SetBuffer(B);
end;

procedure TBitmap.LoadFromFile(const FileName: string);
var
  B, C: PGdkPixbuf;
begin
  B := gdk_pixbuf_new_from_file(PChar(FileName), nil);
  if B <> nil then
  begin
    FFormat := StrToImageFormat(FileExtractExt(FileName));
    if gdk_pixbuf_get_has_alpha(B) then
      SetBuffer(B)
    else
    begin
      C := gdk_pixbuf_add_alpha(B, False, 0, 0, 0);
      g_object_unref(B);
      SetBuffer(C);
    end;
  end;
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
const
  DataSize = 1024 * 16;
var
  Loader: PGdkPixbufLoader;
  Data: PByte;
  B, C: PGdkPixbuf;
  F: PGdkPixbufFormat;
  I: LongInt;
begin
  if Stream.Cancelled then
    Exit;
  Loader := gdk_pixbuf_loader_new;
  Data := GetMem(DataSize);
  try
    repeat
      I := Stream.Read(Data^, DataSize);
      if Stream.Cancelled then
        Exit;
      if not gdk_pixbuf_loader_write(Loader, Data, I, nil) then
        Exit;
    until I < DataSize;
    if not gdk_pixbuf_loader_close(Loader, nil) then
      Exit;
    B := gdk_pixbuf_loader_get_pixbuf(Loader);
    if B <> nil then
    begin
      g_object_ref(B);
      if gdk_pixbuf_get_has_alpha(B) then
        SetBuffer(B)
      else
      begin
        C := gdk_pixbuf_add_alpha(B, False, 0, 0, 0);
        g_object_unref(B);
        SetBuffer(C);
      end;
      F := gdk_pixbuf_loader_get_format(Loader);
      if F <> nil then
        FFormat := StrToImageFormat(F.name);
    end;
  finally
    FreeMem(Data);
    g_object_unref(Loader);
  end;
end;

procedure TBitmap.SaveToFile(const FileName: string);
var
  S: string;
begin
  if not Empty then
  begin
    S := FileExtractExt(FileName);
    FFormat := StrToImageFormat(S);
    S := ImageFormatToStr(FFormat);
    gdk_pixbuf_save(FBuffer, PChar(FileName), PChar(S), nil);
  end;
end;

function SaveCallback(buffer: PGChar; count: GSize; error: PPGError;
  data: GPointer): GBoolean; cdecl;
var
  Stream: TStream absolute data;
begin
  Stream.Write(buffer^, count);
  if Stream.Cancelled then
    Result := not Stream.Cancelled;
end;

procedure TBitmap.SaveToStream(Stream: TStream);
var
  S: string;
begin
  if not Empty then
  begin
    S := ImageFormatToStr(FFormat);
    gdk_pixbuf_save_to_callback(FBuffer, SaveCallback, Stream, PChar(S), nil);
  end;
end;
{$endif}

end.

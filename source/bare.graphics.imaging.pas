(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.graphics.imaging.txt> }
unit Bare.Graphics.Imaging;

{$i bare.inc}

interface

uses
  Bare.System,
  Bare.Types;

type
  { The format used when saving a bitmap to a stream }
  TImageFormat = (fmPng, fmJpeg, fmGif, fmBmp, fmIco, fmTiff);
  { The quality used when resizing a bitmap }
  TResampleQuality = (rqLowest, rqNormal, rqBest);

  {doc ignore}
  IBitmap = interface;

{ IBitmap provides access to cross platform image manipulation
  See also
  <link Overview.Bare.Graphics.Imaging.IBitmap, IBitmap members> }

  IBitmap = interface(ICloneable<IBitmap>)
  ['{DB935633-A218-4181-96A2-B0808697150F}']
    { Returns true if the bitmap has no dimensions }
    function GetEmpty: Boolean;
    { Get the bitmap bounds }
    function GetClientRect: TRectI;
    { Get the current format }
    function GetFormat: TImageFormat;
    { Set the current format }
    procedure SetFormat(Value: TImageFormat);
    { Get the bitmap height in pixels }
    function GetHeight: Integer;
    { Get the bitmap width in pixels }
    function GetWidth: Integer;
    { Get a pointer to the pixel data }
    function GetPixels: PPixel;
    { Reset the bitmap to empty }
    procedure Clear;
    { Create a resized copy of the bitmap of width and height dimensions using a reasmple quality }
    function Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
    { Load the bitmap from a file }
    procedure LoadFromFile(const FileName: string);
    { Save the bitmap to a file }
    procedure SaveToFile(const FileName: string);
    { Load the bitmap from a stream }
    procedure LoadFromStream(Stream: TStream);
    { Save the bitmap to a stream }
    procedure SaveToStream(Stream: TStream);
    { Set the dimensions of a bitmap }
    procedure SetSize(Width, Height: Integer);
    { Returns true if the bitmap has no dimensions }
    property Empty: Boolean read GetEmpty;
    { The bitmap bounds }
    property ClientRect: TRectI read GetClientRect;
    { The image format used while saving to a stream }
    property Format: TImageFormat read GetFormat write SetFormat;
    { The width of the bitmap }
    property Width: Integer read GetWidth;
    { The height of the bitmap }
    property Height: Integer read GetHeight;
    { A pointer to the pixel data }
    property Pixels: PPixel read GetPixels;
  end;

{ Create an empty bitmap }
function CreateBitmap: IBitmap; overload;
{ Create a bitmap of width and height dimensions }
function CreateBitmap(Width, Height: Integer): IBitmap; overload;
{ Create a bitmap from a file }
function CreateBitmap(const FileName: string): IBitmap; overload;
{ Create a bitmap from a stream }
function CreateBitmap(Stream: TStream): IBitmap; overload;

{ Convert a file extension to a TImageFormat }
function StrToImageFormat(S: string): TImageFormat;
{ Convert a TImageFormat to a file extension }
function ImageFormatToStr(F: TImageFormat): string;
{ Convert a TImageFormat to a mimetype }
function ImageFormatToMimeType(F: TImageFormat): string;

implementation

uses
{$ifdef linux}
  Bare.Graphics.Imaging.Linux;
{$endif}
{$ifdef windows}
  Bare.Graphics.Imaging.Windows;
{$endif}

function CreateBitmap: IBitmap;
begin
  Result := TBitmap.Create;
end;

function CreateBitmap(Width, Height: Integer): IBitmap;
begin
  Result := TBitmap.Create;
  Result.SetSize(Width, Height);
end;

function CreateBitmap(const FileName: string): IBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromFile(FileName);
end;

function CreateBitmap(Stream: TStream): IBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromStream(Stream);
end;

{ Image format routines }

function StrToImageFormat(S: string): TImageFormat;
begin
  S := StrLower(StrTrim(S));
  if S = '' then
    Exit(fmPng);
  if S[1] = '.' then
    S := StrCopy(S, 2, Length(S) - 1);
  if S = 'jpeg' then
    Exit(fmJpeg);
  if S = 'jpg' then
    Exit(fmJpeg);
  if S = 'gif' then
    Exit(fmGif);
  if S = 'bmp' then
    Exit(fmBmp);
  if S = 'bmp' then
    Exit(fmBmp);
  if S = 'ico' then
    Exit(fmIco);
  if S = 'tif' then
    Exit(fmTiff);
  if S = 'tiff' then
    Exit(fmTiff);
  Result := fmPng;
end;

function ImageFormatToStr(F: TImageFormat): string;
const
  Formats: array[TImageFormat] of string =
    ('png', 'jpeg', 'gif', 'bmp', 'ico', 'tiff');
begin
  Result := Formats[F];
end;

function ImageFormatToMimeType(F: TImageFormat): string;
begin
  Result := 'image/' + ImageFormatToStr(F);
end;

end.


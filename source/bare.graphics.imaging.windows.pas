(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.graphics.imaging.windows.txt> }
unit Bare.Graphics.Imaging.Windows;

{$i bare.inc}

interface

{$ifdef windows}
uses
  Bare.System,
  Bare.Types,
  Bare.Graphics.Imaging;

type
  TBitmap = class(TInterfacedObject, IBitmap)
  private
    FHandle: Pointer;
    FFormat: TImageFormat;
    FMemory: Pointer;
    FBits: Pointer;
    procedure Lock;
    procedure Unlock;
    procedure Swap;
    procedure SetHandle(Value: Pointer);
  public
    constructor Create(H: Pointer = nil; M: Pointer = nil);
    destructor Destroy; override;
    function Clone: IBitmap;
    function GetEmpty: Boolean;
    function GetClientRect: TRectI;
    function GetFormat: TImageFormat;
    procedure SetFormat(Value: TImageFormat);
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPixels: PPixel;
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

{$ifdef windows}
  {$define gdiplus := external 'gdiplus.dll'}
uses
  Bare.Interop.Windows;

type
  IStreamClear = interface(IStream)
  ['{7CC46FED-4382-48C8-B483-EE80B9806068}']
    function Clear: HResult; stdcall;
  end;

  TStreamAdapter = class(TInterfacedObject, ISequentialStream, IStream, IStreamClear)
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream);
    { ISequentialStream }
    function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult; stdcall;
    function Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult; stdcall;
    { IStream }
    function Seek(dlibMove: LargeInt; dwOrigin: LongInt; libNewPosition: PLargeInt): HResult; stdcall;
    function SetSize(libNewSize: LargeInt): HResult; stdcall;
    function CopyTo(stm: IStream; cb: LargeInt; out cbRead: LargeInt; out cbWritten: LargeInt): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: LargeInt; cb: LargeInt; dwLockType: LongInt): HResult; stdcall;
    function UnlockRegion(libOffset: LargeInt; cb: LargeInt; dwLockType: LongInt): HResult; stdcall;
    function Stat(statstg: PStatStg; grfStatFlag: Longint): HResult; stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
    { IClearStream }
    function Clear: HResult; stdcall;
  end;

constructor TStreamAdapter.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

{ ISequentialStream }

function TStreamAdapter.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult;
var
  I: LargeWord;
begin
  if FStream = nil then
    Exit(S_FALSE);
  I := FStream.Read(pv^, cb);
  if pcbRead <> nil then
    pcbRead^ := I;
  Result := S_OK;
end;

function TStreamAdapter.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult;
var
  I: LargeWord;
begin
  if FStream = nil then
    Exit(S_FALSE);
  I := FStream.Write(pv^, cb);
  if pcbWritten <> nil then
    pcbWritten^ := I;
  Result := S_OK;
end;

{ IStream }

function TStreamAdapter.Seek(dlibMove: LargeInt; dwOrigin: LongInt; libNewPosition: PLargeInt): HResult;
var
  I: LargeInt;
begin
  if FStream = nil then
    Exit(S_FALSE);
  case dwOrigin of
    0: I := FStream.Seek(dlibMove, soBegin);
    1: I := FStream.Seek(dlibMove, soCurrent);
    2: I := FStream.Seek(dlibMove, soEnd);
  else
    Exit(STG_E_INVALIDFUNCTION);
  end;
  if libNewPosition <> nil then
    libNewPosition^ := I;
  Result := S_OK;
end;

function TStreamAdapter.SetSize(libNewSize: LargeInt): HResult;
begin
  if FStream = nil then
    Exit(S_FALSE);
  FStream.Size := libNewSize;
  if FStream.Size = libNewSize then
    Result := S_FALSE
  else
    Result := S_OK;
end;

function TStreamAdapter.CopyTo(stm: IStream; cb: LargeInt; out cbRead: LargeInt; out cbWritten: LargeInt): HResult;
const
  BufferSize = 4096;
var
  Buffer: Pointer;
  R, W: LongWord;
begin
  if FStream = nil then
    Exit(S_FALSE);
  if stm = nil then
    Exit(S_FALSE);
  Result := S_OK;
  cbRead := 0;
  cbWritten := 0;
  if cb < 0 then
    Exit;
  GetMem(Buffer, BufferSize);
  try
    while cb > 0 do
    begin
      if cb > BufferSize then
        R := FStream.Read(Buffer^, BufferSize)
      else
        R := FStream.Read(Buffer^, cb);
      if R = 0 then
        Exit;
      cbRead := cbRead + R;
      stm.Write(Buffer, R, @W);
      cbWritten := cbWritten + W;
      if cb > BufferSize then
        cb := cb - BufferSize
      else
        cb := 0;
    end;
  finally
    FreeMem(Buffer);
  end;
  Result := S_OK;
end;

function TStreamAdapter.Commit(grfCommitFlags: Longint): HResult;
begin
  Result := S_OK;
end;

function TStreamAdapter.Revert: HResult;
begin
  Result := STG_E_REVERTED;
end;

function TStreamAdapter.LockRegion(libOffset: LargeInt; cb: LargeInt; dwLockType: LongInt): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TStreamAdapter.UnlockRegion(libOffset: LargeInt; cb: LargeInt; dwLockType: LongInt): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TStreamAdapter.Stat(statstg: PStatStg; grfStatFlag: Longint): HResult;
begin
  if FStream = nil then
    Exit(S_FALSE);
  if statstg <> nil then
  begin
    statstg.pwcsName := nil;
    statstg.dwType := STGTY_STREAM;
    statstg.cbSize := FStream.Size;
    statstg.mTime.dwLowDateTime := 0;
    statstg.mTime.dwHighDateTime := 0;
    statstg.cTime.dwLowDateTime := 0;
    statstg.cTime.dwHighDateTime := 0;
    statstg.aTime.dwLowDateTime := 0;
    statstg.aTime.dwHighDateTime := 0;
    statstg.grfLocksSupported := LOCK_WRITE;
  end;
  Result := S_OK;
end;

function TStreamAdapter.Clone(out stm: IStream): HResult;
begin
  Result := E_NOTIMPL;
end;

function TStreamAdapter.Clear: HResult;
begin
  FStream := nil;
  Result :=  S_OK;
end;

type
  BOOL = LongBool;
  UInt = LongWord;
  ULONG = UIntPtr;
  GpImage = Pointer;
  GpBitmap = Pointer;
  GpGraphics = Pointer;

  TStatus = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiPlusVersion,
    GdiPlusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported);

  TPixelFormat = Integer;

const
  PixelFormatGDI = $00020000;
  PixelFormatAlpha = $00040000;
  PixelFormatCanonical = $00200000;
  PixelFormat32bppARGB = (10 or (32 shl 8) or PixelFormatGDI or PixelFormatAlpha or PixelFormatCanonical);

  FlushIntentionSync = 1;

  QualityModeDefault = 0;
  QualityModeLow = 1;
  QualityModeHigh = 2;

  CompositingModeSourceOver = 0;

type
  TImageCodecInfo = packed record
    Clsid: TGuid;
    FormatID: TGuid;
    CodecName: PWideChar;
    DllName: PWideChar;
    FormatDescription: PWideChar;
    FilenameExtension: PWideChar;
    MimeType: PWideChar;
    Flags: DWORD;
    Version: DWORD;
    SigCount: DWORD;
    SigSize: DWORD;
    SigPattern: PByte;
    SigMask: PByte;
  end;
  PImageCodecInfo = ^TImageCodecInfo;

  TBitmapData = packed record
    Width: UInt;
    Height: UInt;
    Stride: Integer;
    PixelFormat: TPixelFormat;
    Scan0: Pointer;
    Reserved: UInt;
  end;
  PBitmapData = ^TBitmapData;

  TGdiRectI = packed record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TDebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning);

  TDebugEventProc = procedure(level: tDebugEventLevel; message: PChar); stdcall;

  TGdiPlusStartupInput = packed record
    GdiPlusVersion: LongWord;
    DebugEventCallback: TDebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs: BOOL;
  end;
  PGdiPlusStartupInput = ^TGdiPlusStartupInput;

function GdiplusStartup(out token: ULONG; input: PGdiPlusStartupInput; output: Pointer): TStatus; stdcall; gdiplus;
function GdipAlloc(size: ULONG): Pointer; stdcall; gdiplus;
procedure GdipFree(ptr: Pointer); stdcall; gdiplus;
function GdipDisposeImage(image: GpImage): TStatus; stdcall; gdiplus;
function GdipGetImageWidth(image: GpImage; out width: UInt): TStatus; stdcall; gdiplus;
function GdipGetImageHeight(image: GpImage; out height: UInt): TStatus; stdcall; gdiplus;
function GdipGetImagePixelFormat(image: GpImage; out format: TPixelFormat): TStatus; stdcall; gdiplus;
function GdipCreateBitmapFromScan0(width: Integer; height: Integer; stride: Integer; format: TPixelFormat; scan0: PByte; out bitmap: GpBitmap): TStatus; stdcall; gdiplus;
function GdipCreateBitmapFromStream(stream: IStream; out bitmap: GpBitmap): TStatus; stdcall; gdiplus;
function GdipCreateBitmapFromFile(filename: PWideChar; out bitmap: GpBitmap): TStatus; stdcall; gdiplus;
function GdipGetImageEncodersSize(out numEncoders: UInt; out size: UInt): TStatus; stdcall; gdiplus;
function GdipGetImageEncoders(numEncoders: UInt; size: UInt; encoders: PImageCodecInfo): TStatus; stdcall; gdiplus;
function GdipSaveImageToFile(image: GpImage; filename: PWideChar; var clsidEncoder: TGuid; encoderParams: Pointer): TStatus; stdcall; gdiplus;
function GdipSaveImageToStream(image: GpImage; stream: IStream; var clsidEncoder: TGuid; encoderParams: Pointer): TStatus; stdcall; gdiplus;
function GdipBitmapLockBits(bitmap: GpBitmap; constref rect: TGdiRectI; flags: UInt; format: TPixelFormat; lockedBitmapData: PBitmapData): TStatus; stdcall; gdiplus;
function GdipBitmapUnlockBits(bitmap: GpBitmap; lockedBitmapData: PBitmapData): TStatus; stdcall; gdiplus;
function GdipCloneImage(image: GpImage; out cloneImage: GpImage): TStatus; stdcall; gdiplus;
function GdipGetImageGraphicsContext(image: GpImage; out graphics: GpGraphics): TStatus; stdcall; gdiplus;
function GdipDeleteGraphics(graphics: GpGraphics): TStatus; stdcall; gdiplus;
function GdipGraphicsClear(graphics: GpGraphics; Color: DWORD): TStatus; stdcall; gdiplus;
function GdipSetInterpolationMode(graphics: GpGraphics; interpolationMode: Integer): TStatus; stdcall; gdiplus;
function GdipSetCompositingMode(graphics: GpGraphics; compositingMode: Integer): TStatus; stdcall; gdiplus;
function GdipSetPixelOffsetMode(graphics: GpGraphics; pixelOffsetMode: Integer): TStatus; stdcall; gdiplus;
function GdipDrawImageRectI(graphics: GpGraphics; image: GpImage; x, y, width, height: Integer): TStatus; stdcall; gdiplus;
function GdipFlush(graphics: GpGraphics; intention: Integer): TStatus; stdcall; gdiplus;

var
  StartupInput: TGdiPlusStartupInput;
  GdiPlusToken: UIntPtr;

procedure GdipLoad;
var
  P: Pointer;
begin
  P := GdipAlloc(SizeOf(Pointer));
  if P = nil then
  begin
    StartupInput.DebugEventCallback := nil;
    StartupInput.SuppressBackgroundThread := False;
    StartupInput.SuppressExternalCodecs := False;
    StartupInput.GdiplusVersion := 1;
    GdiplusStartup(GdiPlusToken, @StartupInput, nil);
  end
  else
    GdipFree(P);
end;

{ TBitmap }

constructor TBitmap.Create(H: Pointer = nil; M: Pointer = nil);
begin
  inherited Create;
  GdipLoad;
  GetMem(FBits, SizeOf(TBitmapData));
  FillChar(FBits^, SizeOf(TBitmapData), 0);
  SetHandle(H);
  FMemory := M;
end;

destructor TBitmap.Destroy;
begin
  inherited Destroy;
  SetHandle(nil);
  FreeMem(FBits);
end;

procedure TBitmap.SetHandle(Value: GpBitmap);
begin
  if Value = FHandle then
    Exit;
  Unlock;
  if FMemory <> nil then
  begin
    FreeMem(FMemory);
    FMemory := nil;
  end;
  if FHandle <> nil then
    GdipDisposeImage(FHandle);
  FHandle := Value;
  Lock;
end;

procedure TBitmap.Lock;
const
  LockFlags = 3;
var
  R: TGdiRectI;
  B: PBitmapData;
begin
  if FHandle = nil then
    Exit;
  R.X:= 0;
  R.Y := 0;
  R.Width := GetWidth;
  R.Height := GetHeight;
  B := FBits;
  B.Width := R.Width;
  B.Height := R.Height;
  B.Stride := B.Width * B.Height * 4;
  B.PixelFormat := PixelFormat32bppARGB;
  B.Scan0 := nil;
  if GdipBitmapLockBits(FHandle, R, LockFlags, PixelFormat32bppARGB, FBits) < Ok then
    B.Scan0 := nil;
end;

procedure TBitmap.Unlock;
begin
  if (FHandle = nil) or (PBitmapData(FBits).Scan0 = nil) then
    Exit;
  GdipBitmapUnlockBits(FHandle, FBits);
  PBitmapData(FBits).Scan0 := nil;
end;

procedure TBitmap.Swap;
var
  Pixel: PPixel;
  X: Byte;
  I: LongWord;
begin
  Pixel := Pixels;
  if Pixel = nil then
    Exit;
  I := PBitmapData(FBits).Width * PBitmapData(FBits).Height;
  while I > 0 do
  begin
    X := Pixel.Red;
    Pixel.Red := Pixel.Blue;
    Pixel.Blue := X;
    Inc(Pixel);
    Dec(I);
  end;
end;

function TBitmap.Clone: IBitmap;
var
  B: GpBitmap;
begin
  B := nil;
  if FHandle <> nil then
  begin
    Unlock;
    GdipCloneImage(FHandle, B);
    Lock;
  end;
  Result := TBitmap.Create(B);
end;

function TBitmap.GetEmpty: Boolean;
begin
  Result := FHandle = nil;
end;

function TBitmap.GetClientRect: TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := GetWidth;
  Result.Height := GetHeight;
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
var
  H: UInt;
begin
  if FHandle = nil then
    Exit(0);
  if GdipGetImageHeight(FHandle, H) = Ok then
    Result := H
  else
    Result := 0;
end;

function TBitmap.GetWidth: Integer;
var
  W: UInt;
begin
  if FHandle = nil then
    Exit(0);
  if GdipGetImageWidth(FHandle, W) = Ok then
    Result := W
  else
    Result := 0;
end;

function TBitmap.GetPixels: PPixel;
begin
  if FHandle = nil then
    Result := nil
  else
    Result := PPixel(PBitmapData(FBits).Scan0);
end;

procedure TBitmap.Clear;
begin
  SetHandle(nil);
end;

procedure ForcePixelFormat(Bitmap: TBitmap);
var
  P: TPixelFormat;
  B: IBitmap;
  S: TBitmap;
  I: Pointer;
begin
  GdipGetImagePixelFormat(Bitmap.FHandle, P);
  if P <> PixelFormat32bppARGB then
  begin
    B := Bitmap.Resample(Bitmap.Width, Bitmap.Height);
    S := B as TBitmap;
    I := Bitmap.FHandle;
    Bitmap.FHandle := S.FHandle;
    S.FHandle := I;
    I := Bitmap.FMemory;
    Bitmap.FMemory := S.FMemory;
    S.FMemory := I;
    I := Bitmap.FBits;
    Bitmap.FBits := S.FBits;
    S.FBits := I;
  end;
end;

procedure TBitmap.LoadFromFile(const FileName: string);
var
  F: WideString;
  H: GpBitmap;
begin
  FFormat := StrToImageFormat(FileExtractExt(FileName));
  F := FileName;
  if GdipCreateBitmapFromFile(PWideChar(F), H) = Ok then
  begin
    SetHandle(H);
    ForcePixelFormat(Self);
    Swap;
  end
  else
    SetHandle(nil);
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
var
  Adapter: IStreamClear;
  H: GpBitmap;
begin
  Adapter := TStreamAdapter.Create(Stream);
  if GdipCreateBitmapFromStream(Adapter, H) = Ok then
  begin
    SetHandle(H);
    ForcePixelFormat(Self);
    Swap;
  end
  else
    SetHandle(nil);
  Adapter.Clear;
end;

function GetEncoderClsid(const Format: WideString; out Clsid: TGuid): Boolean;
var
  Num, Size: UInt;
  Codecs, Item: PImageCodecInfo;
  MimeType: WideString;
  I: Integer;
begin
  Result := False;
  GdipGetImageEncodersSize(Num, Size);
  GetMem(Codecs, Size);
  GdipGetImageEncoders(Num, Size, Codecs);
  Item := Codecs;
  for I := 0 to Num - 1 do
  begin
    MimeType := Item.MimeType;
    if Format = MimeType then
    begin
      Clsid := Item.Clsid;
      Result := True;
      Break;
    end;
    Inc(Item);
  end;
  FreeMem(Codecs);
end;

procedure TBitmap.SaveToFile(const FileName: string);
var
  Clsid: TGuid;
  S: string;
  W: WideString;
begin
  if FHandle <> nil then
  begin
    Swap;
    Unlock;
    try
      S := FileExtractExt(FileName);
      FFormat := StrToImageFormat(S);
      W := ImageFormatToMimeType(FFormat);
      if GetEncoderClsid(W, Clsid) then
      begin
        W := FileName;
        GdipSaveImageToFile(FHandle, PWideChar(W), Clsid, nil);
      end;
    finally
      Lock;
    end;
    Swap;
  end;
end;

procedure TBitmap.SaveToStream(Stream: TStream);
var
  Adapter: IStreamClear;
  Clsid: TGuid;
  W: WideString;
begin
  if FHandle <> nil then
  begin
    Swap;
    Unlock;
    try
      W := ImageFormatToMimeType(FFormat);
      if GetEncoderClsid(W, Clsid) then
      begin
        Adapter := TStreamAdapter.Create(Stream);
        GdipSaveImageToStream(FHandle, Adapter, Clsid, nil);
        Adapter.Clear;
      end;
    finally
      Lock;
    end;
    Swap;
  end;
end;

function CreateBitmap(Width, Height: Integer; out Memory: Pointer): GpBitmap;
var
  S: Integer;
  B: GpBitmap;
begin
  Memory := nil;
  Result := nil;
  S := Width * Height * SizeOf(TPixel);
  GetMem(Memory, S);
  FillChar(Memory^, S, #0);
  if GdipCreateBitmapFromScan0(Width, Height, Width * SizeOf(TPixel),
      PixelFormat32bppARGB, PByte(Memory), B) = Ok then
      Result := B
  else
  begin
    FreeMem(Memory);
    Memory := nil;
  end;
end;

const
  MaxSize = 4096;

function TBitmap.Resample(Width, Height: Integer; Quality: TResampleQuality = rqNormal): IBitmap;
const
  QualityValues: array[TResampleQuality] of Integer =
    (QualityModeLow, QualityModeDefault, QualityModeHigh);
var
  M: Pointer;
  B: GpBitmap;
  G: GpGraphics;
begin
  if FHandle = nil then
    Exit(TBitmap.Create);
  if (Width < 1) or (Height < 1) then
    Exit(TBitmap.Create);
  if (Width > MaxSize) or (Height > MaxSize) then
    Exit(TBitmap.Create);
  Unlock;
  try
    B := CreateBitmap(Width, Height, M);
    if M = nil then
      Exit(TBitmap.Create);
    GdipGetImageGraphicsContext(B, G);
    GdipSetCompositingMode(G, CompositingModeSourceOver);
    GdipSetInterpolationMode(G, QualityValues[Quality]);
    GdipSetPixelOffsetMode(G, QualityModeHigh);
    GdipDrawImageRectI(G, FHandle, 0, 0, Width, Height);
    GdipFlush(G, FlushIntentionSync);
    GdipDeleteGraphics(G);
  finally
    Lock;
  end;
  Result := TBitmap.Create(B, M);
end;

procedure TBitmap.SetSize(Width, Height: Integer);
var
  M: Pointer;
  B: GpBitmap;
begin
  if (FHandle <> nil) and (Width = GetWidth) and (Height = GetHeight) then
    Exit;
  SetHandle(nil);
  if (Width < 1) or (Height < 1) then
    Exit;
  if (Width > MaxSize) or (Height > MaxSize) then
    Exit;
  B := CreateBitmap(Width, Height, M);
  SetHandle(B);
  FMemory := M;
end;
{$endif}

end.

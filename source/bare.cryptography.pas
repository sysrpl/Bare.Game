(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.cryptography.txt> }
unit Bare.Cryptography;

{$i bare.inc}

interface

uses
  Bare.System,
  Bare.Interop.OpenSSL,
  Bare.Text;

type
  THashKind = (hkMD5, hkSHA1, hkSHA256, hkSHA512);

  TDigest = TBuffer;

{ Hashing routines }

function HashString(Kind: THashKind; const S: string): TDigest;
function HashBuffer(Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest; overload;
function HashStream(Kind: THashKind; Stream: TStream): TDigest;
function HashFile(Kind: THashKind; const FileName: string): TDigest;

{ Hash message authentication code routines HMAC }

function AuthString(const Key: string; Kind: THashKind; const S: string): TDigest;
function AuthBuffer(const Key: string; Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
function AuthStream(const Key: string; Kind: THashKind; Stream: TStream): TDigest;
function AuthFile(const Key: string; Kind: THashKind; const FileName: string): TDigest;

implementation

type
  THashMethods = record
    Context: array[0..500] of Byte;
    Digest: TDigest;
    Init: function(var Context): LongBool; cdecl;
    Update: function(var Context; Data: Pointer; Size: Cardinal): LongBool; cdecl;
    Final: function(var Digest; var Context): LongBool; cdecl;
  end;

  TAuthMethod = record
    Digest: TDigest;
    Method:  TEVPMethod;
  end;

function GetHashMethods(Kind: THashKind; out Methods: THashMethods): Boolean;
begin
  Result := True;
  case Kind of
    hkMD5:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TMD5Digest));
        Methods.Init := @MD5_Init;
        Methods.Update := @MD5_Update;
        Methods.Final := @MD5_Final;
      end;
    hkSHA1:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TSHA1Digest));
        Methods.Init := @SHA1_Init;
        Methods.Update := @SHA1_Update;
        Methods.Final := @SHA1_Final;
      end;
    hkSHA256:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TSHA256Digest));
        Methods.Init := @SHA256_Init;
        Methods.Update := @SHA256_Update;
        Methods.Final := @SHA256_Final;
      end;
    hkSHA512:
      begin
        Methods.Digest := TBuffer.Create(SizeOf(TSHA512Digest));
        Methods.Init := @SHA512_Init;
        Methods.Update := @SHA512_Update;
        Methods.Final := @SHA512_Final;
      end;
  else
    Methods.Digest := TBuffer.Create(0);
    Methods.Init := nil;
    Methods.Update := nil;
    Methods.Final := nil;
    Result := False;
  end;
end;

function GetAuthMethod(Kind: THashKind; out Method: TAuthMethod): Boolean;
begin
  Result := True;
  case Kind of
    hkMD5:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TMD5Digest));
        Method.Method := EVP_md5;
      end;
    hkSHA1:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TSHA1Digest));
        Method.Method := EVP_sha1;
      end;
    hkSHA256:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TSHA256Digest));
        Method.Method := EVP_sha256;
      end;
    hkSHA512:
      begin
        Method.Digest := TBuffer.Create(SizeOf(TSHA512Digest));
        Method.Method := EVP_sha512;
      end;
  else
    Method.Digest := TBuffer.Create(0);
    Method.Method := nil;
    Result := False;
  end;
end;

function HashString(Kind: THashKind; const S: string): TDigest;
begin
  Result := HashBuffer(Kind, PAnsiChar(S)^, Length(S));
end;

function HashBuffer(Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
var
  Methods: THashMethods;
begin
  if GetHashMethods(Kind, Methods) then
  begin
    Methods.Init(Methods.Context);
    Methods.Update(Methods.Context, @Buffer, BufferSize);
    if not Methods.Final(Methods.Digest.Data^, Methods.Context) then
      Methods.Digest := TDigest.Create(0);
  end;
  Result := Methods.Digest;
end;

function HashStream(Kind: THashKind; Stream: TStream): TDigest;
const
  BufferSize = $10000;
var
  Buffer: TBuffer;
  Bytes: LongInt;

  function ReadBuffer: Boolean;
  begin
    Bytes := Stream.Read(Buffer.Data^, BufferSize);
    Result := Bytes > 0;
  end;

var
  Methods: THashMethods;
begin
  if GetHashMethods(Kind, Methods) then
  begin
    Buffer := TBuffer.Create(BufferSize);
    Methods.Init(Methods.Context);
    while ReadBuffer do
      Methods.Update(Methods.Context, Buffer, Bytes);
    if not Methods.Final(Methods.Digest.Data^, Methods.Context) then
      Methods.Digest := TDigest.Create(0);
  end;
  Result := Methods.Digest;
end;

function HashFile(Kind: THashKind; const FileName: string): TDigest;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpen);
  try
    Result := HashStream(Kind, Stream);
  finally
    Stream.Free;
  end;
end;

function AuthString(const Key: string; Kind: THashKind; const S: string): TDigest;
begin
  Result := AuthBuffer(Key, Kind, Pointer(S)^, Length(S));
end;

function AuthBuffer(const Key: string; Kind: THashKind; var Buffer; BufferSize: Cardinal): TDigest;
var
  Method: TAuthMethod;
  Context: THMACCtx;
  I: LongWord;
begin
  if GetAuthMethod(Kind, Method) then
  begin
    HMAC_CTX_init(Context);
    try
      HMAC_Init_ex(Context, Pointer(Key), Length(Key), Method.Method, nil);
      HMAC_Update(Context, @Buffer, BufferSize);
      I := Method.Digest.Size;
      if not HMAC_Final(Context, Method.Digest, I) then
        Method.Digest := TDigest.Create(0);
    finally
      HMAC_CTX_cleanup(Context);
    end;
  end;
  Result := Method.Digest;
end;

function AuthStream(const Key: string; Kind: THashKind; Stream: TStream): TDigest;
const
  BufferSize = $10000;
var
  Buffer: TBuffer;
  Bytes: LongInt;

  function ReadBuffer: Boolean;
  begin
    Bytes := Stream.Read(Buffer.Data^, BufferSize);
    Result := Bytes > 0;
  end;

var
  Method: TAuthMethod;
  Context: THMACCtx;
  I: LongWord;
begin
  if GetAuthMethod(Kind, Method) then
  begin
    Buffer := TBuffer.Create(BufferSize);
    HMAC_CTX_init(Context);
    try
      HMAC_Init_ex(Context, Pointer(Key), Length(Key), Method.Method, nil);
      while ReadBuffer do
        HMAC_Update(Context, Buffer, BufferSize);
      I := Method.Digest.Size;
      if not HMAC_Final(Context, Method.Digest, I) then
        Method.Digest := TDigest.Create(0);
    finally
      HMAC_CTX_cleanup(Context);
    end;
  end;
  Result := Method.Digest;
end;

function AuthFile(const Key: string; Kind: THashKind; const FileName: string): TDigest;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpen);
  try
    Result := AuthStream(Key, Kind, Stream);
  finally
    Stream.Free;
  end;
end;

end.


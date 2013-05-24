(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.interop.openssl.txt> }
unit Bare.Interop.OpenSSL;

{$i bare.inc}

interface

{$ifdef windows}
  {.$define static}
{$endif}

{$ifdef static}
  {$define libssl := external}
  {$define libcrypto := external}
{$else}
  {$ifdef linux}
    {$define libssl := external 'libssl.so'}
    {$define libcrypto := external 'libssl.so'}
  {$endif}
  {$ifdef windows}
    {$define libssl := external 'libssl.dll'}
    {$define libcrypto := external 'libcrypto.dll'}
  {$endif}
{$endif}

const
  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5;
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;

  MD5_DIGEST_LENGTH = 16;
  SHA1_DIGEST_LENGTH = 20;
  SHA256_DIGEST_LENGTH = 32;
  SHA512_DIGEST_LENGTH = 64;

type
  TSSLCtx = Pointer;
  TSSL = Pointer;
  TSSLMethod = Pointer;
  TEVPMethod = Pointer;

  MD5_CTX = record
    data: array[0..127] of Byte;
  end;
  TMD5Ctx = MD5_CTX;
  PMD5Ctx = ^TMD5Ctx;

  MD5_DIGEST = record
    data: array [0..MD5_DIGEST_LENGTH - 1] of Byte;
  end;
  TMD5Digest = MD5_DIGEST;
  PMD5Digest = ^TMD5Digest;

  SHA1_CTX = record
    data: array[0..255] of Byte;
  end;
  TSHA1Ctx = SHA1_CTX;
  PSHA1Ctx = ^TSHA1Ctx;

  SHA1_DIGEST = record
    data: array [0..SHA1_DIGEST_LENGTH - 1] of Byte;
  end;
  TSHA1Digest = SHA1_DIGEST;
  PSHA1Digest = ^TSHA1Digest;

  SHA256_CTX = record
    data: array[0..255] of Byte;
  end;
  TSHA256Ctx = SHA256_CTX;
  PSHA256Ctx = ^TSHA256Ctx;

  SHA256_DIGEST = record
    data: array [0..SHA256_DIGEST_LENGTH - 1] of Byte;
  end;
  TSHA256Digest = SHA256_DIGEST;
  PSHA256Digest = ^TSHA256Digest;

  SHA512_CTX = record
    data: array[0..255] of Byte;
  end;
  TSHA512Ctx = SHA512_CTX;
  PSHA512Ctx = ^TSHA512Ctx;

  SHA512_DIGEST = record
    data: array [0..SHA512_DIGEST_LENGTH - 1] of Byte;
  end;
  TSHA512Digest = SHA512_DIGEST;
  PSHA512Digest = ^TSHA512Digest;

  HMAC_CTX = record
    data: array[0..511] of Byte;
  end;
  THMACCtx = HMAC_CTX;
  PHMACCtx = ^THMACCtx;

{ OpenSSL routines }

function SSL_library_init: Integer; cdecl; libssl;
procedure SSL_load_error_strings; cdecl; libssl;
function SSLv23_client_method: TSSLMethod; cdecl; libssl;
function SSL_CTX_new(method: TSSLMethod): TSSLCtx; cdecl; libssl;
procedure SSL_CTX_free(context: TSSLCtx); cdecl; libssl;
function SSL_new(context: TSSLCtx): TSSL; cdecl; libssl;
function SSL_shutdown(ssl: TSSL): LongInt; cdecl; libssl;
procedure SSL_free(ssl: TSSL); cdecl; libssl;
function SSL_set_fd(ssl: TSSL; socket: LongInt): LongBool; cdecl; libssl;
function SSL_connect(ssl: TSSL): LongBool; cdecl; libssl;
function SSL_write(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl; libssl;
function SSL_read(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl; libssl;
function SSL_get_error(ssl: TSSL; ret_code: Integer): Integer; cdecl; libssl;

{ Hashing routines }

function MD5_Init(out context: TMD5Ctx): LongBool; cdecl; libcrypto;
function MD5_Update(var context: TMD5Ctx; data: Pointer; size: Cardinal): LongBool; cdecl; libcrypto;
function MD5_Final(out digest: TMD5Digest; var context: TMD5Ctx): LongBool; cdecl; libcrypto;
function SHA1_Init(out context: TSHA1Ctx): LongBool; cdecl; libcrypto;
function SHA1_Update(var context: TSHA1Ctx; data: Pointer; size: Cardinal): LongBool; cdecl; libcrypto;
function SHA1_Final(out digest: TSHA1Digest; var context: TSHA1Ctx): LongBool; cdecl; libcrypto;
function SHA256_Init(out context: TSHA256Ctx): LongBool; cdecl; libcrypto;
function SHA256_Update(var context: TSHA256Ctx; data: Pointer; size: Cardinal): LongBool; cdecl; libcrypto;
function SHA256_Final(out digest: TSHA256Digest; var context: TSHA256Ctx): LongBool; cdecl; libcrypto;
function SHA512_Init(out context: TSHA512Ctx): LongBool; cdecl; libcrypto;
function SHA512_Update(var context: TSHA512Ctx; data: Pointer; size: Cardinal): LongBool; cdecl; libcrypto;
function SHA512_Final(out digest: TSHA512Digest; var context: TSHA512Ctx): LongBool; cdecl; libcrypto;
function EVP_md5: TEVPMethod; cdecl; libcrypto;
function EVP_sha1: TEVPMethod; cdecl; libcrypto;
function EVP_sha256: TEVPMethod; cdecl; libcrypto;
function EVP_sha512: TEVPMethod; cdecl; libcrypto;
procedure HMAC_CTX_init(out context: THMACCtx); cdecl; libcrypto;
procedure HMAC_CTX_cleanup(var context: THMACCtx); cdecl; libcrypto;
function HMAC_Init_ex(var context: THMACCtx; key: Pointer; size: Cardinal; method: TEVPMethod; engine: Pointer): LongBool; cdecl; libcrypto;
function HMAC_Update(var context: THMACCtx; data: Pointer; size: Cardinal): LongBool; cdecl; libcrypto;
function HMAC_Final(var context: THMACCtx; digest: Pointer; var digestSize: LongWord): LongBool; cdecl; libcrypto;

function OpenSSLInit: Boolean;

implementation

{$ifdef static}
  {$ifdef windows}
    {$i mingw-w64.inc}
  {$endif}
  {$linklib libssl.a}
  {$linklib libcrypto.a}
{$endif}

var
  Initialized: Boolean;  
  
function OpenSSLInit: Boolean;
begin
  if Initialized then
    Exit(True);
  Initialized := True;
  SSL_library_init;
  SSL_load_error_strings;
  Result := True;
end;

end.


(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.interop.openssl.txt> }
unit bare.interop.openssl;

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
    {$define libsslres := 'libssl.so'}
    {$define libcryptores := 'libssl.so'}
    {$define libssl := external 'libssl.so'}
    {$define libcrypto := external 'libssl.so'}
  {$endif}
  {$ifdef windows}
		{$define latebind}
    {$define libsslres := 'libssl.dll'}
    {$define libcryptores := 'libcrypto.dll'}
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

{$ifdef latebind}
var
  { OpenSSL routines }
  SSL_library_init: function: Integer; cdecl;
  SSL_load_error_strings: procedure; cdecl;
  SSLv23_client_method: function: TSSLMethod; cdecl;
  SSL_CTX_new: function(method: TSSLMethod): TSSLCtx; cdecl;
  SSL_CTX_free: procedure(context: TSSLCtx); cdecl;
  SSL_new: function(context: TSSLCtx): TSSL; cdecl;
  SSL_shutdown: function(ssl: TSSL): LongInt; cdecl;
  SSL_free: procedure(ssl: TSSL); cdecl;
  SSL_set_fd: function(ssl: TSSL; socket: LongInt): LongBool; cdecl;
  SSL_connect: function(ssl: TSSL): LongBool; cdecl;
  SSL_write: function(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl;
  SSL_read: function(ssl: TSSL; buffer: Pointer; size: LongWord): LongInt; cdecl;
  SSL_get_error: function(ssl: TSSL; ret_code: Integer): Integer; cdecl;
  { Hashing routines }
  MD5_Init: function(out context: TMD5Ctx): LongBool; cdecl;
  MD5_Update: function(var context: TMD5Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  MD5_Final: function(out digest: TMD5Digest; var context: TMD5Ctx): LongBool; cdecl;
  SHA1_Init: function(out context: TSHA1Ctx): LongBool; cdecl;
  SHA1_Update: function(var context: TSHA1Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  SHA1_Final: function(out digest: TSHA1Digest; var context: TSHA1Ctx): LongBool; cdecl;
  SHA256_Init: function(out context: TSHA256Ctx): LongBool; cdecl;
  SHA256_Update: function(var context: TSHA256Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  SHA256_Final: function(out digest: TSHA256Digest; var context: TSHA256Ctx): LongBool; cdecl;
  SHA512_Init: function(out context: TSHA512Ctx): LongBool; cdecl;
  SHA512_Update: function(var context: TSHA512Ctx; data: Pointer; size: Cardinal): LongBool; cdecl;
  SHA512_Final: function(out digest: TSHA512Digest; var context: TSHA512Ctx): LongBool; cdecl;
  EVP_md5: function: TEVPMethod; cdecl;
  EVP_sha1: function: TEVPMethod; cdecl;
  EVP_sha256: function: TEVPMethod; cdecl;
  EVP_sha512: function: TEVPMethod; cdecl;
  HMAC_CTX_init: procedure(out context: THMACCtx); cdecl;
  HMAC_CTX_cleanup: procedure(var context: THMACCtx); cdecl;
  HMAC_Init_ex: function(var context: THMACCtx; key: Pointer; size: Cardinal; method: TEVPMethod; engine: Pointer): LongBool; cdecl;
  HMAC_Update: function(var context: THMACCtx; data: Pointer; size: Cardinal): LongBool; cdecl;
  HMAC_Final: function(var context: THMACCtx; digest: Pointer; var digestSize: LongWord): LongBool; cdecl;
{$else}
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
{$endif}

function OpenSSLInit: Boolean;

implementation

{$ifdef static}
  {$ifdef windows}
    {$i mingw-w64.inc}
  {$endif}
  {$linklib libssl.a}
  {$linklib libcrypto.a}
{$endif}
{$ifdef latebind}
uses
  Bare.System;
{$endif}

var
  Initialized: Boolean;  
  
function OpenSSLInit: Boolean;
{$ifdef latebind}
var
  Module: HModule;
{$endif}
begin
  if Initialized then
    Exit(True);
  Initialized := True;
  {$ifdef latebind}
  Module := LoadLibrary(libsslres);
  @SSL_library_init := GetProcAddress(Module, 'SSL_library_init');
  @SSL_load_error_strings := GetProcAddress(Module, 'SSL_load_error_strings');
  @SSLv23_client_method := GetProcAddress(Module, 'SSLv23_client_method');
  @SSL_CTX_new := GetProcAddress(Module, 'SSL_CTX_new');
  @SSL_CTX_free := GetProcAddress(Module, 'SSL_CTX_free');
  @SSL_new := GetProcAddress(Module, 'SSL_new');
  @SSL_shutdown := GetProcAddress(Module, 'SSL_shutdown');
  @SSL_free := GetProcAddress(Module, 'SSL_free');
  @SSL_set_fd := GetProcAddress(Module, 'SSL_set_fd');
  @SSL_connect := GetProcAddress(Module, 'SSL_connect');
  @SSL_write := GetProcAddress(Module, 'SSL_write');
  @SSL_read := GetProcAddress(Module, 'SSL_read');
  @SSL_get_error := GetProcAddress(Module, 'SSL_get_error');
  Module := LoadLibrary(libcryptores);
  @MD5_Init := GetProcAddress(Module, 'MD5_Init');
  @MD5_Update := GetProcAddress(Module, 'MD5_Update');
  @MD5_Final := GetProcAddress(Module, 'MD5_Final');
  @SHA1_Init := GetProcAddress(Module, 'SHA1_Init');
  @SHA1_Update := GetProcAddress(Module, 'SHA1_Update');
  @SHA1_Final := GetProcAddress(Module, 'SHA1_Final');
  @SHA256_Init := GetProcAddress(Module, 'SHA256_Init');
  @SHA256_Update := GetProcAddress(Module, 'SHA256_Update');
  @SHA256_Final := GetProcAddress(Module, 'SHA256_Final');
  @SHA512_Init := GetProcAddress(Module, 'SHA512_Init');
  @SHA512_Update := GetProcAddress(Module, 'SHA512_Update');
  @SHA512_Final := GetProcAddress(Module, 'SHA512_Final');
  @EVP_md5 := GetProcAddress(Module, 'EVP_md5');
  @EVP_sha1 := GetProcAddress(Module, 'EVP_sha1');
  @EVP_sha256 := GetProcAddress(Module, 'EVP_sha256');
  @EVP_sha512 := GetProcAddress(Module, 'EVP_sha512');
  @HMAC_CTX_init := GetProcAddress(Module, 'HMAC_CTX_init');
  @HMAC_CTX_cleanup := GetProcAddress(Module, 'HMAC_CTX_cleanup');
  @HMAC_Init_ex := GetProcAddress(Module, 'HMAC_Init_ex');
  @HMAC_Update := GetProcAddress(Module, 'HMAC_Update');
  @HMAC_Final := GetProcAddress(Module, 'HMAC_Final');
  {$endif}
  SSL_library_init;
  SSL_load_error_strings;
  Result := True;
end;

end.


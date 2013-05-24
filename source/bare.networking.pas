(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.networking.txt> }
unit Bare.Networking;

{$i bare.inc}

interface

uses
  Bare.System,
  Bare.Interop.Sockets,
  Bare.Interop.OpenSSL;

{ TAddressName record }

type
  TAddressName = record
  private
    FAddress: LongWord;
    FHost: string;
    FLocation: string;
    FResolved: Boolean;
  public
    class function Create(const Host: string): TAddressName; static; overload;
    class function Create(Address: LongWord): TAddressName; static; overload;
    class function Create(A, B, C, D: Byte): TAddressName; static; overload;
    class operator Implicit(const Value: TAddressName): string;
    class operator Implicit(const Value: string): TAddressName;
    function Resolve: Boolean;
    property Address: LongWord read FAddress;
    property Host: string read FHost;
    property Location: string read FLocation;
    property Resolved: Boolean read FResolved;
  end;

{ TSocket class }

  TSocketState = (ssClosed, ssServer, ssClient, ssRemote);

  TSocket = class
  private
    FAddress: TAddressName;
    FPort: Word;
    FHandle: TSocketHandle;
    FServer: TSocket;
    FState: TSocketState;
    FSecure: Boolean;
    FSSLContext: TSSLCtx;
    FSSLSocket: TSSL;
    FTimeout: LongWord;
    function GetAddress: TAddressName;
    procedure SetSecure(Value: Boolean);
    function GetConnected: Boolean;
  public
    constructor Create; overload;
    constructor Create(Server: TSocket); overload;
    destructor Destroy; override;
    procedure Close;
    function Connect(const Address: TAddressName; Port: Word): Boolean;
    function Listen(const Address: TAddressName; Port: Word): Boolean; overload;
    function Listen(Port: Word): Boolean; overload;
    function Accept(Socket: TSocket): Boolean;
    function Read(var Data; Size: LongWord): Integer; overload;
    function Read(out Text: string; BufferSize: LongWord = $1000): Integer; overload;
    function Write(var Data; Size: LongWord): Integer; overload;
    function Write(const Text: string): Integer; overload;
    property Address: TAddressName read GetAddress;
    property Port: Word read FPort;
    property Server: TSocket read FServer;
    property Secure: Boolean read FSecure write SetSecure;
    property Timeout: LongWord read FTimeout write FTimeout;
    property Connected: Boolean read GetConnected;
  end;

implementation

{ TAddressName }

class function TAddressName.Create(const Host: string): TAddressName;
begin
  Result.FAddress := 0;
  Result.FHost := Host;
  Result.FLocation := '';
  Result.FResolved := False;
end;

class function TAddressName.Create(Address: LongWord): TAddressName;
var
  Addr: TInAddr;
begin
  Addr.s_addr := Address;
  Result.FAddress := Addr.s_addr;
  Result.FHost := inet_ntoa(Addr);
  Result.FLocation := Result.FHost;
  Result.FResolved := True;
end;

class function TAddressName.Create(A, B, C, D: Byte): TAddressName;
var
  Addr: TInAddr;
begin
  SocketsInit;
  Addr.S_un_b.s_b1 := A;
  Addr.S_un_b.s_b2 := B;
  Addr.S_un_b.s_b3 := C;
  Addr.S_un_b.s_b4 := D;
  Result.FAddress := Addr.s_addr;
  Result.FHost := inet_ntoa(Addr);
  Result.FLocation := Result.FHost;
  Result.FResolved := True;
end;

class operator TAddressName.Implicit(const Value: TAddressName): string;
begin
  Result := Value.Host;
end;

class operator TAddressName.Implicit(const Value: string): TAddressName;
begin
  Result := TAddressName.Create(Value);
end;

function TAddressName.Resolve: Boolean;
var
  HostEnt: PHostEnt;
  Addr: PInAddr;
begin
  SocketsInit;
  if FResolved then
    Exit(True);
  if FHost = '' then
    Exit(False);
  HostEnt := gethostbyname(PAnsiChar(FHost));
  if HostEnt = nil then
    Exit(False);
  Addr := HostEnt.h_addr^;
  FAddress := Addr.S_addr;
  FLocation := inet_ntoa(Addr^);
  FResolved := True;
  Result := True;
end;

{ TSocket class }

constructor TSocket.Create;
const
  DefaultTimeout = 4000;
begin
  inherited Create;
  SocketsInit;
  FHandle := INVALID_SOCKET;
  FTimeout := DefaultTimeout;
end;

constructor TSocket.Create(Server: TSocket);
begin
  Create;
  FServer := Server;
end;

destructor TSocket.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TSocket.Close;
var
  S: TSSL;
  C: TSSLCtx;
  H: TSocketHandle;
begin
  if FHandle = INVALID_SOCKET then
    Exit;
  S := FSSLSocket;
  C := FSSLContext;
  H := FHandle;
  FSSLSocket := nil;
  FSSLContext := nil;
  FHandle := INVALID_SOCKET;
  if S <> nil then
  begin
    SSL_shutdown(S);
    SSL_free(S);
  end;
  if C <> nil then
    SSL_CTX_free(C);
  shutdown(H, SHUT_RDWR);
  Bare.Interop.Sockets.close(H);
end;

function TSocket.Connect(const Address: TAddressName; Port: Word):  Boolean;
var
  Addr: TSockAddrIn;
begin
  Close;
  if FSecure then
    OpenSSLInit;
  FAddress := Address;
  FPort := Port;
  if not FAddress.Resolve then
    Exit(False);
  if FPort = 0 then
    Exit(False);
  FHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if FHandle = INVALID_SOCKET then
    Exit(False);
  Addr.sin_family := AF_INET;
  Addr.sin_addr.s_addr := FAddress.Address;
  Addr.sin_port := htons(FPort);
  if Bare.Interop.Sockets.connect(FHandle, @Addr, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    Close;
    Exit(False);
  end;
  if Timeout > 0 then
  begin
    setsockopt(FHandle, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
    setsockopt(FHandle, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
  end;
  FState := ssClient;
  if FSecure then
  begin
    FSSLContext := SSL_CTX_new(SSLv23_client_method);
    if FSSLContext = nil then
    begin
      Close;
      Exit(False);
    end;
    FSSLSocket := SSL_new(FSSLContext);
    if FSSLSocket = nil then
    begin
      Close;
      Exit(False);
    end;
    if not SSL_set_fd(FSSLSocket, FHandle) then
    begin
      Close;
      Exit(False);
    end;
    if not SSL_connect(FSSLSocket) then
    begin
      Close;
      Exit(False);
    end;
  end;
  Result := True;
end;

function TSocket.Listen(const Address: TAddressName; Port: Word): Boolean;
var
  Addr: TSockAddrIn;
begin
  Close;
  FAddress := Address;
  FPort := Port;
  if FPort = 0 then
    Exit(False);
  if FSecure then
    Exit(False);
  FHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if FHandle = INVALID_SOCKET then
    Exit(False);
  Addr.sin_family := AF_INET;
  if FAddress.Resolve then
    Addr.sin_addr.s_addr := FAddress.Address
  else
    Addr.sin_addr.s_addr := INADDR_ANY;
  Addr.sin_port := htons(FPort);
  if bind(FHandle, @Addr, SizeOf(Addr)) = SOCKET_ERROR then
  begin
    Close;
    Exit(False);
  end;
  if not FAddress.Resolved then
    FAddress := TAddressName.Create(Addr.sin_addr.s_addr);
  if Bare.Interop.Sockets.listen(FHandle, SOMAXCONN) = SOCKET_ERROR then
  begin
    Close;
    Exit(False);
  end;
  FState := ssServer;
  Result := True;
end;

function TSocket.Listen(Port: Word): Boolean;
begin
  Result := Listen(TAddressName.Create(''), Port);
end;

function TSocket.Accept(Socket: TSocket): Boolean;
var
  Addr: TSockAddrIn;
  I: Integer;
  H: TSocketHandle;
begin
  if Socket = Self then
    Exit(False);
  Socket.Close;
  if FState <> ssServer then
    Exit(False);
  I := SizeOf(Addr);
  H := Bare.Interop.Sockets.accept(FHandle, @Addr, I);
  if H = INVALID_SOCKET then
    Exit(False);
  if Socket.FTimeout > 0 then
  begin
    setsockopt(H, SOL_SOCKET, SO_SNDTIMEO, @Socket.FTimeout, SizeOf(FTimeout));
    setsockopt(H, SOL_SOCKET, SO_RCVTIMEO, @Socket.FTimeout, SizeOf(FTimeout));
  end;
  Socket.FHandle := H;
  Socket.FAddress := TAddressName.Create(Addr.sin_addr.s_addr);
  Socket.FPort := ntohs(Addr.sin_port);
  Socket.FState := ssRemote;
  Socket.FServer := Self;
  Result := True;
end;

function TSocket.Read(var Data; Size: LongWord): Integer;
var
  Bytes: LongInt;
begin
  if FState < ssClient then
    Exit(SOCKET_ERROR);
  if Size = 0 then
    Exit(0);
  if FSecure then
    Bytes := SSL_read(FSSLSocket, @Data, Size)
  else
    Bytes := recv(FHandle, Data, Size, MSG_NOSIGNAL);
  if Bytes < 0 then
  begin
    Close;
    Exit(SOCKET_ERROR);
  end;
  Result := Bytes;
end;

function TSocket.Read(out Text: string; BufferSize: LongWord = $1000): Integer;
var
  Bytes: LongInt;
begin
  SetLength(Text, 0);
  if FState < ssClient then
    Exit(SOCKET_ERROR);
  if BufferSize = 0 then
    Exit(0);
  SetLength(Text, BufferSize);
  if FSecure then
    Bytes := SSL_read(FSSLSocket, Pointer(Text), BufferSize)
  else
    Bytes := recv(FHandle, PChar(Text)^, BufferSize, MSG_NOSIGNAL);
  if Bytes < 0 then
  begin
    SetLength(Text, 0);
    Close;
    Exit(SOCKET_ERROR);
  end;
  if Bytes < Length(Text) then
    SetLength(Text, Bytes);
  Result := Bytes;
end;

function TSocket.Write(var Data; Size: LongWord): Integer;
var
  Bytes: LongInt;
begin
  if FState < ssClient then
    Exit(SOCKET_ERROR);
  if Size = 0 then
    Exit(0);
  if FSecure then
    Bytes := SSL_write(FSSLSocket, @Data, Size)
  else
    Bytes := send(FHandle, Data, Size, MSG_NOSIGNAL);
  if Bytes < 0 then
  begin
    Close;
    Exit(SOCKET_ERROR);
  end;
  Result := Bytes;
end;

function TSocket.Write(const Text: string): Integer;
var
  Size: LongWord;
  Bytes: LongInt;
begin
  if FState < ssClient then
    Exit(SOCKET_ERROR);
  Size := Length(Text);
  if Size = 0 then
    Exit(0);
  if FSecure then
    Bytes := SSL_write(FSSLSocket, Pointer(Text), Size)
  else
    Bytes := send(FHandle, PChar(Text)^, Size, MSG_NOSIGNAL);
  if Bytes < 0 then
  begin
    Close;
    Exit(SOCKET_ERROR);
  end;
  Result := Bytes;
end;

function TSocket.GetAddress: TAddressName;
begin
  Result := FAddress;
end;

procedure TSocket.SetSecure(Value: Boolean);
begin
  if Value <> FSecure then
  begin
    Close;
    FSecure := True;
  end;
end;

function TSocket.GetConnected: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

end.


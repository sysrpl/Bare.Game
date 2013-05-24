unit CrossSockets;

{$i cross.inc}

interface

{$ifdef linux}
  {$define libsocket := external 'libc.so'}
{$endif}
{$ifdef windows}
  {$define libsocket := external 'wsock32.dll'}
{$endif}

type
  TSocketHandle = LongInt;

const
  FD_SETSIZE = 64;

  SD_RECEIVE = 0;
  SD_SEND = 1;
  SD_BOTH = 2;

type
  PFDSet = ^TFDSet;
  TFDSet = record
    fd_count: LongWord;
    fd_array: array[0..FD_SETSIZE-1] of TSocketHandle;
  end;

  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: LongInt;
    tv_usec: LongInt;
  end;

  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: Byte;
  end;

  SunW = packed record
    s_w1, s_w2: Word;
  end;

  PInAddr = ^TInAddr;
  TInAddr = record
    case LongInt of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: LongWord);
  end;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = packed record
    case LongInt of
       0: (
        sin_family: Word;
        sin_port: Word;
        sin_addr: TInAddr;
        sin_zero: array[0..7] of AnsiChar);
      1: (
        sa_family: Word;
        sa_data: array[0..13] of AnsiChar)
  end;

  PSockAddr = ^TSockAddr;
  TSockAddr = TSockAddrIn;

  PHostEnt = ^THostEnt;
  THostEnt = record
    h_name: PAnsiChar;
    h_aliases: ^PAnsiChar;
    {$ifdef windows}
    h_addrtype: SmallInt;
    h_length: SmallInt;
    {$else}
    h_addrtype: LongInt;
    h_length: LongInt;
    {$endif}
    h_addr: ^PInAddr;
  end;

  PNetEnt = ^TNetEnt;
  TNetEnt = record
    n_name: PAnsiChar;
    n_aliases: ^PAnsiChar;
    n_addrtype: SmallInt;
    n_net: LongInt;
  end;

  PServEnt = ^TServEnt;
  TServEnt = record
    s_name: PAnsiChar;
    s_aliases: ^PAnsiChar;
    s_port: Word;
    s_proto: PAnsiChar;
  end;

  PProtoEnt = ^TProtoEnt;
  TProtoEnt = record
    p_name: PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto: SmallInt;
  end;

{ Protocols }

const
  IPPROTO_IP = 0;
  IPPROTO_ICMP = 1;
  IPPROTO_IGMP = 2;
  IPPROTO_GGP = 3;
  IPPROTO_TCP = 6;
  IPPROTO_PUP = 12;
  IPPROTO_UDP = 17;
  IPPROTO_IDP = 22;
  IPPROTO_ND = 77;
  IPPROTO_RAW = 255;
  IPPROTO_MAX = 256;

  IPPORT_ECHO = 7;
  IPPORT_DISCARD = 9;
  IPPORT_SYSTAT = 11;
  IPPORT_DAYTIME = 13;
  IPPORT_NETSTAT = 15;
  IPPORT_FTP = 21;
  IPPORT_TELNET = 23;
  IPPORT_SMTP = 25;
  IPPORT_TIMESERVER = 37;
  IPPORT_NAMESERVER = 42;
  IPPORT_WHOIS = 43;
  IPPORT_MTP = 57;

  IPPORT_TFTP = 69;
  IPPORT_RJE = 77;
  IPPORT_FINGER = 79;
  IPPORT_TTYLINK = 87;
  IPPORT_SUPDUP = 95;

  IPPORT_EXECSERVER = 512;
  IPPORT_LOGINSERVER = 513;
  IPPORT_CMDSERVER = 514;
  IPPORT_EFSSERVER = 520;

  IPPORT_BIFFUDP = 512;
  IPPORT_WHOSERVER = 513;
  IPPORT_ROUTESERVER = 520;

  IPPORT_RESERVED = 1024;

  IMPLINK_IP = 155;
  IMPLINK_LOWEXPER = 156;
  IMPLINK_HIGHEXPER = 158;

  {$ifdef unix}
  MSG_NOSIGNAL  = $4000;
  {$else}
  MSG_NOSIGNAL  = $0000;
  {$endif}

const
  INADDR_ANY = $00000000;
  INADDR_LOOPBACK = $7F000001;
  INADDR_BROADCAST = -1;
  INADDR_NONE = -1;

const
  TF_DISCONNECT = $01;
  TF_REUSE_SOCKET = $02;
  TF_WRITE_BEHIND = $04;

{ Options for use with [gs]etsockopt at the IP level. }

  IP_OPTIONS = 1;
  IP_MULTICAST_IF = 2; // set/get IP multicast interface
  IP_MULTICAST_TTL = 3; // set/get IP multicast timetolive
  IP_MULTICAST_LOOP = 4; // set/get IP multicast loopback
  IP_ADD_MEMBERSHIP = 5; // add an IP group membership
  IP_DROP_MEMBERSHIP = 6; // drop an IP group membership
  IP_TTL = 7; // set/get IP Time To Live
  IP_TOS = 8; // set/get IP Type Of Service
  IP_DONTFRAGMENT = 9; // set/get IP Don't Fragment flag


  IP_DEFAULT_MULTICAST_TTL = 1; // normally limit m'casts to 1 hop
  IP_DEFAULT_MULTICAST_LOOP = 1; // normally hear sends if a member
  IP_MAX_MEMBERSHIPS = 20; // per socket; must fit in one mbuf

  INVALID_SOCKET = TSocketHandle(-1);
  SOCKET_ERROR = -1;

  { Socket types }

  SOCK_STREAM = 1; // stream socket
  SOCK_DGRAM = 2; // datagram socket
  SOCK_RAW = 3; // raw-protocol interface
  SOCK_RDM = 4; // reliably-delivered message
  SOCK_SEQPACKET = 5; // sequenced packet stream

{ Option flags per-socket. }

  SO_DEBUG = $0001; // turn on debugging info recording
  SO_ACCEPTCONN = $0002; // socket has had listen()
  SO_REUSEADDR = $0004; // allow local address reuse
  SO_KEEPALIVE = $0008; // keep connections alive
  SO_DONTROUTE = $0010; // just use interface addresses
  SO_BROADCAST = $0020; // permit sending of broadcast msgs
  SO_USELOOPBACK = $0040; // bypass hardware when possible
  SO_LINGER = $0080; // linger on close if data present
  SO_OOBINLINE = $0100; // leave received OOB data in line

  SO_DONTLINGER = $ff7f;

{ Additional options. }

  SO_RCVBUF = $1002; // receive buffer size
  SO_SNDLOWAT = $1003; // send low-water mark
  SO_RCVLOWAT = $1004; // receive low-water mark
  SO_SNDTIMEO = $1005; // send timeout
  SO_RCVTIMEO = $1006; // receive timeout
  SO_ERROR = $1007; // get error status and clear
  SO_TYPE = $1008; // get socket type

{ Options for connect and disconnect data and options. Used only by
  non-TCP/IP transports such as DECNet, OSI TP4, etc. }

  SO_CONNDATA = $7000;
  SO_CONNOPT = $7001;
  SO_DISCDATA = $7002;
  SO_DISCOPT = $7003;
  SO_CONNDATALEN = $7004;
  SO_CONNOPTLEN = $7005;
  SO_DISCDATALEN = $7006;
  SO_DISCOPTLEN = $7007;

{ Option for opening sockets for synchronous access. }

  SO_OPENTYPE = $7008;

  SO_SYNCHRONOUS_ALERT = $10;
  SO_SYNCHRONOUS_NONALERT = $20;

{ Other NT-specific options. }

  SO_MAXDG = $7009;
  SO_MAXPATHDG = $700A;
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
  SO_CONNECT_TIME = $700C;

{ TCP options. }

  TCP_NODELAY = $0001;
  TCP_BSDURGENT = $7000;

{ Address families. }

  AF_UNSPEC = 0; // unspecified
  AF_UNIX = 1; // local to host (pipes, portals)
  AF_INET = 2; // internetwork: UDP, TCP, etc.
  AF_IMPLINK = 3; // arpanet imp addresses
  AF_PUP = 4; // pup protocols: e.g. BSP
  AF_CHAOS = 5; // mit CHAOS protocols
  AF_IPX = 6; // IPX and SPX
  AF_NS = 6; // XEROX NS protocols
  AF_ISO = 7; // ISO protocols
  AF_OSI = AF_ISO; // OSI is ISO
  AF_ECMA = 8; // european computer manufacturers
  AF_DATAKIT = 9; // datakit protocols
  AF_CCITT = 10; // CCITT protocols, X.25 etc
  AF_SNA = 11; // IBM SNA
  AF_DECnet = 12; // DECnet
  AF_DLI = 13; // Direct data link interface
  AF_LAT = 14; // LAT
  AF_HYLINK = 15; // NSC Hyperchannel
  AF_APPLETALK = 16; // AppleTalk
  AF_NETBIOS = 17; // NetBios-style addresses
  AF_VOICEVIEW = 18; // VoiceView
  AF_FIREFOX = 19; // FireFox
  AF_UNKNOWN1 = 20; // Somebody is using this!
  AF_BAN = 21; // Banyan

  AF_MAX = 22;

{ Structure used by kernel to store most addresses. }

type
  PSockProto = ^TSockProto;
  TSockProto = record
   sp_family: Word;
   sp_protocol: Word;
  end;

const
  PF_UNSPEC = AF_UNSPEC;
  PF_UNIX = AF_UNIX;
  PF_INET = AF_INET;
  PF_IMPLINK = AF_IMPLINK;
  PF_PUP = AF_PUP;
  PF_CHAOS = AF_CHAOS;
  PF_NS = AF_NS;
  PF_IPX = AF_IPX;
  PF_ISO = AF_ISO;
  PF_OSI = AF_OSI;
  PF_ECMA = AF_ECMA;
  PF_DATAKIT = AF_DATAKIT;
  PF_CCITT = AF_CCITT;
  PF_SNA = AF_SNA;
  PF_DECnet = AF_DECnet;
  PF_DLI = AF_DLI;
  PF_LAT = AF_LAT;
  PF_HYLINK = AF_HYLINK;
  PF_APPLETALK = AF_APPLETALK;
  PF_VOICEVIEW = AF_VOICEVIEW;
  PF_FIREFOX = AF_FIREFOX;
  PF_UNKNOWN1 = AF_UNKNOWN1;
  PF_BAN = AF_BAN;

  PF_MAX = AF_MAX;

type
  PLinger = ^TLinger;
  TLinger = record
    l_onoff: Word;
    l_linger: Word;
  end;

const
  SOL_SOCKET = $FFFF;
  SOMAXCONN = 5;
  MSG_OOB = $1;
  MSG_PEEK = $2;
  MSG_DONTROUTE = $4;
  MSG_MAXIOVLEN = 16;
  MSG_PARTIAL = $8000;

  MAXGETHOSTSTRUCT = 1024;

  FD_READ = $01;
  FD_WRITE = $02;
  FD_OOB = $04;
  FD_ACCEPT = $08;
  FD_CONNECT = $10;
  FD_CLOSE = $20;

{ The following constants should be used for the
  second parameter of 'shutdown' }

  SHUT_RD = 0; // No more receptions
  SHUT_WR = 1; // No more transmissions
  SHUT_RDWR = 2; // No more receptions or transmissions

  WSABASEERR = 10000;
  WSAEINTR = (WSABASEERR+4);
  WSAEBADF = (WSABASEERR+9);
  WSAEACCES = (WSABASEERR+13);
  WSAEFAULT = (WSABASEERR+14);
  WSAEINVAL = (WSABASEERR+22);
  WSAEMFILE = (WSABASEERR+24);

  WSAEWOULDBLOCK = (WSABASEERR+35);
  WSAEINPROGRESS = (WSABASEERR+36);
  WSAEALREADY = (WSABASEERR+37);
  WSAENOTSOCK = (WSABASEERR+38);
  WSAEDESTADDRREQ = (WSABASEERR+39);
  WSAEMSGSIZE = (WSABASEERR+40);
  WSAEPROTOTYPE = (WSABASEERR+41);
  WSAENOPROTOOPT = (WSABASEERR+42);
  WSAEPROTONOSUPPORT = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT = (WSABASEERR+44);
  WSAEOPNOTSUPP = (WSABASEERR+45);
  WSAEPFNOSUPPORT = (WSABASEERR+46);
  WSAEAFNOSUPPORT = (WSABASEERR+47);
  WSAEADDRINUSE = (WSABASEERR+48);
  WSAEADDRNOTAVAIL = (WSABASEERR+49);
  WSAENETDOWN = (WSABASEERR+50);
  WSAENETUNREACH = (WSABASEERR+51);
  WSAENETRESET = (WSABASEERR+52);
  WSAECONNABORTED = (WSABASEERR+53);
  WSAECONNRESET = (WSABASEERR+54);
  WSAENOBUFS = (WSABASEERR+55);
  WSAEISCONN = (WSABASEERR+56);
  WSAENOTCONN = (WSABASEERR+57);
  WSAESHUTDOWN = (WSABASEERR+58);
  WSAETOOMANYREFS = (WSABASEERR+59);
  WSAETIMEDOUT = (WSABASEERR+60);
  WSAECONNREFUSED = (WSABASEERR+61);
  WSAELOOP = (WSABASEERR+62);
  WSAENAMETOOLONG = (WSABASEERR+63);
  WSAEHOSTDOWN = (WSABASEERR+64);
  WSAEHOSTUNREACH = (WSABASEERR+65);
  WSAENOTEMPTY = (WSABASEERR+66);
  WSAEPROCLIM = (WSABASEERR+67);
  WSAEUSERS = (WSABASEERR+68);
  WSAEDQUOT = (WSABASEERR+69);
  WSAESTALE = (WSABASEERR+70);
  WSAEREMOTE = (WSABASEERR+71);

  WSAEDISCON = (WSABASEERR+101);

  WSASYSNOTREADY = (WSABASEERR+91);
  WSAVERNOTSUPPORTED = (WSABASEERR+92);
  WSANOTINITIALISED = (WSABASEERR+93);
  WSAHOST_NOT_FOUND = (WSABASEERR+1001);
  HOST_NOT_FOUND = WSAHOST_NOT_FOUND;
  WSATRY_AGAIN = (WSABASEERR+1002);
  TRY_AGAIN = WSATRY_AGAIN;
  WSANO_RECOVERY = (WSABASEERR+1003);
  NO_RECOVERY = WSANO_RECOVERY;
  WSANO_DATA = (WSABASEERR+1004);
  NO_DATA = WSANO_DATA;
  WSANO_ADDRESS = WSANO_DATA;
  NO_ADDRESS = WSANO_ADDRESS;

  EWOULDBLOCK = WSAEWOULDBLOCK;
  EINPROGRESS = WSAEINPROGRESS;
  EALREADY = WSAEALREADY;
  ENOTSOCK = WSAENOTSOCK;
  EDESTADDRREQ = WSAEDESTADDRREQ;
  EMSGSIZE = WSAEMSGSIZE;
  EPROTOTYPE = WSAEPROTOTYPE;
  ENOPROTOOPT = WSAENOPROTOOPT;
  EPROTONOSUPPORT = WSAEPROTONOSUPPORT;
  ESOCKTNOSUPPORT = WSAESOCKTNOSUPPORT;
  EOPNOTSUPP = WSAEOPNOTSUPP;
  EPFNOSUPPORT = WSAEPFNOSUPPORT;
  EAFNOSUPPORT = WSAEAFNOSUPPORT;
  EADDRINUSE = WSAEADDRINUSE;
  EADDRNOTAVAIL = WSAEADDRNOTAVAIL;
  ENETDOWN = WSAENETDOWN;
  ENETUNREACH = WSAENETUNREACH;
  ENETRESET = WSAENETRESET;
  ECONNABORTED = WSAECONNABORTED;
  ECONNRESET = WSAECONNRESET;
  ENOBUFS = WSAENOBUFS;
  EISCONN = WSAEISCONN;
  ENOTCONN = WSAENOTCONN;
  ESHUTDOWN = WSAESHUTDOWN;
  ETOOMANYREFS = WSAETOOMANYREFS;
  ETIMEDOUT = WSAETIMEDOUT;
  ECONNREFUSED = WSAECONNREFUSED;
  ELOOP = WSAELOOP;
  ENAMETOOLONG = WSAENAMETOOLONG;
  EHOSTDOWN = WSAEHOSTDOWN;
  EHOSTUNREACH = WSAEHOSTUNREACH;
  ENOTEMPTY = WSAENOTEMPTY;
  EPROCLIM = WSAEPROCLIM;
  EUSERS = WSAEUSERS;
  EDQUOT = WSAEDQUOT;
  ESTALE = WSAESTALE;
  EREMOTE = WSAEREMOTE;

{ Socket routines }

function socket(af, struct, protocol: LongInt): TSocketHandle; apicall; libsocket;
function shutdown(s: TSocketHandle; how: LongInt): LongInt; apicall; libsocket;
function connect(s: TSocketHandle; addr: PSockAddr; namelen: LongInt): TSocketHandle; apicall; libsocket;
function bind(s: TSocketHandle; addr: PSockAddr; namelen: LongInt): LongInt; apicall; libsocket;
function listen(s: TSocketHandle; backlog: LongInt): LongInt; apicall; libsocket;
function accept(s: TSocketHandle; addr: PSockAddr; var addrlen: LongInt): TSocketHandle; apicall; libsocket;
function close(s: TSocketHandle): LongInt; apicall; libsocket {$ifdef windows}name 'closesocket'{$endif};
function send(s: TSocketHandle; var buf; len, flags: LongWord): LongInt; apicall; libsocket;
function sendto(s: TSocketHandle; var buf; len, flags: LongWord; var addrto: TSockAddr; tolen: LongInt): LongInt; apicall; libsocket;
function recv(s: TSocketHandle; var buf; len, flags: LongWord): LongInt; apicall; libsocket;
function recvfrom(s: TSocketHandle; var buf; len, flags: LongWord; var from: TSockAddr; var fromlen: LongInt): LongInt; apicall; libsocket;
function setsockopt(s: TSocketHandle; level, optname: LongInt; optval: Pointer; optlen: LongInt): LongInt; apicall; libsocket;

{ Socket utility routines }

function getpeername(s: TSocketHandle; var name: TSockAddr; var namelen: LongInt): LongInt; apicall; libsocket;
function getsockname(s: TSocketHandle; var name: TSockAddr; var namelen: LongInt): LongInt; apicall; libsocket;
function getsockopt(s: TSocketHandle; level, optname: LongInt; optval: PAnsiChar; var optlen: LongInt): LongInt; apicall; libsocket;
function htonl(hostlong: LongInt): LongInt; apicall; libsocket;
function htons(hostshort: Word): Word; apicall; libsocket;
function inet_addr(cp: PAnsiChar): LongInt; apicall; libsocket;
function inet_ntoa(inaddr: TInAddr): PAnsiChar; apicall; libsocket;
function ntohl(netlong: LongInt): LongInt; apicall; libsocket;
function ntohs(netshort: Word): Word; apicall; libsocket;
function select(nfds: LongInt; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): LongInt; apicall; libsocket;
function gethostbyaddr(addr: Pointer; len, Struct: LongInt): PHostEnt; apicall; libsocket;
function gethostbyname(name: PAnsiChar): PHostEnt; apicall; libsocket;
function gethostname(name: PAnsiChar; len: LongInt): LongInt; apicall; libsocket;
function getservbyport(port: LongInt; proto: PAnsiChar): PServEnt; apicall; libsocket;
function getservbyname(name, proto: PAnsiChar): PServEnt; apicall; libsocket;
function getprotobynumber(proto: LongInt): PProtoEnt; apicall; libsocket;
function getprotobyname(name: PAnsiChar): PProtoEnt; apicall; libsocket;

{$ifdef windows}
const
  WSADESCRIPTION_LEN = 256;
  WSASYS_STATUS_LEN = 128;

type
  PWSAData = ^TWSAData;
  TWSAData = record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  end;

function WSAStartup(version: Word; out WSData: TWSAData): LongInt; apicall; libsocket;
function WSACleanup: LongInt; apicall; libsocket;
procedure WSASetLastError(E: LongInt); apicall; libsocket;
function WSAGetLastError: LongInt; apicall; libsocket;
{$endif}

function SocketsInit: Boolean;

implementation

{$ifdef windows}
var
  Initialized: Boolean;

function SocketsInit: Boolean;
var
  Data: TWSAData;
begin
  if Initialized then
    Exit(True);
  Initialized := True;
  WSAStartup($0202, Data);
  Result := True;
end;
{$else}
function SocketsInit: Boolean;
begin
  Result := True;
end;
{$endif}

end.


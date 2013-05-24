(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.networking.web.txt> }
unit Bare.Networking.Web;

{$i bare.inc}

interface

uses
  Bare.System,
  Bare.Text,
  Bare.Networking;

{ Return the default port for a protocol }

function ProtocolPort(const Protocol: string): Word;

{ TUrl parses urls
  See also
  <link Overview.Bare.Game.TUrl, TUrl members>
  <link Bare.Networking.Web.TWebClient, TWebClient class> }

type
  TUrl = record
  private
    FProtocol: string;
    FPort: Word;
    FDomain: string;
    FResource: string;
    FSecure: Boolean;
    FValid: Boolean;
  public
    { Create a TUrl from a string }
    class function Create(const S: string): TUrl; static;
    { Convert a url to a string }
    class operator Implicit(const Value: TUrl): string;
    { Convert a stringt to a url }
    class operator Implicit(const Value: string): TUrl;
    { The protocol e.g. http, https, ftp, ect }
    property Protocol: string read FProtocol;
    { The port number use by a protocol }
    property Port: Word read FPort;
    { The domain name such as stackoverflow.com }
    property Domain: string read FDomain;
    { The resource such as questions?sort=featured }
    property Resource: string read FResource;
    { True if the protcol requires ssl }
    property Secure: Boolean read FSecure;
    { True if the url is properly formed }
    property Valid: Boolean read FValid;
  end;

{ TWebHeader contains the web server's response to an http request
  See also
  <link Overview.Bare.Networking.Web.TWebHeader, TWebHeader members>
  <link Bare.Networking.Web.TWebClient, TWebClient class> }

  TWebHeader = class
  private
    FLines: TStringList;
    FRaw: string;
    FCode: Integer;
    FStatus: string;
    function GetCount: Integer;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
  public
    {doc ignore}
    constructor Create;
    destructor Destroy; override;
    { Clear the response }
    procedure Clear;
    { The raw headers }
    property Raw: string read FRaw;
    { The count of items in the header }
    property Count: Integer read GetCount;
    { The name of a header item }
    property Name[Index: Integer]: string read GetName;
    { The value of a named header item }
    property Value[const Name: string]: string read GetValue; default;
    { The response code, 200 typically means success }
    property Code: Integer read FCode;
    { The response status, OK typically means success }
    property Status: string read FStatus;
  end;

{ Arguments for a web progress event }
  TWebProgressArgs = record
    { Length in bytes of the content being received as communicated by a response header }
    ContentLength: LargeWord;
    { Length in bytes of content received so far }
    ReadLength: LargeWord;
  end;

{ TWebProgressEvent notifies you part of a response has been received }
  TWebProgressEvent = TEventHandler<TWebProgressArgs>;

{TODO: Implement verbs other than GET on TWebClient}

{ TWebClient provides access to a simple http client
  Remarks
  Currently supports only GET requests
  See also
  <link Overview.Bare.Networking.Web.TWebClient, TWebClient members>
  <link topic_networking, Accessing the Internet topic> }

  TWebClient = class
  private
    FHeader: TWebHeader;
    FCancelled: Integer;
    FUserAgent: string;
    FOnCancel: TEmptyEvent;
    FOnHeader: TEmptyEvent;
    FOnProgress: TWebProgressEvent;
    FOnComplete: TEmptyEvent;
    procedure DoCancel;
    procedure DoHeader;
    procedure DoProgress(var Args: TWebProgressArgs);
    procedure DoComplete;
    procedure Reset;
    function GetCancelled: Boolean;
  public
    { Create a new TWebClient }
    constructor Create;
    destructor Destroy; override;
    { Cancel further reading of a response }
    procedure Cancel;
    { Perform an http get request returning the response body as a stream }
    function Get(const Url: TUrl; Stream: TStream): Boolean; overload;
    { Perform an http get request returning the response body as text }
    function Get(const Url: TUrl; out Text: string): Boolean; overload;
    { The client idenity string sent when a request is made }
    property UserAgent: string read FUserAgent write FUserAgent;
    { The response header }
    property Header: TWebHeader read FHeader;
    { Reading of the response was cancelled }
    property Cancelled: Boolean read GetCancelled;
    { Invoked after a response is cancelled }
    property OnCancel: TEmptyEvent read FOnCancel write FOnCancel;
    { Invoked after complete response header has been read }
    property OnHeader: TEmptyEvent read FOnHeader write FOnHeader;
    { Invoked continuously while a response body is read }
    property OnProgress: TWebProgressEvent read FOnProgress write FOnProgress;
    { Invoked when a request completed }
    property OnComplete: TEmptyEvent read FOnComplete write FOnComplete;
  end;

{ Build a get http get request header given a url and optionally a user agent }
function WebGetBuild(const Url: TUrl; const UserAgent: string = ''): string;
{ Download a web resource to a stream }
function WebGet(const Url: TUrl; Stream: TStream; const UserAgent: string = ''): Boolean; overload;
{ Download a web resource to a text }
function WebGet(const Url: TUrl; out Text: string; const UserAgent: string = ''): Boolean; overload;
{ Encodes url parameters }
function UrlEncodeParam(const Param: string): string;

implementation

{ Returns the default port used by a protocol }

function ProtocolPort(const Protocol: string): Word;
begin
  if Protocol = 'FTP' then
    Result := 21
  else if Protocol = 'HTTP' then
    Result := 80
  else if Protocol = 'HTTPS' then
    Result := 443
  else
    Result := 0;
end;

function DomainValidate(const S: string): Boolean;
begin
  {TODO: Write a more compelling domain validator}
  Result := S <> '';
end;

{ Implicitly convert a url to a string }

class operator TUrl.Implicit(const Value: TUrl): string;
begin
  Result := StrLower(Value.FProtocol) + '://' + Value.FDomain;
  if Value.FPort <> ProtocolPort(Value.FProtocol) then
    Result := Result + ':' + IntToStr(Value.FPort);
  if Value.FResource <> '/' then
    Result := Result + Value.FResource;
end;

{ Implicitly convert a string to a url }

class operator TUrl.Implicit(const Value: string): TUrl;
begin
  Result := TUrl.Create(Value);
end;

{ Create a url given a string such as http://stackoverflow.com/questions?sort=featured }

class function TUrl.Create(const S: string): TUrl;
var
  U: string;
  I: Integer;
begin
  Result.FProtocol := 'HTTP';
  U := S;
  I := StrFind(U, '://');
  if I > 0 then
  begin
    Result.FProtocol := StrUpper(StrCopy(U, 1, I - 1));
    U := StrCopy(U, I + 3);
  end;
  Result.FPort := ProtocolPort(Result.FProtocol);
  Result.FResource := '/' + StrSecondOf(U, '/');
  U := StrFirstOf(U, '/');
  Result.FDomain := StrFirstOf(U, ':');
  U := StrSecondOf(U, ':');
  if U <> '' then
    Result.FPort := StrToIntDef(U, Result.FPort);
  Result.FSecure := Result.FProtocol = 'HTTPS';
  Result.FValid := DomainValidate(Result.FDomain) and (Result.FPort > 0);
end;

{ TWebHeader }

const
  WebValueSeparator = ':';

constructor TWebHeader.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TWebHeader.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TWebHeader.Clear;
begin
  FLines.Clear;
  FRaw := '';
  FCode := 0;
  FStatus := '';
end;

function TWebHeader.GetCount: Integer;
begin
  Result := FLines.Count;
end;

function TWebHeader.GetName(Index: Integer): string;
begin
  Result := StrFirstOf(FLines[Index], WebValueSeparator);
  Result := StrTrim(Result);
end;

function TWebHeader.GetValue(const Name: string): string;
var
  A, B: string;
  I: Integer;
begin
  Result := '';
  A := StrUpper(StrTrim(Name));
  if A = '' then
    Exit;
  for I := 0 to FLines.Count - 1 do
  begin
    B := StrUpper(GetName(I));
    if A = B then
      Exit(StrTrim(StrSecondOf(FLines[I], WebValueSeparator)));
  end;
end;

{ TWebClient }

constructor TWebClient.Create;
begin
  inherited Create;
  FHeader := TWebHeader.Create;
end;

destructor TWebClient.Destroy;
begin
  FHeader.Free;
  inherited Destroy;
end;

procedure TWebClient.DoCancel;
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self, EmptyArgs);
end;

procedure TWebClient.DoHeader;
begin
  if Assigned(FOnHeader) then
    FOnHeader(Self, EmptyArgs);
end;

procedure TWebClient.DoProgress(var Args: TWebProgressArgs);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Args);
end;

procedure TWebClient.DoComplete;
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self, EmptyArgs);
end;

procedure TWebClient.Cancel;
begin
  InterlockedIncrement(FCancelled);
end;

procedure TWebClient.Reset;
begin
  FHeader.Clear;
  FCancelled := 0;
end;

function TWebClient.GetCancelled: Boolean;
begin
  Result := FCancelled > 0;
end;

function TWebClient.Get(const Url: TUrl; Stream: TStream): Boolean;
const
  NN = #10#10;
  RR = #13#13;
  RN = #13#10#13#10;
  NR = #10#13#10#13;
var
  Progress: TWebProgressArgs;

  function HeaderType(out Index:  Integer): Integer;
  var
    A: array[0..3] of Integer;
    I: Integer;
  begin
    Result := 0;
    A[0] := StrFind(FHeader.FRaw, NN);
    A[1] := StrFind(FHeader.FRaw, RR);
    A[2] := StrFind(FHeader.FRaw, RN);
    A[3] := StrFind(FHeader.FRaw, NR);
    Index := Length(FHeader.FRaw);
    for I := Low(A) to High(A) do
      if (A[I] > 0) and (A[I] < Index) then
      begin
        Index := A[I];
        Result := I;
      end;
  end;

  procedure ProcessHeader;
  var
    Raw, S: string;
    A: StringArray;
    I, J: Integer;
  begin
    Raw := FHeader.FRaw;
    J := HeaderType(I);
    FHeader.FRaw := StrCopy(FHeader.FRaw, 1, I - 1);
    FHeader.FLines.Text := FHeader.FRaw;
    if FHeader.Count > 0 then
    begin
      S := FHeader.FLines[0];
      FHeader.FLines.Delete(0);
      A := StrSplit(S, ' ');
      if Length(A) > 1 then
        FHeader.FCode := StrToIntDef(A[1], 0);
      if Length(A) > 2 then
        FHeader.FStatus := A[2];
      Progress.ContentLength := StrToIntDef(FHeader['Content-Length'], 0);
      if Progress.ContentLength = 0 then
        Progress.ContentLength := StrToIntDef(FHeader['ContentLength'], 0);
    end;
    DoHeader;
    I := I + (J shr 1 + 1) * 2;
    S := Copy(Raw, I, Length(Raw));
    Progress.ReadLength := Length(S);
    if Progress.ReadLength > 0 then
    begin
      Stream.Write(S[1], Progress.ReadLength);
      DoProgress(Progress);
    end;
  end;

var
  Socket: TSocket;
  BufferSize: LongWord;
  Buffer: Pointer;
  I: Integer;
  S: string;
begin
  Result := False;
  Reset;
  S := WebGetBuild(Url, FUserAgent);
  if S = '' then
  begin
    DoCancel;
    Exit;
  end;
  if Stream = nil then
  begin
    DoCancel;
    Exit(False);
  end;
  Socket := TSocket.Create;
  try
    if not Socket.Connect(Url.Domain, Url.Port) then
    begin
      DoCancel;
      Exit;
    end;
    if Socket.Write(S) <> Length(S) then
    begin
      DoCancel;
      Exit;
    end;
    Progress.ContentLength := 0;
    Progress.ReadLength := 0;
    repeat
      if Socket.Read(S) < 1 then
      begin
        DoCancel;
        Exit;
      end;
      FHeader.FRaw := FHeader.FRaw + S;
      if (Length(FHeader.FRaw) > 10000) or (FCancelled > 0) then
      begin
        DoCancel;
        Exit;
      end;
    until StrContains(FHeader.FRaw, NN) or StrContains(FHeader.FRaw, RR) or
        StrContains(FHeader.FRaw, NR) or StrContains(FHeader.FRaw, RN);
    ProcessHeader;
    BufferSize := Stream.BufferSize;
    Buffer := GetMem(BufferSize);
    try
      while True do
      begin
        I := Socket.Read(Buffer^, BufferSize);
        if I < 1 then
          Break;
        Stream.Write(Buffer^, I);
        Inc(Progress.ReadLength, I);
        if FCancelled > 0 then
        begin
          DoCancel;
          Exit;
        end;
        DoProgress(Progress);
      end;
      Result := True;
    finally
      FreeMem(Buffer);
    end;
  finally
    Socket.Free;
  end;
  DoComplete;
end;

function TWebClient.Get(const Url: TUrl; out Text: string): Boolean;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Result := Get(Url, S);
    if Result then
      Text := S.Data
    else
      Text := '';
  finally
    S.Free;
  end;
end;

function WebGetBuild(const Url: TUrl; const UserAgent: string = ''): string;
var
  S: string;
begin
  if not Url.Valid then
    Exit('');
  Result :=
    'GET ' + Url.Resource + ' HTTP/1.1'#13#10 +
    'Host: ' + Url.Domain + #13#10;
  S := StrTrim(UserAgent);
  if S <> '' then
    Result := Result + 'User-Agent: ' + S + #13#10;
  Result := Result + 'Connection: Close'#13#10#13#10;
end;

function WebGet(const Url: TUrl; Stream: TStream; const UserAgent: string = ''): Boolean;
var
  Request: TWebClient;
begin
  Request := TWebClient.Create;
  try
    Request.UserAgent := UserAgent;
    Result := Request.Get(Url, Stream);
  finally
    Request.Free;
  end;
end;

{ Shortcut function to download a web resources to a string }

function WebGet(const Url: TUrl; out Text: string; const UserAgent: string = ''): Boolean;
var
  Request: TWebClient;
begin
  Request := TWebClient.Create;
  try
    Request.UserAgent := UserAgent;
    Result := Request.Get(Url, Text);
  finally
    Request.Free;
  end;
end;

{ Encodes url parameters }

function UrlEncodeParam(const Param: string): string;
var
  C: AnsiChar;
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Param) do
  begin
    C := Param[I];
    if C in ['-', '_', '0'..'9', 'A'..'Z', 'a'..'z'] then
      Result := Result + C
    else
      Result := Result + '%' + HexStr(Ord(C), 2);
  end;
end;

end.


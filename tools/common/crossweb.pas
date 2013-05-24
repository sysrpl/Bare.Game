unit CrossWeb;

{$i cross.inc}

interface

uses
  SysUtils, Classes, CrossText, CrossNet;

type
  LargeWord = QWord;

function ProtocolPort(const Protocol: string): Word;

{ TUrl }

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
    class function Create(const S: string): TUrl; static;
    class operator Implicit(const Value: TUrl): string;
    class operator Implicit(const Value: string): TUrl;
    property Protocol: string read FProtocol;
    property Port: Word read FPort;
    property Domain: string read FDomain;
    property Resource: string read FResource;
    property Secure: Boolean read FSecure;
    property Valid: Boolean read FValid;
  end;

{ TWebHeader }

  TWebHeader = class
  protected
    FLines: TStringList;
    FRaw: string;
    FCode: Integer;
    FStatus: string;
    function GetCount: Integer;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Raw: string read FRaw;
    property Count: Integer read GetCount;
    property Name[Index: Integer]: string read GetName;
    property Value[const Name: string]: string read GetValue; default;
    property Code: Integer read FCode;
    property Status: string read FStatus;
  end;

  TWebHeaderEvent = procedure(Sender: TObject; Header: TWebHeader) of object;
  TWebProgressEvent = procedure(Sender: TObject; ContentLength, ReadLength: LargeWord) of object;

{ TWebVerbEvents }

  TWebVerbEvents = record
    OnCancel: TNotifyEvent;
    OnHeader: TWebHeaderEvent;
    OnProgress: TWebProgressEvent;
    OnComplete: TNotifyEvent;
  end;

{ TWebClient }

  TWebClient = class
  private
    FHeader: TWebHeader;
    FCancelled: Integer;
    FUserAgent: string;
    FContentLength: LargeWord;
    FReadLength: LargeWord;
    FOnCancel: TNotifyEvent;
    FOnHeader: TWebHeaderEvent;
    FOnProgress: TWebProgressEvent;
    FOnComplete: TNotifyEvent;
    procedure DoCancel;
    procedure Reset;
    function GetCancelled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    function Get(const Url: TUrl; Stream: TStream): Boolean; overload;
    function Get(const Url: TUrl; out Text: string): Boolean; overload;
    property UserAgent: string read FUserAgent write FUserAgent;
    property Header: TWebHeader read FHeader;
    property Cancelled: Boolean read GetCancelled;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnHeader: TWebHeaderEvent read FOnHeader write FOnHeader;
    property OnProgress: TWebProgressEvent read FOnProgress write FOnProgress;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

function WebGetBuild(const Url: TUrl; const UserAgent: string = ''): string;
function WebGet(const Url: TUrl; Stream: TStream; const UserAgent: string = ''): Boolean; overload;
function WebGet(const Url: TUrl; out Text: string; const UserAgent: string = ''): Boolean; overload;
function UrlEncodeParam(const Param: string): string;

implementation

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

class operator TUrl.Implicit(const Value: TUrl): string;
begin
  Result := StrLower(Value.FProtocol) + '://' + Value.FDomain;
  if Value.FPort <> ProtocolPort(Value.FProtocol) then
    Result := Result + ':' + IntToStr(Value.FPort);
  if Value.FResource <> '/' then
    Result := Result + Value.FResource;
end;

class operator TUrl.Implicit(const Value: string): TUrl;
begin
  Result := TUrl.Create(Value);
end;

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
    FOnCancel(Self);
end;

procedure TWebClient.Cancel;
begin
  InterlockedIncrement(FCancelled);
end;

procedure TWebClient.Reset;
begin
  FHeader.Clear;
  FContentLength := 0;
  FReadLength := 0;
  FCancelled := 0;
end;

function TWebClient.GetCancelled: Boolean;
begin
  Result := FCancelled > 0;
end;

function TWebClient.Get(const Url: TUrl; Stream: TStream): Boolean;
const
  BufferSize = 1024 * 4;
const
  NN = #10#10;
  RR = #13#13;
  RN = #13#10#13#10;
  NR = #10#13#10#13;

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
      FContentLength := StrToIntDef(FHeader['Content-Length'], 0);
      if FContentLength = 0 then
        FContentLength := StrToIntDef(FHeader['ContentLength'], 0);
    end;
    if Assigned(FOnHeader) then
      FOnHeader(Self, FHeader);
    I := I + (J shr 1 + 1) * 2;
    S := Copy(Raw, I, Length(Raw));
    FReadLength := Length(S);
    if FReadLength > 0 then
    begin
      Stream.Write(S[1], FReadLength);
      if Assigned(FOnProgress) then
        FOnProgress(Self, FContentLength, FReadLength);
    end;
  end;

var
  Socket: TSocket;
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
    Buffer := GetMem(BufferSize);
    try
      while True do
      begin
        I := Socket.Read(Buffer^, BufferSize);
        if I < 1 then
          Break;
        Stream.Write(Buffer^, I);
        Inc(FReadLength, I);
        if FCancelled > 0 then
        begin
          DoCancel;
          Exit;
        end;
        if Assigned(FOnProgress) then
          FOnProgress(Self, FContentLength, FReadLength);
      end;
      Result := True;
    finally
      FreeMem(Buffer);
    end;
  finally
    Socket.Free;
  end;
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

function TWebClient.Get(const Url: TUrl; out Text: string): Boolean;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    Result := Get(Url, S);
    if Result then
      Text := S.DataString
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


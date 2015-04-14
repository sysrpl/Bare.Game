(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.text.txt> }
unit Bare.Text;

{$i bare.inc}

interface

uses
  Bare.System,
  Bare.Types;

const
  DefaultValueSeparator = '=';

type
  TStringList = class;

  IStringEnumerator = interface(IEnumerator<string>)
  end;

  TStringKeys = class
  private
    FStrings: TStringList;
  public
    function GetEnumerator: IStringEnumerator;
    procedure Add(const Key, Value: string);
    procedure Remove(const Key: string);
    function IndexOf(Key: string): Integer;
    function Find(Key: string): Boolean;
    function Key(Index: Integer): string;
    function Value(Index: Integer): string;
    function GetValue(Key: string): string;
    procedure SetValue(Key: string; const Value: string);
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TStringList = class(TIndexedList<string>)
  private
    FCaseSensitive: Boolean;
    FKeys: TStringKeys;
    FValueSeparator: string;
    function GetKeys: TStringKeys;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure AddItem(constref Item: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    function IndexOf(const Item: string): Integer; override;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property Text: string read GetText write SetText;
    property Keys: TStringKeys read GetKeys;
    property ValueSeparator: string read FValueSeparator write FValueSeparator;
  end;

  TEncodeMethod = (emHex, emBase64);

  IBuffer = interface
    ['{62C3AEC2-A51F-468C-9664-6027FF8722E6}']
    function GetData: Pointer;
    function GetSize: LongWord;
    procedure SetSize(Value: LongWord);
    property Data: Pointer read GetData;
    property Size: LongWord read GetSize write SetSize;
  end;

{ TBuffer manages encoding/decoding binary data }

  TBuffer = record
  private
    FBuffer: IBuffer;
    function GetData: Pointer;
    function GetSize: LongWord;
    procedure SetSize(Value: LongWord);
  public
    class function Create(Size: LongWord): TBuffer; static;
    class operator Implicit(const Value: TBuffer): Pointer;
    property Data: Pointer read GetData;
    property Size: LongWord read GetSize write SetSize;
    function Encode(Method: TEncodeMethod = emBase64): string;
  end;

{ Hex routines }

function HexEncode(Buffer: Pointer; Size: LongWord): string; overload;
function HexEncode(const Buffer: TBuffer): string; overload;
function HexEncode(const S: string): string; overload;
function HexDecode(const S: string): TBuffer;

{ Base64 routines }

function Base64Encode(Buffer: Pointer; Size: LongWord): string; overload;
function Base64Encode(const Buffer: TBuffer): string; overload;
function Base64Encode(const S: string): string; overload;
function Base64Decode(const S: string): TBuffer;

implementation

type
  TStringEnumerator = class(TInterfacedObject, IStringEnumerator)
  private
    FStrings: TStringList;
    FPosition: Integer;
  public
    constructor Create(Strings: TStringList);
    function GetCurrent: string;
    function MoveNext: Boolean;
  end;

constructor TStringEnumerator.Create(Strings: TStringList);
begin
  inherited Create;
  FStrings := Strings;
  FPosition := -1;
end;

function TStringEnumerator.GetCurrent: string;
begin
  Result := StrFirstOf(FStrings[FPosition], FStrings.FValueSeparator);
end;

function TStringEnumerator.MoveNext: Boolean;
var
  S: string;
begin
  Result := False;
  repeat
    Inc(FPosition);
    if FPosition = FStrings.Count then
      Exit(False);
    S := StrTrim(FStrings[FPosition]);
    if S = '' then
      Continue;
  until Result;
end;

{ TStringKeys }

function TStringKeys.GetEnumerator: IStringEnumerator;
begin
  Result := TStringEnumerator.Create(FStrings);
end;

procedure TStringKeys.Add(const Key, Value: string);
begin
  SetValue(Key, Value);
end;

procedure TStringKeys.Remove(const Key: string);
var
  I: Integer;
begin
  I := IndexOf(Key);
  if I > -1 then
    FStrings.Delete(I);
end;

function TStringKeys.IndexOf(Key: string): Integer;
var
  S: string;
  I: Integer;
begin
  Result := -1;
  Key := StrTrim(Key);
  if Key = '' then
    Exit;
  if not FStrings.FCaseSensitive then
    Key := StrUpper(Key);
  for I := 0 to FStrings.Count - 1 do
  begin
    S := StrTrim(FStrings[I]);
    S := StrFirstOf(S, FStrings.FValueSeparator);
    if S = '' then
      Continue;
    if not FStrings.FCaseSensitive then
      S := StrUpper(S);
    if S = Key then
      Exit(I);
  end;
end;

function TStringKeys.Find(Key: string): Boolean;
begin
  Result := IndexOf(Key) > -1;
end;

function TStringKeys.Key(Index: Integer): string;
begin
  Result := StrFirstOf(FStrings[Index], FStrings.FValueSeparator);
  Result := StrTrim(Result);
end;

function TStringKeys.Value(Index: Integer): string;
begin
  Result := StrSecondOf(FStrings[Index], FStrings.FValueSeparator);
  Result := StrTrim(Result);
end;

function TStringKeys.GetValue(Key: string): string;
var
  I: Integer;
begin
  I := IndexOf(Key);
  if I < 0 then
    Result := ''
  else
    Result := StrTrim(StrSecondOf(FStrings[I], FStrings.FValueSeparator));
end;

procedure TStringKeys.SetValue(Key: string; const Value: string);
var
  I: Integer;
begin
  Key := StrTrim(Key);
  if (Key = '') or (Key = FStrings.FValueSeparator) then
    Exit;
  I := IndexOf(Key);
  if I < 0 then
    FStrings.Add(Key + FStrings.FValueSeparator + StrTrim(Value))
  else
    FStrings[I] := Key + FStrings.FValueSeparator + StrTrim(Value);
end;

{ TStringList }

constructor TStringList.Create;
begin
  inherited Create;
  FCaseSensitive := True;
  FValueSeparator := DefaultValueSeparator;
end;

destructor TStringList.Destroy;
begin
  FKeys.Free;
  inherited Destroy;
end;

procedure TStringList.AddItem(constref Item: string);
var
  Lines: IntArray;
  S, B, L: string;
  I, J: Integer;
begin
  S := StrAdjustLineBreaks(Item);
  B := LineEnding;
  Lines := StrFindIndex(S, B);
  if Length(Lines) < 1 then
  begin
    inherited AddItem(S);
    Exit;
  end;
  J := 1;
  for I := Low(Lines) to High(Lines) do
  begin
    L := StrCopy(S, J, Lines[I] - J);
    inherited AddItem(L);
    Inc(J, Length(L) + Length(B));
  end;
  L := StrCopy(S, J, Length(S) + 1);
  inherited AddItem(L);
end;

procedure TStringList.LoadFromFile(const FileName: string);
var
  Stream: TStream;
  I: Integer;
begin
  Clear;
  Stream := TFileStream.Create(FileName, fmOpen);
  try
    I := Stream.Size;
    if I < 1 then
      Exit;
    Text := Stream.ReadStr(I);
  finally
    Stream.Free;
  end;
end;

procedure TStringList.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteStr(Text);
  finally
    Stream.Free;
  end;
end;

procedure TStringList.LoadFromStream(Stream: TStream);
var
  I: Integer;
begin
  Clear;
  if Stream.Cancelled then
    Exit;
  I := Stream.Position - Stream.Size;
  if I < 1 then
    Exit;
  Text := Stream.ReadStr(I);
end;

procedure TStringList.SaveToStream(Stream: TStream);
begin
  if not Stream.Cancelled then
    Stream.WriteStr(Text);
end;

function FindStringI(const A, B: string): Integer;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function FindString(const A, B: string): Integer;
begin
  Result := FindStringI(A, StrUpper(B));
end;

function TStringList.IndexOf(const Item: string): Integer;
begin
  if FCaseSensitive then
    Result := Find(FindString, StrUpper(Item))
  else
    Result := Find(FindStringI, Item);
end;

function TStringList.GetKeys: TStringKeys;
begin
  if FKeys = nil then
  begin
    FKeys := TStringKeys.Create;
    FKeys.FStrings := Self;
  end;
  Result := FKeys;
end;

function TStringList.GetText: string;
var
  S, B: string;
  P: PChar;
  I, J: Integer;
begin
  if Count = 0 then
    Exit('');
  if Count = 1 then
    Exit(Item[0]);
  I := 0;
  J := -1;
  for S in Self do
  begin
    Inc(I, Length(S));
    Inc(J);
  end;
  B := LineEnding;
  SetLength(Result, I + J * Length(B));
  P := PChar(Result);
  for S in Self do
  begin
    Move(PChar(S)^, P^, Length(S));
    Inc(P, Length(S));
    if J > 0 then
    begin
      Move(PChar(B)^, P^, Length(S));
      Inc(P, Length(B));
    end;
    Dec(J);
  end;
end;

procedure TStringList.SetText(const Value: string);
begin
  Clear;
  if Value = '' then
    Exit;
  Add(Value);
end;

{ TBufferObject }

type
  TBufferObject = class(TInterfacedObject, IBuffer)
  private
    FData: Pointer;
    FSize: LongWord;
  public
    constructor Create(Size: LongWord);
    destructor Destroy; override;
    function GetData: Pointer;
    function GetSize: LongWord;
    procedure SetSize(Value: LongWord);
  end;

constructor TBufferObject.Create(Size: LongWord);
begin
  inherited Create;
  FSize := Size;
  if FSize > 0 then
    GetMem(FData, FSize)
  else
    FData := nil;
end;

destructor TBufferObject.Destroy;
begin
  if FData <> nil then
    FreeMem(FData);
  inherited Destroy;
end;

function TBufferObject.GetData: Pointer;
begin
  Result := FData;
end;

function TBufferObject.GetSize: LongWord;
begin
  Result := FSize;
end;

procedure TBufferObject.SetSize(Value: LongWord);
begin
  if Value <> FSize then
  begin
    FSize := Value;
    if FSize > 0 then
    begin
      if FData <> nil then
        ReallocMem(FData, FSize)
      else
        GetMem(FData, FSize);
    end
    else
    begin
      if FData <> nil then
        FreeMem(FData);
      FData := nil;
    end;
  end;
end;

{ TBuffer }

class function TBuffer.Create(Size: LongWord): TBuffer;
begin
  if Size > 0 then
    Result.FBuffer := TBufferObject.Create(Size)
  else
    Result.FBuffer := nil;
end;

class operator TBuffer.Implicit(const Value: TBuffer): Pointer;
begin
  if Value.FBuffer = nil then
    Result := nil
  else
    Result := Value.FBuffer.Data;
end;

function TBuffer.Encode(Method: TEncodeMethod = emBase64): string;
begin
  case Method of
    emHex: Result := HexEncode(Data, Size);
    emBase64: Result := Base64Encode(Data, Size);
  else
    Result := '';
  end;
end;

function TBuffer.GetData: Pointer;
begin
  if FBuffer = nil then
    Result := nil
  else
    Result := FBuffer.Data;
end;

function TBuffer.GetSize: LongWord;
begin
  if FBuffer = nil then
    Result := 0
  else
    Result := FBuffer.Size;
end;

procedure TBuffer.SetSize(Value: LongWord);
begin
  if FBuffer = nil then
    FBuffer := TBufferObject.Create(Value);
  FBuffer.Size := Value;
end;

{ Hex routines }

function HexEncode(Buffer: Pointer; Size: LongWord): string;
const
  Hex: PChar = '0123456789ABCDEF';
var
  B: PByte;
  C: PChar;
begin
  if Size = 0 then
    Exit('');
  SetLength(Result, Size shl 1);
  B := PByte(Buffer);
  C := PChar(Result);
  while Size > 0 do
  begin
    C^ := Hex[B^ shr $4];
    Inc(C);
    C^ := Hex[B^ and $F];
    Inc(C);
    Inc(B);
    Dec(Size);
  end;
end;

function HexEncode(const Buffer: TBuffer): string;
begin
  Result := HexEncode(Buffer.Data, Buffer.Size);
end;

function HexEncode(const S: string): string;
begin
  Result := HexEncode(Pointer(S), Length(S));
end;

function HexDecode(const S: string): TBuffer;
const
  Digit0 = Ord('0');
  DigitA = Ord('A');
var
  B: PByte;
  C: PChar;
  I: Integer;
begin
  I := Length(S);
  if Odd(I) or (I = 0) then
    Exit(TBuffer.Create(0));
  Result := TBuffer.Create(I shr 1);
  B := Result.Data;
  C := PChar(S);
  I := 0;
  repeat
    if C[I] in ['0'..'9'] then
      B^ := (Ord(C[I]) - Digit0) shl $4
    else if C[I] in ['A'..'F'] then
      B^ := (Ord(C[I]) - DigitA + $A) shl $4
    else
      Exit(TBuffer.Create(0));
    Inc(I);
    if C[I] in ['0'..'9'] then
      B^ := B^ or (Ord(C[I]) - Digit0)
    else if C[I] in ['A'..'F'] then
      B^ := B^ or (Ord(C[I]) - DigitA + $A)
    else
      Exit(TBuffer.Create(0));
    Inc(B);
    Inc(I);
  until C[I] = #0;
end;

{ Base64 routines }

function Base64EncodedSize(Size: LongWord): Cardinal;
begin
  Result := (Size div 3) shl 2;
  if (Size mod 3) > 0 then
    Inc(Result, 4);
end;

function Base64Encode(Buffer: Pointer; Size: LongWord): string;
const
  Base64: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Fill: Char = '=';
var
  B: PByte;
  C: Byte;
  I: LongWord;
  J: LongWord;
begin
  SetLength(Result, Base64EncodedSize(Size));
  B := Buffer;
  I := 0;
  J := 0;
  while I < Size do
  begin
    C := (B[I] shr 2) and $3F;
    Inc(J);
    Result[J] := Base64[C];
    C := (B[I] shl 4) and $3f;
    Inc(I);
    if I < Size then
      C := C or ((B[I] shr 4) and $0F);
    Inc(J);
    Result[J] := Base64[C];
    if I < Size then
    begin
      C := (B[I] shl 2) and $3F;
      Inc(I);
      if I < Size then
        C := C or ((B[I] shr 6) and $03);
      Inc(J);
      Result[J] := Base64[C];
    end
    else
    begin
      Inc(I);
      Inc(J);
      Result[J] := Fill;
    end;
    if I < Size then
    begin
      C := B[I] and $3F;
      Inc(J);
      Result[J] := Base64[C];
    end
    else
    begin
      Inc(J);
      Result[J] := Fill;
    end;
    Inc(I);
  end;
end;

function Base64Encode(const Buffer: TBuffer): string;
begin
  Result := Base64Encode(Buffer, Buffer.Size);
end;

function Base64Encode(const S: string): string;
begin
  Result := Base64Encode(Pointer(S), Length(S));
end;

function Base64Decode(const S: string): TBuffer;

  procedure Zero(out Sextext, Index: LongWord); inline;
  begin
    Sextext := 0;
    Inc(Index);
  end;

  function Search(out Sextext, Index: LongWord): Boolean;
  const
    Base64: PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  var
    C: Char;
    I: Integer;
  begin
    Sextext := 0;
    C := S[Index];
    Inc(Index);
    for I := 0 to 63 do
      if C = Base64[I] then
      begin
        Sextext := I;
        Exit(True);
      end;
    Result := False;
  end;

type
  TOutput = array[0..0] of Byte;
  POutput = ^TOutput;
var
  Buffer: TBuffer;
  Output: POutput;
  InLen, OutLen, A, B, C, D, E, I, J: LongWord;
begin
  Result := TBuffer.Create(0);
  InLen := Length(S);
  if (InLen < 1) or (InLen mod 4 <> 0) then
    Exit;
  OutLen := InLen div 4 * 3;
  if S[InLen] = '=' then
    Dec(OutLen);
  if S[InLen - 1] = '=' then
    Dec(OutLen);
  if OutLen < 1 then
    Exit;
  Buffer := TBuffer.Create(OutLen);
  Output := Buffer.Data;
  J := 0;
  I := 1;
  Inc(InLen);
   while I < InLen do
  begin
    if S[I] = '=' then Zero(A, I) else if not Search(A, I) then Exit;
    if S[I] = '=' then Zero(B, I) else if not Search(B, I) then Exit;
    if S[I] = '=' then Zero(C, I) else if not Search(C, I) then Exit;
    if S[I] = '=' then Zero(D, I) else if not Search(D, I) then Exit;
    E := A shl 18 + B shl 12 + C shl 6 + D;
    if J = OutLen then Break;
    Output[J] := E shr 16 and $FF; Inc(J);
    if J = OutLen then Break;
    Output[J] := E shr 8 and $FF; Inc(J);
    if J = OutLen then Break;
    Output[J] := E and $FF; Inc(J);
  end;
  Result := Buffer;
end;

end.

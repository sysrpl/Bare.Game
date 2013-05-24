unit CrossText;

{$i cross.inc}

interface

type
  TArray<T> = array of T;
  StringArray = TArray<string>;
  IntArray = TArray<Integer>;
  
const
  LineBreakStyles: array[TTextLineBreakStyle] of string = (#10, #13#10, #13);

function StrUpper(const S: string): string;
function StrLower(const S: string): string;
function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
function StrCopyData(P: Pointer; Len: Integer): string;
function StrInsert(const S, SubStr: string; Position: Integer): string;
function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer; overload;
function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
function StrTrim(const S: string): string;
function StrEquals(const S: string; const Strings: array of string): Boolean;
function StrIndex(const S: string; const Strings: array of string): Integer;
function StrSplit(const S, Separator: string): StringArray;
function StrFirstOf(const S, Separator: string): string;
function StrSecondOf(const S, Separator: string): string;
function StrLastOf(const S, Separator: string): string;
function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
function StrOf(C: Char; Len: Integer): string;
function StrPadLeft(const S: string; C: Char; Len: Integer): string;
function StrPadRight(const S: string; C: Char; Len: Integer): string;
function StrIsIdent(const S: string): Boolean;
function StrIsAttr(const S: string): Boolean;
function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string; overload;
function StrAdjustLineBreaks(const S: string): string; overload;

implementation

function LineBreak: string;
begin
  Result := LineBreakStyles[DefaultTextLineBreakStyle];
end;


function StrUpper(const S: string): string;
begin
  Result := UpCase(S);
end;

function StrLower(const S: string): string;
begin
  Result := LowerCase(S);
end;

function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
var
  C, D: string;
begin
  if IgnoreCase then
  begin
    C := StrUpper(A);
    D := StrUpper(B);
  end
  else
  begin
    C := A;
    D := B;
  end;
  if C < D then
    Result := -1
  else if C > D then
    Result := 1
  else
    Result := 0;
end;

function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
var
  A, B: PChar;
  I: Integer;
begin
  if S = '' then
    Exit('');
  if Start < 1 then
    Exit('');
  I := Length(S);
  if Start > I then
    Exit('');
  if Len = 0 then
    Len := Length(S);
  Dec(Start);
  if Start + Len > I then
    Len := I - Start;
  Setlength(Result, Len);
  A := PChar(S);
  B := PChar(Result);
  Inc(A, Start);
  Move(A^, B^, Len);
end;

function StrCopyData(P: Pointer; Len: Integer): string;
begin
  if Len < 1 then
    Exit('');
  SetLength(Result, Len);
  Move(P^, PChar(Result)^, Len);
end;

function StrInsert(const S, SubStr: string; Position: Integer): string;
begin
  if Position < 1 then
    Position := 1
  else if Position > Length(S) then
    Position := Length(S);
  if Position = 1 then
    Exit(SubStr + S);
  if Position = Length(S) then
    Exit(S + SubStr);
  Result := StrCopy(S, 1, Position - 1) + SubStr + StrCopy(S, Position);
end;

function StrFindBuffer(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if  (SLen = 0) or (SubStrLen = 0) then
    Exit;
  Dec(S);
  Dec(SubStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(SubStr[I]);
    Lookup[B] := SubStrLen - I;
  end;
  Last := SubStr[SubStrLen];
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := S[I];
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if SubStr[K] <> S[J + K] then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function StrFindBufferI(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(S);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(UpCase(SubStr[I]));
    Lookup[B] := SubStrLen - I;
  end;
  Last := UpCase(SubStr[SubStrLen]);
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := UpCase(S[I]);
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if UpCase(SubStr[K]) <> UpCase(S[J + K]) then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function StrTrim(const S: string): string;
const
  WhiteSpace = [#0..' '];
var
  Len, I: Integer;
begin
  Len := Length(S);
  while (Len > 0) and (S[Len] in WhiteSpace) do
   Dec(Len);
  I := 1;
  while ( I <= Len) and (S[I] in WhiteSpace) do
    Inc(I);
  Result := Copy(S, I, 1 + Len - I);
end;

function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  if IgnoreCase then
    Result := StrFindBufferI(PChar(S), PChar(SubStr), Length(S), Length(SubStr))
  else
    Result := StrFindBuffer(PChar(S), PChar(SubStr), Length(S), Length(SubStr));
end;

function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer;
var
  P: PChar;
  I: Integer;
begin
  P := PChar(S);
  I := Length(S);
  if (Start < 1) or (Start > I) then
  begin
    Result := 0;
    Exit;
  end;
  Dec(Start);
  Inc(P, Start);
  Dec(I, Start);
  if IgnoreCase then
    Result := StrFindBufferI(P, PChar(SubStr), I, Length(SubStr))
  else
    Result := StrFindBuffer(P, PChar(SubStr), I, Length(SubStr));
  if Result > 0 then
    Inc(Result, Start);
end;

function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
var
  Start, Index: Integer;
begin
  Result := 0;
  Start := 1;
  repeat
    Index := StrFind(S, SubStr, Start, IgnoreCase);
    if Index > 0 then
    begin
      Inc(Result);
      Start := Index + 1;
    end;
  until Index = 0;
end;

function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
var
  Start, Index: Integer;
begin
  SetLength(Result, StrFindCount(S, SubStr, IgnoreCase));
  Start := 1;
  Index := 0;
  while Index < Length(Result) do
  begin
    Start := StrFind(S, SubStr, Start, IgnoreCase);
    Result[Index] := Start;
    Inc(Start);
    Inc(Index);
  end;
end;

function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  PosIndex: IntArray;
  Diff: Integer;
  I, J, K, L: Integer;
begin
  PosIndex := StrFindIndex(S, OldPattern, IgnoreCase);
  if Length(PosIndex) = 0 then
  begin
    Result := S;
    Exit;
  end;
  Diff := Length(NewPattern) - Length(OldPattern);
  I := Length(S) + Diff * Length(PosIndex);
  SetLength(Result, I);
  I := 0;
  J := 1;
  K := 1;
  while K <= Length(S) do
  begin
    if K = PosIndex[I] then
    begin
      if I < High(PosIndex) then
        Inc(I);
      Inc(K, Length(OldPattern));
      for L := 1 to Length(NewPattern) do
      begin
        Result[J] := NewPattern[L];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := S[K];
      Inc(J);
      Inc(K);
    end;
  end;
end;

function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  I: Integer;
begin
  I := StrFind(S, OldPattern, IgnoreCase);
  if I > 0 then
    Result := Copy(S, 1, I - 1) + NewPattern + Copy(S, I + Length(OldPattern), Length(S))
  else
    Result := S;
end;

function StrEquals(const S: string; const Strings: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Strings) to High(Strings) do
    if S = Strings[I] then
    begin
      Result := True;
      Break;
    end;
end;

function StrIndex(const S: string; const Strings: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Strings) to High(Strings) do
    if S = Strings[I] then
    begin
      Result := I;
      Break;
    end;
end;

function StrSplit(const S, Separator: string): StringArray;
var
  Splits: IntArray;
  Pos: Integer;
  I: Integer;
begin
  Result := nil;
  Splits := nil;
  if Length(S) < 1 then
    Exit;
  if Length(Separator) < 1 then
    Exit;
  if StrFind(S, Separator) < 1 then
  begin
    SetLength(Result, 1);
    Result[0] := S;
    Exit;
  end;
  Splits := StrFindIndex(S, Separator);
  SetLength(Result, Length(Splits) + 1);
  Pos := 1;
  for I := Low(Splits) to High(Splits) do
  begin
    Result[I] := Copy(S, Pos, Splits[I] - Pos);
    Pos := Splits[I] + Length(Separator);
  end;
  Result[Length(Splits)] := Copy(S, Pos, Length(S));
end;

function StrFirstOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    Result := StrCopy(S, 1, I - 1)
  else
    Result := S;
end;

function StrSecondOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    Result := StrCopy(S, I + Length(Separator))
  else
    Result := '';
end;

function StrLastOf(const S, Separator: string): string;
var
  A: StringArray;
begin
  A := StrSplit(S, Separator);
  if Length(A) > 0 then
    Result := A[Length(A) - 1]
  else
    Result := '';
end;

function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrFind(S, SubStr, IgnoreCase) > 0;
end;

function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  C := StrCopy(S, 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  C := StrCopy(S, Length(S) - Length(SubStr) + 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrOf(C: Char; Len: Integer): string;
var
  I: Integer;
begin
  if Len < 1 then
    Exit;
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := C;
end;

function StrPadLeft(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I < 1 then
    Exit;
  if Len < 1 then
    Exit;
  if I > Len then
  begin
    Result := Copy(S, 1, Len);
    Exit;
  end;
  Result := S + StrOf(C, Len - I);
end;

function StrPadRight(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I < 1 then
    Exit;
  if Len < 1 then
    Exit;
  if I > Len then
  begin
    Result := Copy(S, Len - I, Len);
    Exit;
  end;
  Result := StrOf(C, I - Len) + S;
end;

function IsAlpha(C: Char): Boolean;
begin
  Result := (C >= 'A') and (C <= 'Z');
  if Result then Exit;
  Result := (C >= 'a') and (C <= 'z');
end;

function IsUnderscore(C: Char): Boolean;
begin
  Result := C = '_';
end;

function IsNumeric(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function StrIsIdent(const S: string): Boolean;
var
  AlphaFound: Boolean;
  C: Char;
  I: Integer;
begin
  Result := False;
  if Length(S) < 1 then
    Exit;
  C := S[1];
  AlphaFound := IsAlpha(C);
  if (not AlphaFound) and (not IsUnderscore(C)) then
    Exit;
  for I := 2 to Length(S) do
  begin
    C := S[I];
    AlphaFound := AlphaFound or IsAlpha(C);
    if IsAlpha(C) or IsUnderscore(C) or IsNumeric(C) then
      Continue;
    Exit;
  end;
  Result := AlphaFound;
end;

function StrIsAttr(const S: string): Boolean;
begin
  Result := False;
  if Length(S) < 2 then
    Exit;
  if S[1] <> '@' then
    Exit;
  Result := StrIsIdent(Copy(S, 2, Length(S) - 1));
end;

function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
var
  Count: array[TTextLineBreakStyle] of Integer;
  I: TTextLineBreakStyle;
begin
  for I := Low(Count) to High(Count) do
    Count[I] := StrFindCount(S, LineBreakStyles[I]);
  Result := DefaultTextLineBreakStyle;
  for I := Low(Count) to High(Count) do
    if Count[I] > Count[Result] then
      Result := I;
end;

function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
var
  Line: string;
  I, J: Integer;
begin
  if S = '' then
    Exit('');
  I := StrFindCount(S, #10) + StrFindCount(S, #13);
  SetLength(Result, Length(S) + I);
  Line := LineBreakStyles[Style];
  I := 1;
  J := 1;
  while S[I] > #0  do
  begin
    if ((S[I] = #10) and (S[I + 1] = #13)) or ((S[I] = #13) and (S[I + 1] = #10)) then
    begin
      Result[J] := Line[1];
      Inc(J);
      if Length(Line) > 1 then
      begin
        Result[J] := Line[2];
        Inc(J);
      end;
      Inc(I);
    end
    else if (S[I] = #10) or (S[I] = #13) then
    begin
      Result[J] := Line[1];
      Inc(J);
      if Length(Line) > 1 then
      begin
        Result[J] := Line[2];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := S[I];
      Inc(J);
    end;
    Inc(I);
  end;
  SetLength(Result, J - 1);
end;

function StrAdjustLineBreaks(const S: string): string;
begin
  Result := StrAdjustLineBreaks(S, DefaultTextLineBreakStyle);
end;

end.


program replace;

{$mode delphi}
{$apptype console}

uses
  SysUtils, Classes, FileUtil, StrUtils;

function PathAdjustDelimiters(const Path: string): string;
begin
  if PathDelim = '/' then
    Result := StringReplace(Path, '\', '/', [rfReplaceAll])
  else if PathDelim = '\' then
    Result := StringReplace(Path, '/', '\', [rfReplaceAll])
  else
    Halt(1);
end;

function FindOpen(const Path: string; Attr: Longint; out Search: TSearchRec): LongInt;
begin
  Result := FindFirst(PathAdjustDelimiters(Path), Attr, Search);
end;

function StrPrior(const SubString, Source: string; Index: Integer): Integer;
var
  A, B: string;
  I: Integer;
begin
  Result := 0;
  if (Index < 1) or (Index > Length(Source)) then
    Exit;
  A := Copy(Source, 1, Index - 1);
  if A = '' then
    Exit;
  A := ReverseString(A);
  B := ReverseString(SubString);
  I := Pos(B, A);
  Result := Index - (I + Length(SubString) - 1);
end;

function StrNext(const SubString, Source: string; Index: Integer): Integer;
begin
  Result := PosEx(SubString, Source, Index);
end;

type
  TSection = (scNone, scReplace, scReplaceAll, scReplaceCut, scReplaceMatch, scSource, scDest,
    scMatch, scStart, scFinish);

const
  SectionNames: array[TSection] of string =
    ('', '[replace]', '[replace all]', '[replace cut]', '[replace match]',
      '[source]', '[dest]', '[match]', '[start]', '[finish]');

function StrToSection(S: string): TSection;
var
  I: TSection;
begin
  S := LowerCase(Trim(S));
  for I := Low(SectionNames) to High(SectionNames) do
    if S = SectionNames[I] then
      Exit(I);
  Result := scNone;
end;

type
   TStreamHelper = class helper for TStringStream
     procedure SaveToFile(const FileName: string);
     procedure LoadFromFile(const FileName: string);
   end;

procedure TStreamHelper.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    F.CopyFrom(Self, 0);
  finally
    F.Free;
  end;
end;

procedure TStreamHelper.LoadFromFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    CopyFrom(F, 0);
  finally
    F.Free;
  end;
end;

function Cut(const Content, Match, Start, Finish: string): string;
var
  M, S, F: Integer;
begin
  M := Pos(Match, Content);
  if M < 1 then
    Exit(Content);
  S := StrPrior(Start, Content, M);
  if S < 1 then
    Exit(Content);
  F := StrNext(Finish, Content, M);
  if F < 1 then
    Exit(Content);
  Result := Copy(Content, 1, S - 1) + Copy(Content, F + Length(Finish), Length(Content))
end;

function ReplaceMatch(const Content, Start, Finish, Dest: string): string;
var
  S, F, I: Integer;
  Block: string;
begin
  Result := Content;
  while True do
  begin
    S := Pos(Start, Result);
    if S < 1 then
      Exit(Result);
    F := StrNext(Finish, Result, S);
    if F < 1 then
      Exit(Result);
    I := Length(Start);
    Block := Copy(Result, S + I, F - (S + I));
    Block := StringReplace(Dest, '$match', Block, [rfReplaceAll]);
    Result := Copy(Result, 1, S - 1) + Block + Copy(Result, F + Length(Finish), Length(Result));
  end;
end;

procedure Execute(Action: TSection; const FileName, Source, Dest, Match, Start, Finish: string);
var
  Search: TSearchRec;
  Stream: TStringStream;
  Path, A, B: string;
begin
  if not (Action in [scReplace, scReplaceAll, scReplaceCut, scReplaceMatch]) then
  begin
    WriteLn('stopping on error: expected replace action');
    Halt(1);
  end;
  WriteLn('replacing content in ', FileName);
  if FindOpen(FileName, faAnyFile, Search) = 0 then
  begin
    Path := ExtractFilePath(PathAdjustDelimiters(FileName));
    repeat
      Stream := TStringStream.Create('');
      Stream.LoadFromFile(Path + Search.Name);
      A := AdjustLineBreaks(Stream.DataString);
      Stream.Free;
      if Action = scReplace then
        B := StringReplace(A, Source, Dest, [])
      else if Action = scReplaceAll then
        B := StringReplace(A, Source, Dest, [rfReplaceAll])
      else if Action = scReplaceCut then
        B := Cut(A, Match, Start, Finish)
      else if Action = scReplaceMatch then
        B := ReplaceMatch(A, Start, Finish, Dest);
      if A <> B then
      begin
        Stream := TStringStream.Create(B);
        Stream.SaveToFile(Path + Search.Name);
        Stream.Free;
      end;
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
end;

procedure FindAndReplace(const WorkFile: string);
var
  Work: TStringList;
  Action, Group, Next: TSection;
  Line, FileName, Source, Dest, Match, Start, Finish: string;
begin
  WriteLn(ExtractFileName(ParamStr(0)), ' running');
  Work := TStringList.Create;
  try
    WriteLn('opening work file: ', WorkFile);
    Work.LoadFromFile(WorkFile);
    Group := scNone;
    Next := scNone;
    Source := '';
    Dest := '';
    Match := '';
    Start := '';
    Finish := '';
    for Line in Work do
    begin
      Next := StrToSection(Line);
      if Next = scNone then
        case Group of
          scReplace, scReplaceAll, scReplaceCut, scReplaceMatch: FileName := Line;
          scSource:
            if Source = '' then Source := Line else Source := Source + LineEnding + Line;
          scDest:
            if Dest = '' then Dest := Line else Dest := Dest + LineEnding + Line;
          scMatch:
            if Match = '' then Match := Line else Match := Match + LineEnding + Line;
          scStart:
            if Start = '' then Start := Line else Start := Start + LineEnding + Line;
          scFinish:
            if Finish = '' then Finish := Line else Finish := Finish + LineEnding + Line;
        end;
      if Next = scNone then
        Continue;
      Group := Next;
      if Group in [scReplace, scReplaceAll, scReplaceCut, scReplaceMatch] then
      begin
        if FileName <> '' then
        begin
          if Action in [scReplace, scReplaceAll] then
            if (Source = '') or (Dest = '') then
            begin
              WriteLn('stopping on error: need both source and dest');
              Halt(1);
            end;
          if Action in [scReplaceMatch] then
            if (Dest = '') or (Start = '') or (Finish = '')  then
            begin
              WriteLn('stopping on error: need start, finish, and dest');
              Halt(1);
            end;
          if Action = scReplaceCut then
            if (Match = '') or (Start = '') or (Finish = '') then
            begin
              WriteLn('stopping on error: need match, start, and finish');
              Halt(1);
            end;
          Execute(Action, FileName, Source, Dest, Match, Start, Finish);
        end;
        Action := Group;
        FileName := '';
        Source := '';
        Dest := '';
        Match := '';
        Start := '';
        Finish := '';
      end;
    end;
    if FileName <> '' then
    begin
      if Action in [scReplace, scReplaceAll] then
        if (Source = '') or (Dest = '') then
        begin
          WriteLn('stopping on error: need both source and dest');
          Halt(1);
        end;
      if Action = scReplaceCut then
        if (Match = '') or (Start = '') or (Finish = '') then
        begin
          WriteLn('stopping on error: need match, start, and finish');
          Halt(1);
        end;
      Execute(Action, FileName, Source, Dest, Match, Start, Finish);
    end;
  finally
    Work.Free;
  end;
  WriteLn(ExtractFileName(ParamStr(0)), ' done');
end;

procedure Run;
var
  WorkFile: string;
begin
  WorkFile := ParamStr(1);
  if FileExists(WorkFile) then
    FindAndReplace(WorkFile)
  else
  begin
    WriteLn('usage');
    WriteLn('  ', ExtractFileName(ParamStr(0)), ' <workfile>');
    WriteLn('    finds and replaces in files');
    Halt(1);
  end;
end;

begin
  ExitCode := 0;
  Run;
end.


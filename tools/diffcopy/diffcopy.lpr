program diffcopy;

uses
  SysUtils, Classes, FileUtil;

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

procedure DoCopy(const Source, Dest: string; var Counter: Integer);
begin
  if CopyFile(Source, Dest, True) then
    Inc(Counter)
  else
    WriteLn('failed to copy to ', Dest);
end;

procedure CopyFiles;
var
  Search: TSearchRec;
  SourceDir, DestDir: string;
  A,  B: string;
  C, D: TMemoryStream;
  I, J: Integer;
begin
  SourceDir := ExtractFilePath(ParamStr(1));
  DestDir := IncludeTrailingPathDelimiter(SourceDir);
  DestDir := ExtractFilePath(ParamStr(2));
  if not DirectoryExists(DestDir) then
  begin
    WriteLn('stopping on error: destination directory not found ', DestDir);
    Halt(1);
  end;
  DestDir := IncludeTrailingPathDelimiter(DestDir);
  I := 0;
  J := 0;
  if FindOpen(ParamStr(1), faAnyFile and (not faDirectory), Search) = 0 then
  begin
    repeat
      Inc(I);
      A := SourceDir + Search.Name;
      B := DestDir + Search.Name;
      if not FileExists(B) then
        DoCopy(A, B, J)
      else
      begin
        C := TMemoryStream.Create;
        D := TMemoryStream.Create;
        C.LoadFromFile(A);
        D.LoadFromFile(B);
        if C.Size <> D.Size then
          DoCopy(A, B, J)
        else if not CompareMem(C.Memory, D.Memory, C.Size) then
          DoCopy(A, B, J);
        C.Free;
        D.Free;
      end;
    until FindNext(Search) <> 0;
    FindClose(Search);
    WriteLn(I, ' file(s) were found');
    WriteLn(J, ' different file(s) copied');
  end
  else
    WriteLn('no files to copy');
end;

procedure Run;
begin
  if ParamCount = 2 then
    CopyFiles
  else
  begin
    WriteLn('usage');
    WriteLn('  ', ExtractFileName(ParamStr(0)), ' <source file pattern> <destination folder>');
    WriteLn('    copies files if they are have different contents');
    Halt(1);
  end;
end;

begin
  ExitCode := 0;
  Run;
end.


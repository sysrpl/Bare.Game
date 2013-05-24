unit CrossConfig;

{$i cross.inc}

interface

uses
  Classes, SysUtils, FileUtil, CrossFiles;

procedure SetConfig(FileName, InstallPath: string);
procedure UnsetConfig(FileName, InstallPath: string);
procedure SetConfigAll(InstallPath: string);
procedure UnsetConfigAll(InstallPath: string);

const
  ShortcutName = 'Bare Game Lazarus';
  ShortcutFileName = 'Bare Game Lazarus.lnk';
  ShortcutWebName = 'Bare Game Website';
  ShortcutWebFileName = 'Bare Game Website.lnk';

implementation

const
  InstallPathVar = '$install_path';

procedure SetConfig(FileName, InstallPath: string);
begin
  FileFindReplace(FileName, InstallPathVar, ExcludeTrailingPathDelimiter(InstallPath){$ifdef windows}, True{$endif});
end;

procedure UnsetConfig(FileName, InstallPath: string);
begin
  FileFindReplace(FileName, ExcludeTrailingPathDelimiter(InstallPath), InstallPathVar{$ifdef windows}, True{$endif});
end;

function FindOpen(const FileName: string; Attr: Longint; out Search: TSearchRec): LongInt;
begin
  Result := FindFirst(FileName, Attr, Search);
end;

function FindGccVersion(Path: string): string;
var
  Strings: TStringList;
var
  Search: TSearchRec;
  Name: string;
begin
  Result := '';
  if Path = '' then
    Exit;
  Strings := TStringList.Create;
  try
    if FindOpen(Path + '/*', faAnyFile, Search) = 0 then
    begin
      repeat
        Name := PathCombine(Path, Search.Name);
        if not DirectoryExists(Name) then
          Continue;
        if not (Search.Name[1] in ['1'..'9']) then
          Continue;
        Strings.Add(Name);
      until FindNext(Search) <> 0;
      FindClose(Search);
    end;
    Strings.Sorted := True;
    if Strings.Count > 0 then
      Result := Strings[Strings.Count - 1];
  finally
    Strings.Free;
  end;
end;

{$ifdef linux}
procedure SetConfigLinux(FileName: string);
const
  lib_gnu = '$lib_gnu';
  gcc_i386 = '$gcc_i386';
  gcc_x86_64 = '$gcc_x86_64';
var
  gcc32: string;
  gcc64: string;
begin
  {$ifdef linux_i386}
  if DirectoryExists('/usr/lib/i386-linux-gnu') then
    FileFindReplace(FileName, lib_gnu, '/usr/lib/i386-linux-gnu')
  else if DirectoryExists('/usr/lib/i386-linux') then
    FileFindReplace(FileName, lib_gnu, '/usr/lib/i386-linux')
  else if DirectoryExists('/usr/lib/i686-linux-gnu') then
    FileFindReplace(FileName, lib_gnu, '/usr/lib/i686-linux-gnu')
  else if DirectoryExists('/usr/lib/i686-linux') then
    FileFindReplace(FileName, lib_gnu, '/usr/lib/i686-linux')
  else
    FileFindReplace(FileName, lib_gnu, '/usr/lib');
  {$endif}
  {$ifdef linux_x86_64}
  if DirectoryExists('/usr/lib/x86_64-linux-gnu') then
    FileFindReplace(FileName, lib_gnu, '/usr/lib/x86_64-linux-gnu')
  else if DirectoryExists('/usr/lib/x86_64-linux') then
    FileFindReplace(FileName, lib_gnu, '/usr/lib/x86_64-linux')
  else
    FileFindReplace(FileName, lib_gnu, '/usr/lib');
  {$endif}
  if DirectoryExists('/usr/lib/gcc/x86_64-linux-gnu') then
    gcc64 := '/usr/lib/gcc/x86_64-linux-gnu'
  else if DirectoryExists('/usr/lib/gcc/x86_64-linux') then
    gcc64 := '/usr/lib/gcc/x86_64-linux'
  else if DirectoryExists('/usr/lib/gcc') then
    gcc64 := '/usr/lib/gcc'
  else
    gcc64 := '';
  if DirectoryExists('/usr/lib/gcc/i686-linux-gnu') then
    gcc32 := '/usr/lib/gcc/i686-linux-gnu'
  else if DirectoryExists('/usr/lib/gcc/i686-linux') then
    gcc32 := '/usr/lib/gcc/i686-linux'
  else if DirectoryExists('/usr/lib/gcc/i386-linux-gnu') then
    gcc32 := '/usr/lib/gcc/i386-linux-gnu'
  else if DirectoryExists('/usr/lib/gcc/i386-linux') then
    gcc32 := '/usr/lib/gcc/i386-linux'
  else if DirectoryExists('/usr/lib/gcc') then
    gcc32 := '/usr/lib/gcc'
  else
    gcc32 := '';
  gcc32 := FindGccVersion(gcc32);
  if gcc32 <> '' then
    FileFindReplace(FileName, gcc_i386, gcc32);
  gcc64 := FindGccVersion(gcc64);
  if gcc64 <> '' then
    FileFindReplace(FileName, gcc_x86_64, gcc64);
end;
{$endif}

{$ifdef windows}
procedure CreateWindowsShortcut(const Path: string);
var
  Args: TCreateShortcutArgs;
begin
  Args.LinkPath := Path;
  Args.TargetPath := 'http://www.baregame.org';
  Args.Name := ShortcutWebName;
  CreateShortcut(Args);
  if FileExists(PathCombine(Path, 'lazarus\lazarus.exe')) then
  begin
    Args.WorkingPath := PathCombine(Path, 'lazarus');
    Args.TargetPath := PathCombine(Args.WorkingPath, 'lazarus.exe');
    Args.Name := ShortcutName;
    Args.Arguments := '--primary-config-path=config';
    CreateShortcut(Args);
  end;
end;
{$endif}

procedure SetConfigAll(InstallPath: string);
var
  S: string;
begin
  S := PathCombine(InstallPath, 'lazarus');
  SetConfig(S + '/config/*.*', InstallPath);
  {$ifdef linux}
  SetConfig(S + '/*.sh', InstallPath);
  SetConfig(S + '/install/*.desktop', InstallPath);
  DeleteFile(S + '/fpc.cfg');
  if FileExists(S + '/fpc.unset.cfg') then
	  CopyFile(S + '/fpc.unset.cfg', S + '/fpc.cfg');
  SetConfig(S + '/fpc.cfg', InstallPath);
  SetConfigLinux(S + '/fpc.cfg');
  {$endif}
  {$ifdef windows}
  SetConfig(S + '\*.bat', InstallPath);
  S := ExtractFilePath(S);
	DeleteFile(PathCombine(S, ShortcutWebFileName));
	DeleteFile(PathCombine(S, ShortcutFileName));
  CreateWindowsShortcut(S);
  S := PathCombine(S, 'fpc\bin\i386-win32\fpc.cfg');
  SetConfig(S, InstallPath);
  {$endif}
end;

procedure UnsetConfigAll(InstallPath: string);
var
  S: string;
begin
  S := PathCombine(InstallPath, 'lazarus');
  UnsetConfig(S + '/config/*.*', InstallPath);
  {$ifdef linux}
  UnsetConfig(S + '/install/*.desktop', InstallPath);
  UnsetConfig(S + '/*.sh', InstallPath);
  DeleteFile(S + '/fpc.cfg');
  {$endif}
  {$ifdef windows}
  UnsetConfig(S + '\*.bat', InstallPath);
  S := ExtractFilePath(S);
	DeleteFile(PathCombine(S, ShortcutWebFileName));
  DeleteFile(PathCombine(S, ShortcutFileName));
  S := PathCombine(S, 'fpc\bin\i386-win32\fpc.cfg');
  UnsetConfig(S, InstallPath);
  {$endif}
end;

end.

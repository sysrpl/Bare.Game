unit CrossFiles;

{$i cross.inc}

interface

uses
	{$ifdef windows}
	Windows, ShlObj, ActiveX, ComObj,
	{$endif}
  Classes, SysUtils, FileUtil, CustomTimer, Process, CrossText, CrossWeb;

const
  PlatformName =
  {$ifdef windows}
  'win';
  {$endif}
  {$ifdef linux_i386}
  'linux-i386';
  {$endif}
  {$ifdef linux_x86_64}
  'linux-x86_64';
  {$endif}
  PlatformBigName =
  {$ifdef windows}
  'Windows';
  {$endif}
  {$ifdef linux_i386}
  'Linux 32-bit';
  {$endif}
  {$ifdef linux_x86_64}
  'Linux 64-bit';
  {$endif}

function DirTempName(Path: string): string;
function FileRead(const FileName: string): string;
procedure FileWrite(const FileName, Value: string);
function FileTempName(Path: string): string;
function FileOverwrite(FileName: string): Boolean;
procedure FileFindReplace(const FileName, Find, Replace: string; IgnoreCase: Boolean = False);

function PathAdjustDelimiters(Path: string): string;
function PathCombine(A, B: string): string;
function PathCompare(A, B: string): Boolean;
function PathValidRoot(Path: string): Boolean;

{ TFileOperationAsync }

type
  TFileOperationAsync = class
  private
    FTimer: TCustomTimer;
    FThread: TThread;
    FFiles: TStrings;
    FFileIndex: Integer;
    FOnProgress: TNotifyEvent;
    FOnComplete: TNotifyEvent;
  protected
    procedure Start;
    procedure Stop;
    function GetRunning: Boolean;
    procedure DoTimer(Sender: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel; virtual;
    property Files: TStrings read FFiles;
    property FileIndex: Integer read FFileIndex;
    property Running: Boolean read GetRunning;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

{ TFileDownload }

  TFileDownload = class(TFileOperationAsync)
  private
    FClient: TWebClient;
    FDownloads: TStrings;
    FTempFolder: string;
    FFolder: string;
    FComplete: Integer;
    FContentLength: LongInt;
    FReadLength: LongInt;
  protected
    procedure DoCancel(Sender: TObject);
    procedure DoProgress(Sender: TObject; ContentLength, ReadLength: LargeWord);
    procedure DoComplete(Sender: TObject);
    procedure DoTimer(Sender: TObject); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel; override;
    function Download(const FileName: string): Boolean; overload;
    function Download(Files: TStrings): Boolean; overload;
    function ProgressText(out Position: Integer): string;
    property TempFolder: string read FTempFolder write FTempFolder;
    property ContentLength: LongInt read FContentLength;
    property ReadLength: LongInt read FReadLength;
  end;

{ TFileExtract }

  TFileExtract = class(TFileOperationAsync)
  private
    FComplete: Integer;
    FInstallFolder: string;
    FUnzip: string;
  protected
    procedure DoTimer(Sender: TObject); override;
  public
    procedure Cancel; override;
    function Extract(Files: TStrings): Boolean;
    property Files: TStrings read FFiles;
    property Unzip: string read FUnzip write FUnzip;
    property InstallFolder: string read FInstallFolder write FInstallFolder;
  end;

procedure RunProgram(const FileName: string; Params: array of string);
procedure RunBackgroundProgram(const FileName: string; Params: array of string);

{$ifdef windows}
type
  TCreateShortcutArgs = record
    LinkPath: string;
    TargetPath: string;
    WorkingPath: string;
    Name: string;
    Arguments: string;
    Icon: string;
  end;

function CreateShortcut(const Args: TCreateShortcutArgs): Boolean;
function PathDesktop: string;
function PathStartMenu: string;
function PathWindows: string;
{$endif}

implementation

function DirTempName(Path: string): string;
begin
  Result := PathCombine(GetTempDir, Path);
  if not DirectoryExists(Result) then
    CreateDir(Result);
  if not DirectoryExists(Result) then
    Result := '';
end;

function FileRead(const FileName: string): string;
var
  S: TFileStream;
  I: Integer;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  S := TFileStream.Create(FileName, fmOpenRead);
  try
    I := S.Size;
    if I > 0 then
    begin
      SetLength(Result, I);
      S.Read(Result[1], I);
    end;
  finally
    S.Free;
  end;
end;

procedure FileWrite(const FileName, Value: string);
var
  S: TFileStream;
  I: Integer;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    I := Length(Value);
    if I > 0 then
    begin
      S.Write(Value[1], I);
    end;
  finally
    S.Free;
  end;
end;

function FileTempName(Path: string): string;
begin
  Result := Path;
  if not DirectoryExists(Result) then
    CreateDir(Result);
  if not DirectoryExists(Result) then
    Exit('');
  Result := GetTempFilename(Result, 'tmp');
end;

function FileOverwrite(FileName: string): Boolean;
var
  S: TFileStream;
begin
  DeleteFile(FileName);
  if FileExists(FileName) then
    Result := False
  else
  try
    Result := True;
    S := TFileStream.Create(FileName, fmCreate);
    FreeAndNil(S);
    DeleteFile(FileName);
  except
    Result := False;
  end;
end;

function PathAdjustDelimiters(Path: string): string;
begin
  if DirectorySeparator = '/' then
    Result := StringReplace(Path, '\', '/', [rfReplaceAll])
  else
    Result := StringReplace(Path, '/', '\', [rfReplaceAll]);
end;

function PathValidRoot(Path: string): Boolean;
begin
  if Path = '' then
    Exit(False);
  {$ifdef windows}
  Result := ExtractFileDrive(Path) <> '';
  {$else}
  Result := Path[1] = '/';
  {$endif}
end;

function PathCombine(A, B: string): string;
begin
  Result := PathAdjustDelimiters(IncludeTrailingPathDelimiter(A) + B);
end;

function PathCompare(A, B: string): Boolean;
begin
  {$ifdef windows}
  Result := LowerCase(A) = LowerCase(B);
  {$else}
  Result := A = B;
  {$endif}
end;

constructor TFileOperationAsync.Create;
begin
  inherited Create;
  FTimer := TCustomTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := DoTimer;
  FFileIndex := -1;
  FFiles := TStringList.Create;
end;

destructor TFileOperationAsync.Destroy;
begin
  Stop;
  Cancel;
  FTimer.Free;
  FFiles.Free;
  inherited Destroy;
end;

procedure TFileOperationAsync.Start;
begin
  FTimer.Enabled := True;
end;

procedure TFileOperationAsync.Stop;
begin
  FTimer.Enabled := False;
end;

procedure TFileOperationAsync.Cancel;
begin
  Stop;
  FFileIndex := -1;
end;

function TFileOperationAsync.GetRunning: Boolean;
begin
  Result := (FThread <> nil) and (not FThread.Finished);
  if not Result then
    FreeAndNil(FThread);
end;

procedure TFileOperationAsync.DoTimer(Sender: TObject);
begin
  if Running then
    if Assigned(FOnProgress) then
      FOnProgress(Self);
end;

{ TFileDownload }

type
  TFileDownloadThread = class(TThread)
  private
    FClient: TWebClient;
    FUrl: string;
    FFolder: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Client: TWebClient; const Url, Folder: string);
  end;

constructor TFileDownloadThread.Create(Client: TWebClient; const Url, Folder: string);
begin
  FClient := Client;
  FUrl := Url;
  FFolder := Folder;
  inherited Create(False);
end;

procedure TFileDownloadThread.Execute;
var
  TempName, FileName: string;
  Stream: TStream;
begin
  TempName := FileTempName(FFolder);
  FileName := PathCombine(FFolder, ExtractFileName(FUrl));
  Stream := TFileStream.Create(TempName, fmCreate);
  try
    FClient.Get(FUrl, Stream);
  finally
    Stream.Free;
  end;
  if FClient.Cancelled or (FClient.Header.Code <> 200) then
    DeleteFile(TempName)
  else
    RenameFile(TempName, FileName);
end;

constructor TFileDownload.Create;
begin
  inherited Create;
  FDownloads := TStringList.Create;
  FClient := TWebClient.Create;
  FClient.OnCancel := DoCancel;
  FClient.OnProgress := DoProgress;
  FClient.OnComplete := DoComplete;
end;

destructor TFileDownload.Destroy;
begin
  Cancel;
  FClient.Free;
  FDownloads.Free;
  inherited Destroy;
end;

procedure TFileDownload.Cancel;
begin
  inherited Cancel;
  FClient.Cancel;
  FreeAndNil(FThread);
  FComplete := 0;
  FContentLength := 0;
  FReadLength := 0;
end;

function TFileDownload.Download(const FileName: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    S.Add(FileName);
    Result := Download(S);
  finally
    S.Free;
  end;
end;

function TFileDownload.Download(Files: TStrings): Boolean;
var
  S: string;
begin
  Result := False;
  Cancel;
  FDownloads.Assign(Files);
  FFiles.Clear;
  FFolder := DirTempName(FTempFolder);
  if FFolder = '' then
    Exit;
  for S in FDownloads do
  begin
    S := ExtractFileName(S);
    S := PathCombine(FFolder, S);
    if not FileOverwrite(S) then
      Exit;
    FFiles.Add(S);
  end;
  FComplete := 1;
  Result := True;
  Start;
end;

function TFileDownload.ProgressText(out Position: Integer): string;
const
  KiloByte = 1024;
  MegaByte = KiloByte * 1024;
var
  FileName, Progress: string;
  I, J: Single;
begin
  if (FileIndex < 0) or (FileIndex > Files.Count - 1) then
  begin
    Position := 0;
    Exit('');
  end;
  FileName := ExtractFileName(Files[FileIndex]);
  I := ContentLength;
  J := ReadLength;
  if I > 0 then
  begin
    Position := Round(J / I * 1000);
    if I > MegaByte * 5 then
      Result := Format('Downloading %s progress %.1f of %.1f MB', [FileName, J / MegaByte, I / MegaByte])
    else if I > KiloByte * 10 then
      Result := Format('Downloading %s progress %.1f of %.1f KB', [FileName, J / KiloByte, I / KiloByte])
    else
      Result := Format('Downloading %s progress %.0f of %.0f bytes', [FileName, J, I]);
  end
  else
  begin
    Position := 0;
    if J > MegaByte * 5 then
      Result := Format('Downloading %s %.1f MB', [FileName, J / MegaByte])
    else if J > KiloByte * 10 then
      Result := Format('Downloading %s %.1f KB', [FileName, J / KiloByte])
    else
      Result := Format('Downloading %s %.0f bytes', [FileName, J]);
  end;
end;

procedure TFileDownload.DoCancel(Sender: TObject);
begin
  InterLockedIncrement(FComplete);
end;

procedure TFileDownload.DoProgress(Sender: TObject; ContentLength, ReadLength: LargeWord);
var
  I: LongInt;
begin
  I := ContentLength;
  InterLockedExchange(FContentLength, I);
  I := ReadLength;
  InterLockedExchange(FReadLength, I);
end;

procedure TFileDownload.DoComplete(Sender: TObject);
begin
  InterLockedIncrement(FComplete);
end;

procedure TFileDownload.DoTimer(Sender: TObject);
begin
  if FComplete > 0 then
  begin
    FreeAndNil(FThread);
    FComplete := 0;
    FContentLength := 0;
    FReadLength := 0;
    Inc(FFileIndex);
    if FFileIndex < FDownloads.Count then
      FThread := TFileDownloadThread.Create(FClient, FDownloads[FFileIndex], FFolder)
    else
    begin
      FFileIndex := -1;
      Stop;
      if Assigned(FOnComplete) then
        FOnComplete(Self);
    end;
  end
  else
    inherited DoTimer(Sender);
end;

{ TFileExtract }

type
  TFileExtractThread = class(TThread)
  private
    FComplete: PInteger;
    FUnzip: string;
    FFileName: string;
    FLocation: string;
  protected
    procedure Execute; override;
  public
    constructor Create(var Complete: Integer; const Unzip, FileName, Location: string);
  end;

constructor TFileExtractThread.Create(var Complete: Integer; const Unzip, FileName, Location: string);
begin
  FComplete := @Complete;
  FUnzip := Unzip;
  FFileName := FileName;
  FLocation := Location;
  inherited Create(False);
end;

procedure TFileExtractThread.Execute;
var
  Path, FileName, Folder: string;
  A, B: string;
begin
  try
    ForceDirectory(IncludeTrailingPathDelimiter(FLocation));
    Path := ExtractFilePath(FFileName);
    FileName := ExtractFileName(FFileName);
    Folder := StrFirstOf(FileName, '.');
    A := PathCombine(Path, Folder);
    A := IncludeTrailingPathDelimiter(A);
    DeleteDirectory(A, True);
    RemoveDir(A);
    B := PathCombine(FLocation, Folder);
    B := IncludeTrailingPathDelimiter(B);
    DeleteDirectory(B, True);
    RemoveDir(B);
    RunBackgroundProgram(FUnzip, ['x', FFileName, '-o' + Path]);
    //ExecuteProcess('/bin/tar', ['-C', Path, '-xvf', FFileName]);
    RenameFile(A, B);
    DeleteFile(FFileName);
  finally
    InterLockedIncrement(FComplete^);
  end;
end;

procedure TFileExtract.Cancel;
begin
  inherited Cancel;
  FreeAndNil(FThread);
  FComplete := 0;
end;

function TFileExtract.Extract(Files: TStrings): Boolean;
var
  S: string;
begin
  Result := False;
  Cancel;
  FFiles.Assign(Files);
  for S in FFiles do
  begin
    if not FileExists(S) then
     Exit;
  end;
  FComplete := 1;
  Result := True;
  Start;
end;

procedure TFileExtract.DoTimer(Sender: TObject);
begin
  if FComplete > 0 then
  begin
    FreeAndNil(FThread);
    FComplete := 0;
    Inc(FFileIndex);
    if FFileIndex < FFiles.Count then
      FThread := TFileExtractThread.Create(FComplete, FUnzip,
        FFiles[FFileIndex], FInstallFolder)
    else
    begin
      FFileIndex := -1;
      Stop;
      if Assigned(FOnComplete) then
        FOnComplete(Self);
    end;
  end
  else
    inherited DoTimer(Sender);
end;

function FindOpen(const FileName: string; Attr: Longint; out Search: TSearchRec): LongInt;
begin
  Result := FindFirst(FileName, Attr, Search);
end;

procedure FileFindReplace(const FileName, Find, Replace: string; IgnoreCase: Boolean = False);
var
  Search: TSearchRec;
  Path, Name, Content: string;
  Flags: TReplaceFlags;
  S: string;
begin
  Flags := [rfReplaceAll];
  if IgnoreCase then
  	Flags := Flags + [rfIgnoreCase];
  S := PathAdjustDelimiters(FileName);
  if FindOpen(S, faAnyFile, Search) = 0 then
  begin
    Path := ExtractFilePath(S);
    repeat
      Name := PathCombine(Path, Search.Name);
      if DirectoryExists(Name) then
        Continue;
      Content := FileRead(Name);
      Content := StringReplace(Content, Find, Replace, Flags);
      FileWrite(Name, Content);
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
end;

procedure RunProgram(const FileName: string; Params: array of string);
var
  Process: TProcess;
  S: string;
begin
  S := FileName;
  Process := TProcess.Create(nil);
  try
    if Pos(' ', FileName) > 0 then
     S := '"' + S + '"';
    Process.Executable := S;
    S := FileName;
    S := ExtractFilePath(S);
    if Pos(' ', FileName) > 0 then
     S := '"' + S + '"';
    Process.CurrentDirectory := S;
    for S in Params do
      if Pos(' ', S) > 0 then
        Process.Parameters.Add('"' + S + '"')
      else
        Process.Parameters.Add(S);
    Process.Execute;
  finally
    Process.Free;
  end;
end;

procedure RunBackgroundProgram(const FileName: string; Params: array of string);
var
  Process: TProcess;
  S: string;
begin
  S := FileName;
  Process := TProcess.Create(nil);
  try
    if Pos(' ', FileName) > 0 then
     S := '"' + S + '"';
    Process.Executable := S;
    for S in Params do
      if Pos(' ', S) > 0 then
        Process.Parameters.Add('"' + S + '"')
      else
        Process.Parameters.Add(S);
    Process.ShowWindow := swoHIDE;
    Process.Execute;
    Process.WaitOnExit;
  finally
    Process.Free;
  end;
end;

{$ifdef windows}
function CreateShortcut(const Args: TCreateShortcutArgs): Boolean;
var
  Link: IShellLink;
  Persist: IPersistFile;
  FileName: WideString;
begin
  Link := CreateComObject(CLSID_ShellLink) as IShellLink;
  Persist := Link as IPersistFile;
  Link.SetPath(PChar(Args.TargetPath));
  if Args.WorkingPath <> '' then
	  Link.SetWorkingDirectory(PChar(Args.WorkingPath));
  if Args.Arguments <> '' then
	  Link.SetArguments(PChar(Args.Arguments));
  if Args.Icon <> '' then
	  Link.SetIconLocation(PChar(Args.Icon), 0);
  FileName := PathCombine(Args.LinkPath, Args.Name + '.lnk');
  Result := Persist.Save(PWideChar(FileName), False) = S_OK;
end;

function PathLocate(Folder: Integer): string;
var
  Pidl: PItemIDList;
  Path: array[0..MAX_PATH] of Char;
begin
  SHGetSpecialFolderLocation(0, Folder, Pidl);
  SHGetPathFromIDList(Pidl, Path);
  Result := Path;
end;

function PathDesktop: string;
begin
	Result := PathLocate(CSIDL_DESKTOP);
end;

function PathStartMenu: string;
begin
	Result := PathLocate(CSIDL_STARTMENU);
end;

function PathWindows: string;
var
  Path: array[0..MAX_PATH] of Char;
begin
	GetWindowsDirectoryA(Path, SizeOf(Path));
  Result := Path;
end;

{$endif}

end.



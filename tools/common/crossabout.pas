unit CrossAbout;

{$i cross.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, LCLIntf,
  { Cross units }
  CrossFiles, CrossGraphics, CrossWeb;

{ TCrossAboutForm }

type
  TCrossAboutForm = class(TForm)
    Banner: TImage;
    ProgressBar: TProgressBar;
    UpdateButton: TButton;
    CloseButton: TButton;
    InfoLabel: TLabel;
    SystemLabel: TLabel;
    TagLineLabel: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure UpdateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FShadow: TBitmap;
    FInstallFolder: string;
    FFileDownload: TFileDownload;
    FFileExtract: TFileExtract;
    procedure FileDownloadProgress(Sender: TObject);
    procedure FileDownloadComplete(Sender: TObject);
    procedure FileExtractProgress(Sender: TObject);
    procedure FileExtractComplete(Sender: TObject);
    procedure UpdateVersion;
    function VersionFile: string;
    function VersionMatch(const S: string): Boolean;
  end;

implementation

{$R *.lfm}

{ TCrossAboutForm }

procedure TCrossAboutForm.FormCreate(Sender: TObject);
var
  S: string;
begin
  SystemLabel.Caption := 'for ' + PlatformBigName;
  ClientWidth := CloseButton.Left + CloseButton.Width + 8;
  ClientHeight := CloseButton.Top + CloseButton.Height + 8;
  FFileDownload := TFileDownload.Create;
  FFileDownload.TempFolder := 'Bare';
  FFileDownload.OnProgress := FileDownloadProgress;
  FFileDownload.OnComplete := FileDownloadComplete;
  FFileExtract := TFileExtract.Create;
  FFileExtract.OnProgress := FileExtractProgress;
  FFileExtract.OnComplete := FileExtractComplete;
  {$ifdef windows}
  S := ExtractFilePath(ParamStr(0));
  S := PathCombine(S, '7za.exe');
  {$endif}
  {$ifdef linux}
  S := '/usr/bin/7za';
  if not FileExists(S) then
  	S := '/bin/7za';
  {$endif}
  FFileExtract.Unzip := S;
end;

procedure TCrossAboutForm.FormDestroy(Sender: TObject);
begin
  FShadow.Free;
  FFileDownload.Free;
  FFileExtract.Free;
end;

procedure TCrossAboutForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FFileDownload.Running or FFileExtract.Running;
  CanClose := not CanClose;
end;

procedure TCrossAboutForm.FileDownloadProgress(Sender: TObject);
var
  Position: Integer;
begin
  InfoLabel.Caption := FFileDownload.ProgressText(Position);
  if Position > 0 then
  begin
    ProgressBar.Style := pbstNormal;
    ProgressBar.Position := Position;
  end
  else
    ProgressBar.Style := pbstMarquee;
end;

procedure TCrossAboutForm.FileDownloadComplete(Sender: TObject);
var
  S: string;
begin
  FFileExtract.InstallFolder := FInstallFolder;
  ProgressBar.Style := pbstMarquee;
  for S in FFileDownload.Files do
    if not FileExists(S) then
    begin
      MessageDlg('Some files could not be downloaded', mtError, [mbOk], 0);
      Close;
      Exit;
    end;
  InfoLabel.Caption := 'Download complete';
  FFileExtract.Extract(FFileDownload.Files);
end;

procedure TCrossAboutForm.FileExtractProgress(Sender: TObject);
var
  FileName: string;
begin
  FileName := ExtractFileName(FFileExtract.Files[FFileExtract.FileIndex]);
  InfoLabel.Caption := 'Extracting ' + FileName;
end;

procedure TCrossAboutForm.FileExtractComplete(Sender: TObject);
const
  WhatsNewUrl = 'http://www.baregame.org/go/?visit=whats_new';
begin
  InfoLabel.Caption := 'Update complete';
  InfoLabel.Alignment := taCenter;
  InfoLabel.Layout := tlCenter;
  ProgressBar.Style := pbstNormal;
  ProgressBar.Visible := False;
  CloseButton.Enabled := True;
  OpenURL(WhatsNewUrl);
end;

procedure TCrossAboutForm.UpdateVersion;
const
  UpdateUrl = 'http://download.baregame.org/bare.7z';
begin
  SetFocus;
  InfoLabel.Caption := 'Downloading update ...';
  ProgressBar.Visible := True;
  UpdateButton.Visible := False;
  CloseButton.Enabled := False;
  if not FFileDownload.Download(UpdateUrl) then
  begin
    MessageDlg('Unable to write to download location', mtError, [mbOK], 0);
    Close;
  end;
end;

function TCrossAboutForm.VersionFile: string;
var
  A, B: string;
begin
  Result := '';
  A := ParamStr(0);
  while A <> '' do
  begin
    B := ExtractFileName(A);
    if B = 'Bare' then
    begin
      FInstallFolder := A;
      Exit(PathCombine(A, 'bare/bin/version'));
    end;
    if B = '' then
       Exit;
    A := ExcludeTrailingPathDelimiter(ExtractFilePath(A));
  end;
end;

function TCrossAboutForm.VersionMatch(const S: string): Boolean;
var
  FileName, Dir: string;
begin
  FileName := VersionFile;
  if FileName = '' then
     Exit(True);
  if FileExists(FileName) then
    Result := Trim(FileRead(FileName)) = S
  else
  begin
    Result := True;
    Dir := ExtractFilePath(FileName);
    IncludeTrailingPathDelimiter(Dir);
    CreateDir(Dir);
    if DirectoryExists(Dir) then
      FileWrite(FileName, S);
  end;
end;

procedure TCrossAboutForm.UpdateButtonClick(Sender: TObject);
const
  VersionUrl = 'http://www.baregame.org/version/';
var
  S: string;
begin
  if WebGet(VersionUrl, S, 'lazarus') then
    if VersionMatch(S) then
    begin
      UpdateButton.Caption := 'Your version is current';
      UpdateButton.Enabled := False;
      UpdateButton.OnClick := nil;
    end
    else if MessageDlg('An update was found. Do you want to install it now?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      UpdateVersion;
end;

procedure TCrossAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TCrossAboutForm.FormPaint(Sender: TObject);
var
  Color: TColor;
  Rect: TRect;
begin
  Color := GetDefaultColor(dctBrush);
  Rect := ClientRect;
  DrawEmpty(Canvas, Rect, Color);
  Rect.Top := CloseButton.Top - 8;
  if FShadow = nil then
    FShadow := CreateShadowBitmap(Canvas, Rect, Color);
  Canvas.Draw(Rect.Left, Rect.Top, FShadow);
end;

end.


unit BareGameDescriptors;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, LazIDEIntf, ProjectIntf, MenuIntf;

{ TBareGameProjectDescriptor }

type
  TBareGameProjectDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  TBareGameUnitDescriptor = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    procedure Execute(Sender: TObject);
    function CreateSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

implementation

{ TProjectSimpleProgramDescriptor }

constructor TBareGameProjectDescriptor.Create;
begin
  inherited Create;
  Name := 'Game Project';
  Flags := Flags - [pfMainUnitHasCreateFormStatements,
    pfMainUnitHasTitleStatement] + [pfUseDefaultCompilerOptions];
end;

function TBareGameProjectDescriptor.GetLocalizedName: string;
begin
  Result:= 'Game Project';
end;

function TBareGameProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := GetLocalizedName + LineEnding + LineEnding +
   'A game built using the bare game library.' + LineEnding +
  LineEnding +
  'Visit http://www.baregame.org for instructions, tutorials, and demos.'
end;

function TBareGameProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  Source: string;
  MainFile: TLazProjectFile;
begin
  Result := inherited InitProject(AProject);
  MainFile := AProject.CreateProjectFile('game1.lpr');
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False);
  AProject.MainFileID := 0;
  Source:='program Game1;' + LineEnding +
    LineEnding +
    '{$mode delphi}' + LineEnding +
    LineEnding +
    'uses' + LineEnding +
    '  Bare.Game;' + LineEnding +
    LineEnding +
    '{ TWindow1 }' + LineEnding +
    LineEnding +
    'type' + LineEnding +
    '  TWindow1 = class(TWindow)' + LineEnding +
    '  protected' + LineEnding +
    '    procedure Logic(Stopwatch: TStopwatch); override;' + LineEnding +
    '    procedure Render(Stopwatch: TStopwatch); override;' + LineEnding +
    '  end;' + LineEnding +
    LineEnding +
    'procedure TWindow1.Logic(Stopwatch: TStopwatch);' + LineEnding +
    'begin' + LineEnding +
    '  { Place your game logic code here }' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'procedure TWindow1.Render(Stopwatch: TStopwatch);' + LineEnding +
    'begin' + LineEnding +
    '  { Place your game rendering code here }' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    'begin' + LineEnding +
    '  Application.Run(TWindow1);' + LineEnding +
    'end.' + LineEnding +
    LineEnding;
  AProject.MainFile.SetSourceText(Source);
  AProject.LazCompilerOptions.UnitOutputDirectory := 'lib' + PathDelim + '$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:= 'game1';
  AProject.LazCompilerOptions.SyntaxMode := 'delphi';
  AProject.LazCompilerOptions.UseAnsiStrings := True;
  AProject.LazCompilerOptions.Win32GraphicApp := True;
  AProject.AddPackageDependency('barerun');
end;

function TBareGameProjectDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result := LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename, -1, -1,
    [ofProjectLoading, ofRegularFile]);
end;

{ TFileDescPascalUnit }

constructor TBareGameUnitDescriptor.Create;
begin
  inherited Create;
  Name := 'Game Unit';
  DefaultFilename := 'gameunit.pas';
  DefaultSourceName := 'GameUnit1';
  IsPascalUnit := true;
end;

function TBareGameUnitDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:=
     'unit '+ SourceName+ ';' + LineEnding +
    LineEnding +
    '{$mode delphi}' + LineEnding +
    LineEnding +
    'interface' + LineEnding +
    LineEnding +
    'uses' + LineEnding +
    '  Bare.Game;' + LineEnding +
    LineEnding +
    '{ Place the public interface to your game logic or rendering code here }' + LineEnding +
    LineEnding +
    'implementation' + LineEnding +
    LineEnding +
    '{ Place your implementation or private code here }' + LineEnding +
    LineEnding +
    'end.' + LineEnding +
    LineEnding;
end;

function TBareGameUnitDescriptor.GetLocalizedName: string;
begin
  Result:='Game Unit';
end;

function TBareGameUnitDescriptor.GetLocalizedDescription: string;
begin
  Result:= 'A unit utilizing the Bare.Game library';
end;

procedure TBareGameUnitDescriptor.Execute(Sender: TObject);
begin
  LazarusIDE.DoNewEditorFile(Self, '', '', [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
end;

end.

program blank;

{$mode delphi}

uses
  Bare.Game,
  Bare.Interop.OpenGL;

{ TBlankWindow }

type
  TBlankWindow = class(TWindow)
  protected
    procedure Logic(Stopwatch: TStopwatch); override;
    procedure Render(Stopwatch: TStopwatch); override;
  end;

procedure TBlankWindow.Logic(Stopwatch: TStopwatch);
begin
  { Place your game logic code here }
end;

procedure TBlankWindow.Render(Stopwatch: TStopwatch);
begin
  glLoadIdentity;
  glClearColor(0, 0, 1, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

begin
  Application.Run(TBlankWindow);
end.


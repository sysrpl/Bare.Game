# Bare.Game

Bare Game is a open source modern minimal game cross platform gaming library. It was conceived to take the forthcoming SDL 2.0 game library and pair it with the Free Pascal Compiler, combining Free Pascal's write once compile anywhere philosophy with with SDL 2.0 ability to empower games on every platform. Bear Game wraps SDL 2.0 functions and defines in an extremely easy to reuse class library. It takes care of all the boilerplate plumbing required to get a game started and running on a variety of platforms, empowering you, the game creator, to focus on what you want to do, designing your game. 
Bare Game has several features which endeavor to simplify tasks for game makers including: 

Creating and managing one or more game windows
Reading mouse keyboard gamepad and touch input states
Accessing a hardware accelerated graphics pipeline and using shader programs
Loading and saving image resources using a variety of formats
Playing back and mixing audio
Creating animations and storyboards
Drawing in two dimensions with vector graphics and sprites
Network communications

## Install

Install SDL 2 on your system. Ubuntu users type `sudo apt get install libsdl2-2.0-0`. Windows users download [SDL 2](https://www.libsdl.org/download-2.0.php) and place SDL2.dll in your `C:\Windows\System32` folder.

Install Free Pascal fixes 3.0 and Lazarus, either using [getlazarus.org](http://www.getlazarus.org/setup) or [make it yourself](http://www.getlazarus.org/setup/making). 

If you have git installed type `git clone git@github.com:sysrpl/Bare.Game.git` to git Bare.Game. Otherwise download a [zip file](https://github.com/sysrpl/Bare.Game/archive/master.zip) from github and extract it.

Open Lazaurs, goto file "Packages | Open Package File" and browse to "Bare.Game/source/barerun.lpk". Press compile.

Goto file "Packages | Open Package File" and browse to "Bare.Game/tools/design/baredsgn.lpk". Press "Use | Install".

If you recieve and error in splash.pas with a value of fsBlack, change it to clBlack, save the file, and repeat the step above.

When Lazarus resstart selct "File | New ... | Project | Game Project". You should see a unit list like this.

```program Game1;

{$mode delphi}

uses
  Bare.Game;

{ TWindow1 }

type
  TWindow1 = class(TWindow)
  protected
    procedure Logic(Stopwatch: TStopwatch); override;
    procedure Render(Stopwatch: TStopwatch); override;
  end;

procedure TWindow1.Logic(Stopwatch: TStopwatch);
begin
  { Place your game logic code here }
end;

procedure TWindow1.Render(Stopwatch: TStopwatch);
begin
  { Place your game rendering code here }
end;

begin
  Application.Run(TWindow1);
end.```

## More information

Bare Game also and has built in multithreaded support for separate game logic, render code, and user interface code. 

Bare Game is free and available for testing now. If you choose to use the installer it will setup SDL 2.0 on your system and the Bare Game library on your computer. You'll have the option to install the Free Pascal Compiler, along with the Lazarus IDE which has been enhanced the Bare Game extensions. 

If you choose to follow our progress we'll be updating this website frequently, providing more documentation, tutorials, and we'll be listening to your feedback in making enhancements to Bare Game. 

If you want to take part in the Bare Game community you can join us on the comminity forums.

http://www.getlazarus.org/forums/viewforum.php?f=17

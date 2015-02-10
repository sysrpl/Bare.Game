![Alt text](/splash.gif?raw=true "Bare Game")

[http://www.baregame.org](Bare Game) is a open source modern minimal game cross platform gaming library. It was conceived to take the SDL 2.0 game library and pair it with the Free Pascal Compiler, combining Free Pascal's write once compile anywhere philosophy with with SDL 2.0 ability to empower games on every platform. Bear Game wraps SDL 2.0 functions and defines in an extremely easy to reuse class library. It takes care of all the boilerplate plumbing required to get a game started and running on a variety of platforms, empowering you, the game creator, to focus on what you want to do, designing your game. 

## Links

- [http://www.baregame.org](Official Bare Game Website) 
- [http://www.baregame.org/#bare_game](Official Bare Game API Documentation) 

## Install

Install SDL 2 on your system. Ubuntu users type `sudo apt get install libsdl2-2.0-0`. Windows users download [SDL 2](https://www.libsdl.org/download-2.0.php), extract the zip file, and place SDL2.dll in your `C:\Windows\System32` folder.

Install Free Pascal fixes 3.0 and Lazarus, either using [getlazarus.org](http://www.getlazarus.org/setup) or [make it yourself](http://www.getlazarus.org/setup/making). 

If you have git installed type `git clone git@github.com:sysrpl/Bare.Game.git` to git Bare.Game. Otherwise download a [zip file](https://github.com/sysrpl/Bare.Game/archive/master.zip) from github and extract it.

Open Lazaurs, from the main menu select "Packages | Open Package File" and browse to "Bare.Game/source/barerun.lpk". In the packge window press compile.

From the Lazarus main menu select "Packages | Open Package File" and browse to "Bare.Game/tools/design/baredsgn.lpk". In the packge window press "Use | Install". Lazarus will rebuild itself.

If you recieve and error in splash.pas with a value of fsBlack, change it to clBlack, save the file, and repeat the step above.

When Lazarus restarts, select "File | New ... | Project | Game Project" from the main menu. You should see this skeletal program listing.

```
program Game1;

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
end.
```

Press F9 to run the example. You should see a black window with the title "TWindow1". Next open the project "Bare.Game/examples/draw/draw.lpi" and press F9. You should see a window with an animated neon cursor and some instructional text. If both of those test pass, you've got a working install of Bare Game.

## More information

Bare Game has several features which endeavor to simplify tasks for game makers including: 

- Creating and managing one or more game windows
- Reading mouse keyboard gamepad and touch input states
- Accessing a hardware accelerated graphics pipeline and using shader programs
- Loading and saving image resources using a variety of formats
- Playing back and mixing audio
- Creating animations and storyboards
- Drawing in two dimensions with vector graphics and sprites
- Network communications

Bare Game also and has built in multithreaded support for separate game logic, render code, and user interface code. 

If you want to take part in Bare Game discussions join us on the [community forums](http://www.getlazarus.org/forums/viewforum.php?f=17).

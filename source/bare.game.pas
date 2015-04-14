(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.game.txt> }
unit Bare.Game;

{$i bare.inc}

interface

{TODO: Add clipboard class}
{TODO: Add modifiers signifying left and right modifiers, win key, caps lock ect}

uses
  { Bare.System needs to be the first unit as it sets up threading }
  Bare.System,
  { BareTypes defines basic types and container classes }
  Bare.Types,
  { Bare.Game bridges Bare.Interop.SDL and Bare.Interop.OpenGL }
  Bare.Interop.OpenGL,
  Bare.Interop.SDL2;

type
  { Button enumeration used by <link Bare.Game.TMessageButtons, TMessageButtons> }
  TMessageButton = (mbOk, mbCancel, mbYes, mbNo, mbAll, mbAbort, mbRetry, mbIgnore);
  { Button set used by MessageBox }
  TMessageButtons = set of TMessageButton;
  { Modal result return from MessageBox }
  TModalResult = (mrNone, mrOk, mrCancel, mrYes, mrNo, mrAll, mrAbort, mrRetry, mrIgnore);
  { Style used by MessageBox }
  TMessageType = (mtInformation, mtConfirmation, mtWarning, mtError);

{ Show a message box }
procedure MessageBox(const Prompt: string); overload;
{ Show a message box with an icon and standard title }
procedure MessageBox(Kind: TMessageType; const Prompt: string); overload;
{ Show a message box with an icon, standard title, and some buttons }
function MessageBox(Kind: TMessageType; const Prompt: string; Buttons: TMessageButtons): TModalResult; overload;
{ Show a message box with an icon, some buttons, and a custom title }
function MessageBox(Kind: TMessageType; const Title, Prompt: string; Buttons: TMessageButtons): TModalResult; overload;

{$region virtual keys copied here for convience}
{ Keyboard codes
  See also
  <link Bare.Game.TKeyboardArgs, TKeyboardArgs type>
  <link Bare.Game.TMessage, TMessage.KeyboardArgs> }

type
  TVirtualKeyCode = (
    VK_BACKSPACE = SDLK_BACKSPACE,
    VK_TAB = SDLK_TAB,
    VK_RETURN = SDLK_RETURN,
    VK_ESCAPE = SDLK_ESCAPE,
    VK_SPACE = SDLK_SPACE,
    VK_EXCLAIM = SDLK_EXCLAIM,
    VK_QUOTEDBL = SDLK_QUOTEDBL,
    VK_HASH = SDLK_HASH,
    VK_DOLLAR = SDLK_DOLLAR,
    VK_PERCENT = SDLK_PERCENT,
    VK_AMPERSAND = SDLK_AMPERSAND,
    VK_QUOTE = SDLK_QUOTE,
    VK_LEFTPAREN = SDLK_LEFTPAREN,
    VK_RIGHTPAREN = SDLK_RIGHTPAREN,
    VK_ASTERISK = SDLK_ASTERISK,
    VK_PLUS = SDLK_PLUS,
    VK_COMMA = SDLK_COMMA,
    VK_MINUS = SDLK_MINUS,
    VK_PERIOD = SDLK_PERIOD,
    VK_SLASH = SDLK_SLASH,
    VK_0 = SDLK_0,
    VK_1 = SDLK_1,
    VK_2 = SDLK_2,
    VK_3 = SDLK_3,
    VK_4 = SDLK_4,
    VK_5 = SDLK_5,
    VK_6 = SDLK_6,
    VK_7 = SDLK_7,
    VK_8 = SDLK_8,
    VK_9 = SDLK_9,
    VK_COLON = SDLK_COLON,
    VK_SEMICOLON = SDLK_SEMICOLON,
    VK_LESS = SDLK_LESS,
    VK_EQUALS = SDLK_EQUALS,
    VK_GREATER = SDLK_GREATER,
    VK_QUESTION = SDLK_QUESTION,
    VK_AT = SDLK_AT,
    VK_LEFTBRACKET = SDLK_LEFTBRACKET,
    VK_BACKSLASH = SDLK_BACKSLASH,
    VK_RIGHTBRACKET = SDLK_RIGHTBRACKET,
    VK_CARET = SDLK_CARET,
    VK_UNDERSCORE = SDLK_UNDERSCORE,
    VK_BACKQUOTE = SDLK_BACKQUOTE,
    VK_a = SDLK_a,
    VK_b = SDLK_b,
    VK_c = SDLK_c,
    VK_d = SDLK_d,
    VK_e = SDLK_e,
    VK_f = SDLK_f,
    VK_g = SDLK_g,
    VK_h = SDLK_h,
    VK_i = SDLK_i,
    VK_j = SDLK_j,
    VK_k = SDLK_k,
    VK_l = SDLK_l,
    VK_m = SDLK_m,
    VK_n = SDLK_n,
    VK_o = SDLK_o,
    VK_p = SDLK_p,
    VK_q = SDLK_q,
    VK_r = SDLK_r,
    VK_s = SDLK_s,
    VK_t = SDLK_t,
    VK_u = SDLK_u,
    VK_v = SDLK_v,
    VK_w = SDLK_w,
    VK_x = SDLK_x,
    VK_y = SDLK_y,
    VK_z = SDLK_z,
    VK_DELETE = SDLK_DELETE,
    VK_CAPSLOCK = SDLK_CAPSLOCK,
    VK_F1 = SDLK_F1,
    VK_F2 = SDLK_F2,
    VK_F3 = SDLK_F3,
    VK_F4 = SDLK_F4,
    VK_F5 = SDLK_F5,
    VK_F6 = SDLK_F6,
    VK_F7 = SDLK_F7,
    VK_F8 = SDLK_F8,
    VK_F9 = SDLK_F9,
    VK_F10 = SDLK_F10,
    VK_F11 = SDLK_F11,
    VK_F12 = SDLK_F12,
    VK_PRINTSCREEN = SDLK_PRINTSCREEN,
    VK_SCROLLLOCK = SDLK_SCROLLLOCK,
    VK_PAUSE = SDLK_PAUSE,
    VK_INSERT = SDLK_INSERT,
    VK_HOME = SDLK_HOME,
    VK_PAGEUP = SDLK_PAGEUP,
    VK_END = SDLK_END,
    VK_PAGEDOWN = SDLK_PAGEDOWN,
    VK_RIGHT = SDLK_RIGHT,
    VK_LEFT = SDLK_LEFT,
    VK_DOWN = SDLK_DOWN,
    VK_UP = SDLK_UP,
    VK_NUMLOCKCLEAR = SDLK_NUMLOCKCLEAR,
    VK_KP_DIVIDE = SDLK_KP_DIVIDE,
    VK_KP_MULTIPLY = SDLK_KP_MULTIPLY,
    VK_KP_MINUS = SDLK_KP_MINUS,
    VK_KP_PLUS = SDLK_KP_PLUS,
    VK_KP_ENTER = SDLK_KP_ENTER,
    VK_KP_1 = SDLK_KP_1,
    VK_KP_2 = SDLK_KP_2,
    VK_KP_3 = SDLK_KP_3,
    VK_KP_4 = SDLK_KP_4,
    VK_KP_5 = SDLK_KP_5,
    VK_KP_6 = SDLK_KP_6,
    VK_KP_7 = SDLK_KP_7,
    VK_KP_8 = SDLK_KP_8,
    VK_KP_9 = SDLK_KP_9,
    VK_KP_0 = SDLK_KP_0,
    VK_KP_PERIOD = SDLK_KP_PERIOD,
    VK_APPLICATION = SDLK_APPLICATION,
    VK_POWER = SDLK_POWER,
    VK_KP_EQUALS = SDLK_KP_EQUALS,
    VK_F13 = SDLK_F13,
    VK_F14 = SDLK_F14,
    VK_F15 = SDLK_F15,
    VK_F16 = SDLK_F16,
    VK_F17 = SDLK_F17,
    VK_F18 = SDLK_F18,
    VK_F19 = SDLK_F19,
    VK_F20 = SDLK_F20,
    VK_F21 = SDLK_F21,
    VK_F22 = SDLK_F22,
    VK_F23 = SDLK_F23,
    VK_F24 = SDLK_F24,
    VK_EXECUTE = SDLK_EXECUTE,
    VK_HELP = SDLK_HELP,
    VK_MENU = SDLK_MENU,
    VK_SELECT = SDLK_SELECT,
    VK_STOP = SDLK_STOP,
    VK_AGAIN = SDLK_AGAIN,
    VK_UNDO = SDLK_UNDO,
    VK_CUT = SDLK_CUT,
    VK_COPY = SDLK_COPY,
    VK_PASTE = SDLK_PASTE,
    VK_FIND = SDLK_FIND,
    VK_MUTE = SDLK_MUTE,
    VK_VOLUMEUP = SDLK_VOLUMEUP,
    VK_VOLUMEDOWN = SDLK_VOLUMEDOWN,
    VK_KP_COMMA = SDLK_KP_COMMA,
    VK_KP_EQUALSAS400 = SDLK_KP_EQUALSAS400,
    VK_ALTERASE = SDLK_ALTERASE,
    VK_SYSREQ = SDLK_SYSREQ,
    VK_CANCEL = SDLK_CANCEL,
    VK_CLEAR = SDLK_CLEAR,
    VK_PRIOR = SDLK_PRIOR,
    VK_RETURN2 = SDLK_RETURN2,
    VK_SEPARATOR = SDLK_SEPARATOR,
    VK_OUT = SDLK_OUT,
    VK_OPER = SDLK_OPER,
    VK_CLEARAGAIN = SDLK_CLEARAGAIN,
    VK_CRSEL = SDLK_CRSEL,
    VK_EXSEL = SDLK_EXSEL,
    VK_KP_00 = SDLK_KP_00,
    VK_KP_000 = SDLK_KP_000,
    VK_THOUSANDSSEPARATOR = SDLK_THOUSANDSSEPARATOR,
    VK_DECIMALSEPARATOR = SDLK_DECIMALSEPARATOR,
    VK_CURRENCYUNIT = SDLK_CURRENCYUNIT,
    VK_CURRENCYSUBUNIT = SDLK_CURRENCYSUBUNIT,
    VK_KP_LEFTPAREN = SDLK_KP_LEFTPAREN,
    VK_KP_RIGHTPAREN = SDLK_KP_RIGHTPAREN,
    VK_KP_LEFTBRACE = SDLK_KP_LEFTBRACE,
    VK_KP_RIGHTBRACE = SDLK_KP_RIGHTBRACE,
    VK_KP_TAB = SDLK_KP_TAB,
    VK_KP_BACKSPACE = SDLK_KP_BACKSPACE,
    VK_KP_A = SDLK_KP_A,
    VK_KP_B = SDLK_KP_B,
    VK_KP_C = SDLK_KP_C,
    VK_KP_D = SDLK_KP_D,
    VK_KP_E = SDLK_KP_E,
    VK_KP_F = SDLK_KP_F,
    VK_KP_XOR = SDLK_KP_XOR,
    VK_KP_POWER = SDLK_KP_POWER,
    VK_KP_PERCENT = SDLK_KP_PERCENT,
    VK_KP_LESS = SDLK_KP_LESS,
    VK_KP_GREATER = SDLK_KP_GREATER,
    VK_KP_AMPERSAND = SDLK_KP_AMPERSAND,
    VK_KP_DBLAMPERSAND = SDLK_KP_DBLAMPERSAND,
    VK_KP_VERTICALBAR = SDLK_KP_VERTICALBAR,
    VK_KP_DBLVERTICALBAR = SDLK_KP_DBLVERTICALBAR,
    VK_KP_COLON = SDLK_KP_COLON,
    VK_KP_HASH = SDLK_KP_HASH,
    VK_KP_SPACE = SDLK_KP_SPACE,
    VK_KP_AT = SDLK_KP_AT,
    VK_KP_EXCLAM = SDLK_KP_EXCLAM,
    VK_KP_MEMSTORE = SDLK_KP_MEMSTORE,
    VK_KP_MEMRECALL = SDLK_KP_MEMRECALL,
    VK_KP_MEMCLEAR = SDLK_KP_MEMCLEAR,
    VK_KP_MEMADD = SDLK_KP_MEMADD,
    VK_KP_MEMSUBTRACT = SDLK_KP_MEMSUBTRACT,
    VK_KP_MEMMULTIPLY = SDLK_KP_MEMMULTIPLY,
    VK_KP_MEMDIVIDE = SDLK_KP_MEMDIVIDE,
    VK_KP_PLUSMINUS = SDLK_KP_PLUSMINUS,
    VK_KP_CLEAR = SDLK_KP_CLEAR,
    VK_KP_CLEARENTRY = SDLK_KP_CLEARENTRY,
    VK_KP_BINARY = SDLK_KP_BINARY,
    VK_KP_OCTAL = SDLK_KP_OCTAL,
    VK_KP_DECIMAL = SDLK_KP_DECIMAL,
    VK_KP_HEXADECIMAL = SDLK_KP_HEXADECIMAL,
    VK_LCTRL = SDLK_LCTRL,
    VK_LSHIFT = SDLK_LSHIFT,
    VK_LALT = SDLK_LALT,
    VK_LGUI = SDLK_LGUI,
    VK_RCTRL = SDLK_RCTRL,
    VK_RSHIFT = SDLK_RSHIFT,
    VK_RALT = SDLK_RALT,
    VK_RGUI = SDLK_RGUI,
    VK_MODE = SDLK_MODE,
    VK_AUDIONEXT = SDLK_AUDIONEXT,
    VK_AUDIOPREV = SDLK_AUDIOPREV,
    VK_AUDIOSTOP = SDLK_AUDIOSTOP,
    VK_AUDIOPLAY = SDLK_AUDIOPLAY,
    VK_AUDIOMUTE = SDLK_AUDIOMUTE,
    VK_MEDIASELECT = SDLK_MEDIASELECT,
    VK_WWW = SDLK_WWW,
    VK_MAIL = SDLK_MAIL,
    VK_CALCULATOR = SDLK_CALCULATOR,
    VK_COMPUTER = SDLK_COMPUTER,
    VK_AC_SEARCH = SDLK_AC_SEARCH,
    VK_AC_HOME = SDLK_AC_HOME,
    VK_AC_BACK = SDLK_AC_BACK,
    VK_AC_FORWARD = SDLK_AC_FORWARD,
    VK_AC_STOP = SDLK_AC_STOP,
    VK_AC_REFRESH = SDLK_AC_REFRESH,
    VK_AC_BOOKMARKS = SDLK_AC_BOOKMARKS,
    VK_BRIGHTNESSDOWN = SDLK_BRIGHTNESSDOWN,
    VK_BRIGHTNESSUP = SDLK_BRIGHTNESSUP,
    VK_DISPLAYSWITCH = SDLK_DISPLAYSWITCH,
    VK_KBDILLUMTOGGLE = SDLK_KBDILLUMTOGGLE,
    VK_KBDILLUMDOWN = SDLK_KBDILLUMDOWN,
    VK_KBDILLUMUP = SDLK_KBDILLUMUP,
    VK_EJECT = SDLK_EJECT,
    VK_SLEEP = SDLK_SLEEP);
{$endregion}

{$region event types}
type
  { Keyboard modifier set }
  TShiftState = set of (ssShift, ssAlt, ssCtrl);
  { Mouse button enumeration }
  TMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mExtra2);
  { Mouse button set }
  TMouseButtons = set of TMouseButton;
  { A hoystick hat represents a switch with 9 possible positions }
  TJoystickHat = (hatCenter, hatUp, hatRight, hatDown, hatLeft,
    hatRightUp, hatRightDown, hatLeftUp, hatLeftDown);

  { Arguments for a window move or resize event }
  TMoveResizeArgs = record
    { The x y position or width and height of a window }
    X, Y: Integer;
  end;
  { Arguments for a window query close event }
  TQueryCloseArgs = record
    { Set to false to prevent a window from being closed }
    CanClose: Boolean;
  end;
  { Arguments for a keyboard event }
  TKeyboardArgs = record
    { Refer to virtual key codes in Bare.Game }
    Key: TVirtualKeyCode;
    { State of the modifier keys }
    Shift: TShiftState;
    { True if the keyboard event was generated as a result of a key
      repeating while being held down }
    Repeated: Boolean;
  end;
  { Arguments for a mouse move event }
  TMouseMoveArgs = record
    { The x y position of the mouse relative to a window }
    X, Y: Integer;
    { The x y position of the mouse relative to its last position }
    XRel, YRel: Integer;
    { State of the modifier keys }
    Shift: TShiftState;
  end;
  { Arguments for a mouse button event }
  TMouseButtonArgs = record
    { The mouse button being pressed or released }
    Button: TMouseButton;
    { The x y position of the mouse relative to a window }
    X, Y: Integer;
    { State of the modifier keys }
    Shift: TShiftState;
  end;
  { Arguments for a mouse wheel event }
  TMouseWheelArgs = record
    { Delta of +1 for scroll up and -1 for scroll down }
    Delta: Integer;
    { State of the modifier keys }
    Shift: TShiftState;
  end;
  { Arguments for a window focus event }
  TFocusArgs = record
    { Focused is true if gaining focus, false if losing focus }
    Focused: Boolean;
  end;
  { Arguments for a joystick axis event }
  TJoystickAxisArgs = record
    { The joystick sending the event }
    JoystickIndex: Integer;
    { The axix which changed }
    AxisIndex: Word;
    { The position of the axis in the range -32768 to 32767 }
    Position: SmallInt;
  end;
  { Arguments for a joystick trackball event }
  TJoystickBallArgs = record
    { The joystick sending the event }
    JoystickIndex: Integer;
    { The index of the ball }
    BallIndex: Integer;
    { The relative motion the trackball }
    XRel, YRel: Integer;
  end;
  { Arguments for a joystick button event }
  TJoystickButtonArgs = record
    { The joystick sending the event }
    JoystickIndex: Integer;
    { The index of the button }
    ButtonIndex: Integer;
    { When the button pressed true, when released false }
    Pressed: Boolean;
  end;
  { Arguments for a joystick trackball event }
  TJoystickHatArgs = record
    { The joystick sending the event }
    JoystickIndex: Integer;
    { The index of the hat }
    HatIndex: Integer;
    { The hat position }
    Hat: TJoystickHat;
  end;
  { Arguments for a joystick attach or detach event }
  TJoystickDeviceArgs = record
    { The joystick sending the event }
    JoystickIndex: Integer;
    { Added is true is a joystick was attached, false if it was detached }
    Added: Boolean;
  end;

{doc off}
const
  msgKeyDown = SDL_KEYDOWN;
  msgKeyUp = SDL_KEYUP;
  msgMouseMove = SDL_MOUSEMOTION;
  msgMouseButtonDown = SDL_MOUSEBUTTONDOWN;
  msgMouseButtonUp = SDL_MOUSEBUTTONUP;
  msgMouseWheel = SDL_MOUSEWHEEL;
  msgJoystickAxis = SDL_JOYAXISMOTION;
  msgJoystickBall = SDL_JOYBALLMOTION;
  msgJoystickHat = SDL_JOYHATMOTION;
  msgJoystickButtonDown = SDL_JOYBUTTONDOWN;
  msgJoystickButtonUp = SDL_JOYBUTTONUP;
{doc on}

{ TMessage is used by TApplication.ProcessEvents and TWindow.DispatchMessage
  See also
  <link Overview.Bare.Game.TMessage, TMessage members> }

type
  TMessage = record
    { Refer to the Bare.Game unit for message values }
    Msg: LongWord;
    { The time at which the event was sent }
    Time: Double;
    case LongWord of
      SDL_FIRST_EVENT: (EmptyArgs: TEmptyArgs);
      SDL_WINDOWEVENT_MOVED: (MoveResizeArgs: TMoveResizeArgs);
      SDL_WINDOWEVENT_FOCUS_GAINED: (FocusArgs: TFocusArgs);
      SDL_QUIT_EVENT: (QuitArgs: TEmptyArgs);
      SDL_KEYDOWN: (KeyboardArgs: TKeyboardArgs);
      SDL_MOUSEMOTION: (MouseMoveArgs: TMouseMoveArgs);
      SDL_MOUSEBUTTONDOWN: (MouseButtonArgs: TMouseButtonArgs);
      SDL_MOUSEWHEEL: (MouseWheelArgs: TMouseWheelArgs);
      SDL_JOYAXISMOTION: (JoystickAxisArgs: TJoystickAxisArgs);
      SDL_JOYBALLMOTION: (JoystickBallArgs: TJoystickBallArgs);
      SDL_JOYHATMOTION: (JoystickHatArgs: TJoystickHatArgs);
      SDL_JOYBUTTONDOWN: (JoystickButtonArgs: TJoystickButtonArgs);
      SDL_JOYDEVICEADDED: (JoystickDeviceArgs: TJoystickDeviceArgs);
  end;

{ TMessages is returned from <link Bare.Game.TMessages.Remove, TMessageQueue.Remove method>
  See also
  <link Bare.Game.TMessage, TMessage record> }

  TMessages = TArray<TMessage>;

{ When writing a multithreaded game use <link Bare.Game.TWindow.MessageQueue, MessageQueue>
  to safely access the user input queue in your <link Bare.Game.TWindow.Logic, Logic method>
  See also
  <link Overview.Bare.Game.TMessageQueue, TMessageQueue members>
  <link Bare.Game.TMessageQueue.Remove, Remove method> }

  TMessageQueue = class
  private
    FMessages: TMessages;
    FQueue: TMessages;
    FCount: Integer;
  public
    { Use <link Bare.Game.TWindow.MessageQueue, TWindow.MessageQueue property> instead of
      creating a TMessageQueue }
    constructor Create;
    { Add a message to the queue
      Remarks
      Called for you automatically in the application loop }
    procedure Add(var Message: TMessage);
    { Removes all incomming messages and places them in the queue using a thread safe manner
      Remarks
      It is recommended that you make use of <link Bare.Game.TWindow.MessageQueue, MessageQueue.Remove>
      at the start of your <link Bare.Game.TWindow.Logic, Logic> method }
    function Remove: TMessages;
    { Returns true if key is down in the queue }
    function KeyDown(Key: TVirtualKeyCode): Boolean;
    { Returns true if key is up in the queue }
    function KeyUp(Key: TVirtualKeyCode): Boolean;
    { Returns true if mouse is down in the queue }
    function MouseDown(Button: TMouseButton): Boolean; overload;
    { Returns true if mouse is down in the queue while capturing the position and shift state }
    function MouseDown(Button: TMouseButton; out X, Y: Integer; out Shift: TShiftState): Boolean; overload;
    { Returns true if mouse is up in the queue }
    function MouseUp(Button: TMouseButton): Boolean; overload;
    { Returns true if mouse is up in the queue while capturing the position and shift state }
    function MouseUp(Button: TMouseButton; out X, Y: Integer; out Shift: TShiftState): Boolean; overload;
    { The result of the last remove operation }
    property Queue: TMessages read FQueue;
  end;

  { TEmptyEvent notifies you that window focus was gained or lost }
  TFocusEvent = TEventHandler<TFocusArgs>;
  { TEmptyEvent notifies you that a window is requesting to be closed }
  TQueryCloseEvent = TEventHandler<TQueryCloseArgs>;
  { TKeyboardEvent notifies you that key was pressed or released }
  TKeyboardEvent = TEventHandler<TKeyboardArgs>;
  { TKeyboardEvent notifies you that the mouse moved }
  TMouseMoveEvent = TEventHandler<TMouseMoveArgs>;
  { TKeyboardEvent notifies you that the mouse moved }
  TMouseButtonEvent = TEventHandler<TMouseButtonArgs>;
  { TMouseWheelEvent notifies you that the mouse wheel was scrolled }
  TMouseWheelEvent = TEventHandler<TMouseWheelArgs>;
  { TMoveResizeEvent notifies you that a window was moved or resized }
  TMoveResizeEvent = TEventHandler<TMoveResizeArgs>;
  { TJoystickAxisEvent notifies you that a joystick axis position changed }
  TJoystickAxisEvent = TEventHandler<TJoystickAxisArgs>;
  { TJoystickAxisEvent notifies you that a joystick button was pressed or released }
  TJoystickButtonEvent = TEventHandler<TJoystickButtonArgs>;
  { TJoystickAxisEvent notifies you that a joystick hat changed }
  TJoystickHatEvent = TEventHandler<TJoystickHatArgs>;
  { TJoystickAxisEvent notifies you that a joystick trackball was moved }
  TJoystickBallEvent = TEventHandler<TJoystickBallArgs>;
  { TJoystickDeviceArgs notifies you that a joystick was attached or detached }
  TJoystickDeviceEvent = TEventHandler<TJoystickDeviceArgs>;
{$endregion}

{ TStopwatch tracks high resolution time intervals
  See also
  <link Overview.Bare.Game.TStopwatch, TStopwatch members> 
  <link topic_introduction, Introduction>}    

type
  TStopwatch = class
  private
    FTime: Double;
    FStart: Double;
    FStop: Double;
    FInterval: Double;
    FFrame: Integer;
    FFramerate: Integer;
    FSeconds: LargeWord;
    FPaused: Boolean;
    procedure SetPaused(Value: Boolean);
  public
    { Create a new TStopwatch instance optionally synchronizing its time
      Remarks
      To synchronize time, pass the time from another stopwatch to this constructor
      and both stopwatches will be synchronized until one is paused or reset }
    constructor Create(SyncTime: Double = 0);
    { Reset the the stopwatch to 0 }
    procedure Reset;
    { Move the stopwatch forward one frame }
    function Step: Double;
    { Used to suspend time updates to the stopwatch }
    property Paused: Boolean read FPaused write SetPaused;
    { The total elapsed time in seconds }
    property Time: Double read FTime;
    { The elapsed time in seconds between steps }
    property Interval: Double read FInterval;
    { The number of steps which occurred in the last second }
    property Framerate: Integer read FFramerate;
  end;

{ TTimer fires an OnExpired event every interval milliseconds while enabled
  See also
  <link Overview.Bare.Game.TTimer, TTimer members> }

  TTimer = class
  private
    FTimer: LongInt;
    FEnabled: Boolean;
    FInterval: LongWord;
    FOnExpired: TEmptyEvent;
    class var FInitialized: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: LongWord);
  protected
    { Invokes OnExpired event }
    procedure DoExpired(var Args: TEmptyArgs); virtual;
  public
    { Creates a new TTimer and initializes its interval to 1000 }
    constructor Create;
    destructor Destroy; override;
    { Stops the timer }
    property Enabled: Boolean read FEnabled write SetEnabled;
    { Time in milliseconds between notifications }
    property Interval: LongWord read FInterval write SetInterval;
    { Notification that an interval has elapsed }
    property OnExpired: TEmptyEvent read FOnExpired write FOnExpired;
  end;

{ THardwareCollection<T> represents a collection of connectable devices such as joysticks
  See also
  <link Overview.Bare.Game.THardwareCollection, THardwareCollection members> }

  THardwareCollection<T: TObject> = class(TOwnerCollection<T>)
  public
    {doc ignore}
    constructor Create;
    { Scan for connected devices }
    procedure ScanHardware; virtual;
  end;

{ TAudioSample represents an sound clip which can be played in a bank
  See also
  <link Overview.Bare.Game.TAudioSample, TAudioSample members>
  <link Bare.Game.TAudio, TAudio class>
  <link Bare.Game.TAudioSamples, TAudioSamples class> }

  TAudioSample = class
  private
    FData: PByte;
    FSize: LongWord;
    FName: string;
    function GetDuration: Double;
  public
    { Do not use. Use <link Overview.Bare.Game.TAudioSamples, Audio.Samples.Add> instead. }
    constructor Create(Size: LongWord);
    destructor Destroy; override;
    { Plays a sample in the oldest unlocked bank }
    function Play: Integer; overload;
    { Plays a sample in a specific bank }
    procedure Play(Bank: Integer); overload;
    { Plays a sample in the oldest unlocked bank at a specified volume }
    function PlayVolume(Volume: Byte): Integer; overload;
    { Plays a sample in a specific bank at a specific volume }
    procedure PlayVolume(Volume: Byte; Bank: Integer); overload;
    { The indexed name of the sample }
    property Name: string read FName write FName;
    { The length in seconds of the sample }
    property Duration: Double read GetDuration;
  end;

{ TAudioSamples loads and holds a collection of sound clips in memory
  Remarks
  Only raw or compressed ms-pcm wav data can be loaded. You can use the free
  tool Audacity to convert mp3, oog, or other formats to comrpessed wav files.
  See also
  <link Overview.Bare.Game.TAudioSamples, TAudioSamples members>
  <link Bare.Game.TAudioSample, TAudioSample class> }

  TAudioSamples   = class(TOwnerCollection<TAudioSample>)
  private
    function Add(Ops: PSDL_RWops): TAudioSample; overload;
    function GetSample(const Name: string): TAudioSample;
    function GetSampleByIndex(Index: Integer): TAudioSample;
  public
    { Loads a sound sample from the file system }
    function Add(const FileName: string): TAudioSample; overload;
    { Loads a sound sample from a stream }
    function Add(Stream: TStream): TAudioSample; overload;
    { Unloads a sample from memory }
    procedure Remove(Sample: TAudioSample);
    { The default indexer of type <link Bare.Game.TAudioSample, TAudioSample> }
    property Sample[Name: string]: TAudioSample read GetSample; default;
    { An indexer of type <link Bare.Game.TAudioSample, TAudioSample> }
    property SampleByIndex[Index: Integer]: TAudioSample read GetSampleByIndex;
  end;

{ TAudioBankState designates the state of a <link Bare.Game.TAudioBank, TAudioBank> }

  TAudioBankState = (abStopped, abPlaying, abPaused);

{ TAudioBank is a slot from whence a sample is played
  See also
  <link Overview.Bare.Game.TAudioBank, TAudioBank members> }

  TAudioBank = class
  private
    FLocked: Boolean;
    FMuted: Boolean;
    FLooping: Boolean;
    FPosition: LongWord;
    FVolume: Byte;
    FSample: TAudioSample;
    FState: TAudioBankState;
    FTime: Double;
    function GetDuration: Double;
    function GetDimension: Double;
    procedure SetDimension(const Value: Double);
    procedure SetState(Value: TAudioBankState);
    procedure SetVolume(Value: Byte);
  public
    constructor Create;
    { Play the sample in the bank }
    procedure Play;
    { Stop the sample in the bank }
    procedure Stop;
    { Pause the sample in the bank }
    procedure Pause;
    { Prevents a sample from reusing the bank }
    property Locked: Boolean read FLocked write FLocked;
    { Mutes any samples playing in the bank }
    property Muted: Boolean read FMuted write FMuted;
    { Repeats a sample if it played and reaches the end }
    property Looping: Boolean read FLooping write FLooping;
    { The length in seconds of the current sample }
    property Duration: Double read GetDuration;
    { The cursor in seconds of the current sample }
    property Position: Double read GetDimension write SetDimension;
    { The current <link Bare.Game.TAudioSample> }
    property Sample: TAudioSample read FSample;
    { The state of the bank. <link Bare.Game.TAudioSample, TAudioSample> (read, write) }
    property State: TAudioBankState read FState write SetState;
    { The volume 0 to 255 used to mix the bank }
    property Volume: Byte read FVolume write SetVolume;
  end;

{ The number of available sound banks }

const
  AudioBankCount = 24;

{ TAudioBanks holds a collection of audio banks
  See also
  <link Overview.Bare.Game.TAudioBanks, TAudioBanks members> }

type
  TAudioBanks = class(TOwnerCollection<TAudioBank>)
  private
    function GetBank(Index: Integer): TAudioBank;
  public
    {doc ignore}
    constructor Create;
    { The default indexer of type <link Bare.Game.TAudioBank, TAudioBank> }
    property Bank[Index: Integer]: TAudioBank read GetBank; default;
  end;

{ TAudio is the entry point for using sound in your games. You should never
  create an instance of TAudio, instead use the global <link Bare.Game.Audio, Audio function>.
  See also
  <link Overview.Bare.Game.TAudio, TAudio members> }

  TAudio = class
  private
    FSamples: TAudioSamples;
    FBanks: TAudioBanks;
    FMasterVolume: Byte;
    class var AudioSpec: TSDL_AudioSpec;
  public
    { Do not use.  Use <link Bare.Game.Audio, Audio function> instead. }
    constructor Create;
    destructor Destroy; override;
    { A <link Bare.Game.TAudioSamples, TAudioSample> collection }
    property Samples: TAudioSamples read FSamples;
    { A <link Bare.Game.TAudioBanks, TAudioBank> collection }
    property Banks: TAudioBanks read FBanks;
    { The master volume 0 to 255 used to mix all banks }
    property MasterVolume: Byte read FMasterVolume write FMasterVolume;
  end;

{ TDisplayMode represents possible resolutions and color depths for a Fullscreen window }

  TDisplayMode = record
    { The horizontal screen resolution }
    Width: LongInt;
    { The vertical screen resolution }
    Height: LongInt;
    { The color depth }
    ColorDepth: LongInt;
    { The vertial retrace rate }
    RefreshRate: LongInt;
  end;

{ TDisplayModes is an array of TDisplayMode }

  TDisplayModes = TArray<TDisplayMode>;

{ TScreen represents a monitor (or LCD display) attached to a computer
  See also
  <link Overview.Bare.Game.TScreen, TScreen members> }

  TScreen = class
  private
    FIndex: LongInt;
    function GetName: string;
  public
    {doc ignore}
    constructor Create(Index: Integer);
    { Retrieves an array of supported display modes for the screen }
    procedure GetDisplayModes(out Modes: TDisplayModes);
    { Retrieves the screen's bounding rect }
    procedure GetBounds(out Rect: TRectI);
    { The name of the display }
    property Name: string read GetName;
  end;

{ TScreens provides access to one or more screens. You should never create an
  instance of TScreens, instead use the global <link Bare.Game.Screens, Screens function>.
  See also
  <link Overview.Bare.Game.TScreens, TScreens members>
  <link Bare.Game.TScreen, TScreen class> }

  TScreens = class(THardwareCollection<TScreen>)
  private
    function GetDriverName: string;
    function GetPrimary: TScreen;
    function GetScreen(Index: Integer): TScreen;
  public
    { Recreates the collection of screens
      Remarks
      After this method is invoked, all previous screen instances are invalid }
    procedure ScanHardware; override;
    { The name of the current video driver }
    property DriverName: string read GetDriverName;
    { The primary screen }
    property Primary: TScreen read GetPrimary;
    { The default indexer of type <link Bare.Game.TScreen, TScreen> }
    property Screen[Index: Integer]: TScreen read GetScreen; default;
  end;

{$region peripherals}
{ TInputDevice is an abstract class
  See also
  <link Overview.Bare.Game.TInputDevice, TInputDevice members> }

  TInputDevice = class
  public
    { Abstract method to scan an input device state }
    procedure Scan; virtual; abstract;
  end;

{ TKeyboard allows you to detect which keys are pressed. You should never create an
  instance of TKeyboard, instead use the global <link Bare.Game.Keyboard, Keyboard function>.
  See also
  <link Overview.Bare.Game.TKeyboard, TKeyboard members> }

  TKeyboard = class(TInputDevice)
  private
    FKeys: PUint8;
    FCount: LongWord;
    function GetKey(Index: TVirtualKeyCode): Boolean;
    function GetShiftState: TShiftState;
  public
    { Scan the keyboard state }
    procedure Scan; override;
    { The default indexer returns true is a key is pressed down }
    property Key[Index: TVirtualKeyCode]: Boolean read GetKey;
    { Returns the current shift state }
    property ShiftState: TShiftState read GetShiftState;
  end;

{ TMouse allows you to detect the state of the mouse. You should never create an
  instance of TMouse, instead use the global <link Bare.Game.Mouse, Mouse function>.
  See also
  <link Overview.Bare.Game.TMouse, TMouse members> }

  TMouse = class(TInputDevice)
  private
    FButtons: TMouseButtons;
    FX: Integer;
    FY: Integer;
    function GetCoord: TPointI;
    function GetCaptured: Boolean;
    procedure SetCaptured(Value: Boolean);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
  public
    { Scan the mouse state }
    procedure Scan; override;
    { Move the mouse to a postion relative to the window }
    procedure Move(X, Y: Integer);
    { The mouse button state }
    property Buttons: TMouseButtons read FButtons;
    { The position of the mouse relative to the window with mouse focus }
    property Coord: TPointI read GetCoord;
    { The x position of the mouse relative to the window with mouse focus }
    property X: Integer read FX;
    { The y position of the mouse relative to the window with mouse focus }
    property Y: Integer read FY;
    { When captured the mouse cursor is hidden and events report relative coordinates }
    property Captured: Boolean read GetCaptured write SetCaptured;
    { Shows or hides the mouse cursor }
    property Visible: Boolean read GetVisible write SetVisible;
  end;

{ TJoystickPart<T> represents a series of axes, buttons, or trackballs which are
  part of one joystick
  See also
  <link Overview.Bare.Game.TJoystickPart, TJoystickPart members>
  <link Bare.Game.TJoystick, TJoystick class> }

  TJoystickPart<T> = class(TInputDevice)
  private
    FJoystick: PSDL_Joystick;
    FValues: TArrayList<T>;
    function GetCount: Integer;
    function GetValue(Index: Integer): T;
  protected
    procedure DoScan; virtual; abstract;
  public
    { The enumerator type for  TJoystickPart }
    type TEnumerator = TArrayEnumerator<T>;
    { Returns a part enumerator }
    function GetEnumerator: TEnumerator;
  public
    {doc ignore}
    constructor Create(Joystick: PSDL_Joystick);
    { Scan joystick parts and their states }
    procedure Scan; override;
    { Number of axes, buttons, or trackballs on a joystick }
    property Count: Integer read GetCount;
    { The state of an axis, button, or trackball on a joystick }
    property Value[Index: Integer]: T read GetValue; default;
  end;

{ TJoystickAxes is a collection of analog sticks which are parts of one joystick.
  Values range from Low(SmallInt) to High(SmallInt).
  Remarks
  Typically value[0, 1] are x, y on the first stick, value[2, 3] is [x, y] second
  stick, and so on. Side analog triggers can also represented by value[n]. The
  configuration varies by joystick.
  See also
  <link Overview.Bare.Game.TJoystickAxes, TJoystickAxes members>
  <link Bare.Game.TJoystick, TJoystick class> }

  TJoystickAxes = class(TJoystickPart<SmallInt>)
  protected
    { Scan axes }
    procedure DoScan; override;
  end;

{ TJoystickButtons is a collection of buttons which are parts of one joystick
  See also
  <link Overview.Bare.Game.TJoystickButtons, TJoystickButtons members>
  <link Bare.Game.TJoystick, TJoystick class> }

  TJoystickButtons = class(TJoystickPart<Boolean>)
  protected
    { Scan button }
    procedure DoScan; override;
  end;

{ TJoystickHats is a collection of 9 position switches which are parts of one joystick
  See also
  <link Overview.Bare.Game.TJoystickHats, TJoystickHats members>
  <link Bare.Game.TJoystick, TJoystick class> }

  TJoystickHats = class(TJoystickPart<TJoystickHat>)
  protected
    { Scan hats }
    procedure DoScan; override;
  end;

{ TJoystickBall represents a trackball which is part of a joystick
  See also
  <link Overview.Bare.Game.TJoystickBall, TJoystickBall members>
  <link Bare.Game.TJoystick, TJoystick class> }

  TJoystickTrackball = record
    { The x change for a trackball }
    XDelta: Integer;
    { The y change for a trackball }
    YDelta: Integer;
  end;

{ TJoystickBalls is a collection of trackballs which are parts of one joystick
  See also
  <link Overview.Bare.Game.TJoystickBalls, TJoystickBalls members>
  <link Bare.Game.TJoystick, TJoystick class> }

  TJoystickTrackballs = class(TJoystickPart<TJoystickTrackball>)
  protected
    { Scan trackballs }
    procedure DoScan; override;
  end;

{ TJoystick is an interface to a peripheral with zero or more analog sticks (axes),
  buttons, hats, or trackballs
  See also
  <link Overview.Bare.Game.TJoystick, TJoystick members> }

  TJoystick = class(TInputDevice)
  private
    FJoystick: PSDL_Joystick;
    FIndex: Integer;
    FName: string;
    FAxes: TJoystickAxes;
    FButtons: TJoystickButtons;
    FHats: TJoystickHats;
    FTrackballs: TJoystickTrackballs;
  public
    {doc ignore}
    constructor Create(Index: Integer);
    destructor Destroy; override;
    { Scan for joystick parts and their states }
    procedure Scan; override;
    { The name of the joystick }
    property Name: string read FName;
    { A collection of analog sticks }
    property Axes: TJoystickAxes read FAxes;
    { A collection of buttons }
    property Buttons: TJoystickButtons read FButtons;
    { A collection of 9-way hat switches }
    property Hats: TJoystickHats read FHats;
    { A collection of trackballs }
    property Trackballs: TJoystickTrackballs read FTrackballs;
  end;

{ TJoysticks provides access to one or more joysticks. You should never create an
  instance of TJoysticks, instead use the global <link Bare.Game.Joysticks, Joysticks function>.
  See also
  <link Overview.Bare.Game.TJoysticks, TJoysticks members>
  <link Bare.Game.TJoystick, TJoystick class> }

  TJoysticks = class(THardwareCollection<TJoystick>)
  private
    function GetJoystick(Index: Integer): TJoystick;
  public
    { Recreates the collection of joysticks
      Remarks
      After this method is invoked, all previous screen joysticks are invalid }
    procedure ScanHardware; override;
    { The default indexer of type <link Bare.Game.TJoystick, TJoystick> }
    property Joystick[Index: Integer]: TJoystick read GetJoystick; default;
  end;
{$endregion}

{$region window parameters}
type
{ TWindowStyles defines the decoration of a window }
  TWindowStyles = set of (wsBorder, wsFulldesktop, wsFullscreen,
    wsMaximized, wsResizeable, wsVisible);

{ TAttributes defines OpenGL window attributes }

  TAttributes = record
    { default 0 = no stencil buffer, possible values are: 0, 8, 16, 24, 32 }
    StencilBits: Byte;
    { default 16 = 16 bit depth buffer, possible values are: 0, 8, 16, 24, 32 }
    DepthBits: Byte;
    { default 0 = no full screen anti-ailiasing, possible values are: 0, 2, 4, 8 }
    MultiSamples: Byte;
    { default 1 = synchronize the vertical retrace, possible values are: 0, 1 }
    VerticalSync: Byte;
    { default 1 = force hardware acceleration, possible values are: 0, 1, 2 }
    HardwareAcceleration: Byte;
    { default 0 = disable 3D stereo rendering, possible values are: 0, 1 }
    Stereo: Byte;
    { default 0, optionally specify an OpenGL major version number when creating a 3.x context }
    MajorVersion: Byte;
    { default 0, optionally specify an OpenGL minor version number when creating a 3.x context }
    MinorVersion: Byte;
  end;

const
  { A created window will be positioned by the operatating system
  <link Bare.Game.TWindowParams, TWindowParams record> }
  WindowPosUndefined = SDL_WINDOWPOS_UNDEFINED;
  { A created window will be positioned in the center of the screen
  <link Bare.Game.TWindowParams, TWindowParams record> }
  WindowPosCentered = SDL_WINDOWPOS_CENTERED;

{ TCreateParams defines the capabilities of window
  See also
  <link Bare.Game.TWindow.WindowPosUndefined, WindowPosUndefined constant>
  <link Bare.Game.TWindow.WindowPosCentered, WindowPosCentered constant>
  <link Bare.Game.TWindow.TAttributes, TAttributes record>
  <link Bare.Game.TWindow.CreateParams, CreateParams method> }

type
  TCreateParams = record
    { The caption above a window }
    Caption: string;
    { Horizontal position of a window }
    X: Integer;
    { Vertical position of a window }
    Y: Integer;
    { Width of a window }
    Width: Integer;
    { Height of a window }
    Height: Integer;
    { Display mode to use if fullscreen is set }
    DisplayMode: TDisplayMode;
    { Window decoration options }
    Style: TWindowStyles;
    { Values used to configure OpenGL }
    Attributes: TAttributes;
    { When set to true separate threads for Logic and Render are created }
    Multithreaded: Boolean;
  end;
{$endregion}

{ TWindow is the class which displays content on your screen
  Remarks
  The following methods may execute in arbitrary threads: Load, Update, Render
  See also
  <link Overview.Bare.Game.TWindow, TWindow members>
  <link topic_window, Creating a game window topic> }

  TWindow = class(TPersistsObject, IWindow)
  private
    FWindow: PSDL_Window;
    FContext: PSDL_GLContext;
    FClosed: Boolean;
    FFullscreen: Boolean;
    FVisible: Boolean;
    FMultithreaded: Boolean;
    FLogicThread: TThread;
    FRenderThread: TThread;
    FCreateParams: TCreateParams;
    FMessageQueue: TMessageQueue;
    FOnQueryClose: TQueryCloseEvent;
    FOnClose: TEmptyEvent;
    FOnFocus: TFocusEvent;
    FOnKeyDown: TKeyboardEvent;
    FOnKeyUp: TKeyboardEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseButtonEvent;
    FOnMouseUp: TMouseButtonEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseEnter: TEmptyEvent;
    FOnMouseLeave: TEmptyEvent;
    FOnMove: TMoveResizeEvent;
    FOnResize: TMoveResizeEvent;
    FOnSizeChanged: TEmptyEvent;
    FOnShow: TEmptyEvent;
    FOnHide: TEmptyEvent;
    FOnJoystickAxis: TJoystickAxisEvent;
    FOnJoystickBall: TJoystickBallEvent;
    FOnJoystickHat: TJoystickHatEvent;
    FOnJoystickButtonDown: TJoystickButtonEvent;
    FOnJoystickButtonUp: TJoystickButtonEvent;
    FOnJoystickAdded: TJoystickDeviceEvent;
    FOnJoystickRemoved: TJoystickDeviceEvent;
    procedure CreateWindow(var Params: TCreateParams);
    procedure CreateLoop(Stopwatch: TStopwatch);
    procedure DestroyLoop;
    procedure RunLoop(Stopwatch: TStopwatch);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetFullscreen(Value: Boolean);
    function GetScreen: TScreen;
    procedure SetVisible(Value: Boolean);
  protected
    { IWindow.GetDimension }
    function GetDimension(Index: Integer): Integer;
    { IWindow.SetDimension }
    procedure SetDimension(Index, Value: Integer);
    { CreateParams initializes window settings before window creation }
    procedure CreateParams(var Params: TCreateParams); virtual;
    { SetDisplayMode can change the resolution and color depth of a fullscreen window }
    procedure SetDisplayMode(const Mode: TDisplayMode);
    { GetDisplayMode returns the resolution and color depth of a fullscreen window }
    procedure GetDisplayMode(out Mode: TDisplayMode);
    { This method is called once before Logic and can be used to initialize game state
      <link Bare.Game.TWindow.FinalizeInitialize, FinalizeInitialize method>
      <link Bare.Game.TWindow.Logic, Logic method> }
    procedure LogicInitialize; virtual;
    { Override this method to calculate game state and logic
      Remarks
      This method executed is inside a dedicated thread if multithreaded is true
      See also
      <link Bare.Game.TWindow.Render, Render method>
      <link Bare.Game.TCreateParams, TCreateParams type> }
    procedure Logic(Stopwatch: TStopwatch); virtual;
    { This method is upon close and can be used to finalize game state
      <link Bare.Game.TWindow.LogicInitialize, LogicInitialize method>
      <link Bare.Game.TWindow.Logic, Logic method> }
    procedure LogicFinalize; virtual;
    { This method is called once before Logic and can be used to initialize OpenGL state
      See also
      <link Bare.Game.TWindow.FinalizeInitialize, FinalizeInitialize method>
      <link Bare.Game.TWindow.Render, Render method> }
    procedure RenderInitialize; virtual;
    { Override this method to render visuals with OpenGL functions
      Remarks
      This method executed is inside a dedicated thread if multithreaded is true
      See also
      <link Bare.Game.TWindow.Logic, Logic method>
      <link Bare.Game.TWindow.MessageQueue, MessageQueue property>
      <link Bare.Game.TCreateParams, TCreateParams type> }
    procedure Render(Stopwatch: TStopwatch); virtual;
    { This method is upon close and can be used to finalize OpenGL state
      See also
      <link Bare.Game.TWindow.RenderInitialize, RenderInitialize method>
      <link Bare.Game.TWindow.Render, Render method> }
    procedure RenderFinalize; virtual;
    { Flushes drawing commands and switch the front and back buffers. Called for you
      automatically after Render.
      See also
      <link Bare.Game.TWindow.Render, Render method> }
    procedure SwapBuffers;
    { Called by <link Bare.Game.TApplication.ProcessEvents, TApplication.ProcessEvents> }
    procedure DispatchMessage(var Message: TMessage); virtual;
    procedure DoClose(var Args: TEmptyArgs); virtual;
    procedure DoFocus(var Args: TFocusArgs); virtual;
    procedure DoKeyDown(var Args: TKeyboardArgs); virtual;
    procedure DoKeyUp(var Args: TKeyboardArgs); virtual;
    procedure DoMouseMove(var Args: TMouseMoveArgs); virtual;
    procedure DoMouseDown(var Args: TMouseButtonArgs); virtual;
    procedure DoMouseUp(var Args: TMouseButtonArgs); virtual;
    procedure DoMouseWheel(var Args: TMouseWheelArgs); virtual;
    procedure DoMouseEnter(var Args: TEmptyArgs); virtual;
    procedure DoMouseLeave(var Args: TEmptyArgs); virtual;
    procedure DoMove(var Args: TMoveResizeArgs); virtual;
    procedure DoResize(var Args: TMoveResizeArgs); virtual;
    procedure DoSizeChanged(var Args: TEmptyArgs); virtual;
    procedure DoShow(var Args: TEmptyArgs); virtual;
    procedure DoHide(var Args: TEmptyArgs); virtual;
    procedure DoJoystickAxis(var Args: TJoystickAxisArgs); virtual;
    procedure DoJoystickBall(var Args: TJoystickBallArgs); virtual;
    procedure DoJoystickHat(var Args: TJoystickHatArgs); virtual;
    procedure DoJoystickButtonDown(var Args: TJoystickButtonArgs); virtual;
    procedure DoJoystickButtonUp(var Args: TJoystickButtonArgs); virtual;
    procedure DoJoystickAdded(var Args: TJoystickDeviceArgs); virtual;
    procedure DoJoystickRemoved(var Args: TJoystickDeviceArgs); virtual;
    { This value will be true is multithreaded is set to true
      in <link Bare.Game.TWindow.CreateParams, CreateParams method>
      See also
      <link Bare.Game.TWindow.Logic, Logic method>
      <link Bare.Game.TWindow.Render, Render method> }
    property Multithreaded: Boolean read FMultithreaded;
    { Use MessageQueue to safely read user input in your
      <link Bare.Game.TWindow.Logic, Logic method>
      See Also
      <link Bare.Game.TMessageQueue.Remove, TMessageQueue.Remove method> }
    property MessageQueue: TMessageQueue read FMessageQueue;
    { Notification before a window is closed }
    property OnQueryClose: TQueryCloseEvent read FOnQueryClose write FOnQueryClose;
    { Notification that a window was closed }
    property OnClose: TEmptyEvent read FOnClose write FOnClose;
    { Notification that a window recieved os lost keyboard focus }
    property OnFocus: TFocusEvent read FOnFocus write FOnFocus;
    { Notification that a key is pressed }
    property OnKeyDown: TKeyboardEvent read FOnKeyDown write FOnKeyDown;
    { Notification that a key is released }
    property OnKeyUp: TKeyboardEvent read FOnKeyUp write FOnKeyUp;
    { Notification that a mouse moved inside a window }
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    { Notification that a mouse button waspressed inside a window }
    property OnMouseDown: TMouseButtonEvent read FOnMouseDown write FOnMouseDown;
    { Notification that a mouse button was released inside a window }
    property OnMouseUp: TMouseButtonEvent read FOnMouseUp write FOnMouseUp;
    { Notification that a mouse wheel scrolled while inside a window }
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    { Notification that a mouse entered a window }
    property OnMouseEnter: TEmptyEvent read FOnMouseEnter write FOnMouseEnter;
    { Notification that a mouse left a window }
    property OnMouseLeave: TEmptyEvent read FOnMouseLeave write FOnMouseLeave;
    { Notification that a window is moved }
    property OnMove: TMoveResizeEvent read FOnMove write FOnMove;
    { Notification that a window was resized }
    property OnResize: TMoveResizeEvent read FOnResize write FOnResize;
    property OnSizeChanged: TEmptyEvent read FOnSizeChanged write FOnSizeChanged;
    { Notification that a window was shown }
     property OnShow: TEmptyEvent read FOnShow write FOnShow;
    { Notification that a window was hidden }
     property OnHide: TEmptyEvent read FOnHide write FOnHide;
    { Notification that a joystick axis was moved }
    property OnJoystickAxis: TJoystickAxisEvent read FOnJoystickAxis write FOnJoystickAxis;
    { Notification that a joystick trackball changed }
    property OnJoystickBall: TJoystickBallEvent read FOnJoystickBall write FOnJoystickBall;
    { Notification that a joystick hat switch changed }
    property OnJoystickHat: TJoystickHatEvent read FOnJoystickHat write FOnJoystickHat;
    { Invoked when a joystick button is pressed }
    property OnJoystickButtonDown: TJoystickButtonEvent read FOnJoystickButtonDown write FOnJoystickButtonDown;
    { Invoked when a joystick button is released }
    property OnJoystickButtonUp: TJoystickButtonEvent read FOnJoystickButtonUp write FOnJoystickButtonUp;
    { Invoked when a joystick is attached }
    property OnJoystickAdded: TJoystickDeviceEvent read FOnJoystickAdded write FOnJoystickAdded;
    { Invoked when a joystick is removed }
    property OnJoystickRemoved: TJoystickDeviceEvent read FOnJoystickRemoved write FOnJoystickRemoved;
  public
    { Do not use. Use <link Overview.Bare.Game.TApplication, CreateWindow or Run> instead. }
    constructor Create; virtual;
    destructor Destroy; override;
    { Requests that a window be closed }
    procedure Close;
    { Maximizes a window }
    procedure Maximize;
    { Minimizes a window }
    procedure Minimize;
    { Restores a window }
    procedure Restore;
    { Shows a window }
    procedure Show;
    { Hides a window }
    procedure Hide;
    { Moves a window to a specified position }
    procedure Move(X, Y: Integer);
    { Resizes a window to a specified width and height }
    procedure Resize(Width, Height: Integer);
    { Retrieves the boundaries of a window }
    procedure GetBounds(out Rect: TRectI);
    { Updates the boundaries of a window }
    procedure SetBounds(const Rect: TRectI);
    { The text show in the title bar of a window }
    property Caption: string read GetCaption write SetCaption;
    { Determines if a window fills an entire screen }
    property Fullscreen: Boolean read FFullscreen write SetFullscreen;
    { Window horizontal position }
    property X: Integer index 0 read GetDimension write SetDimension;
    { Window vertical position }
    property Y: Integer index 1 read GetDimension write SetDimension;
    { Window width }
    property Width: Integer index 2 read GetDimension write SetDimension;
    { Window height }
    property Height: Integer index 3 read GetDimension write SetDimension;
    { The screen containing the mid point of the window }
    property Screen: TScreen read GetScreen;
    { Determines whether a window is shown }
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TWindowClass defines a type of window }

  TWindowClass = class of TWindow;

{ TWindows holds a collection of windows
  See also
  <link Overview.Bare.Game.TWindows, TWindows members> }

  TWindows = class(TCollection<TWindow>)
  private
    procedure Delete(Index: Integer);
    function GetWindow(Index: Integer): TWindow;
  public
    { The default indexer for TWindows }
    property Window[Index: Integer]: TWindow read GetWindow; default;
  end;

{ Arguments for an application exception event }

  TExceptionArgs = record
    ExceptObject: Exception;
    Handled: Boolean;
  end;

{ TExceptionEvent allows an application to process unhandled exceptions }

  TExceptionEvent = TEventHandler<TExceptionArgs>;

{ TApplication manages windows and runs your game. You should never create an
  instance of TApplication, instead use the global <link Bare.Game.Application, Application function>.
  See also
  <link Overview.Bare.Game.TApplication, TApplication members> }

  TApplication = class
  private
    FTerminated: Boolean;
    FWindows: TWindows;
    FTimeDelta: Double;
    FOnException: TExceptionEvent;
    function GetKeyboardWindow: TWindow;
    function GetMouseWindow: TWindow;
    function GetMainWindow: TWindow;
  public
    { Do not use.  Use <link Bare.Game.Application, Application function> instead. }
    constructor Create;
    destructor Destroy; override;
    { Init returns true if a version of OpenGL can be initialized
      Remarks
      When strict is off (the default) init will return true if a hardware accelerated
      OpenGL context can be created even if the requested OpenGL version does not exist. }
    function Init(Strict: Boolean = False): Boolean;
    { Creates a window given a <link Bare.Game.TWindowClass, TWindowClass> }
    function CreateWindow(WindowClass: TWindowClass): TWindow;
    { Processes events pending in the event queue }
    procedure ProcessEvents;
    { Runs the application if a window was created }
    procedure Run; overload;
    { Create a window and run the application }
    procedure Run(WindowClass: TWindowClass); overload;
    { Create a window, store it in a variable, and run the application }
    procedure Run(WindowClass: TWindowClass; out Window: TWindow); overload;
    { Signal a running application to close all windows and stop }
    procedure Terminate;
    { The window with keyboard focus }
    property KeyboardWindow: TWindow read GetKeyboardWindow;
    { The window with mouse focus }
    property MouseWindow: TWindow read GetMouseWindow;
    { The first window created }
    property MainWindow: TWindow read GetMainWindow;
    { Set to true when terminate is called }
    property Terminated: Boolean read FTerminated;
    { A <link Bare.Game.TWindows, TWindows> collection }
    property Windows: TWindows read FWindows;
    { Invoked when an unhandled exception occurs }
    property OnException: TExceptionEvent read FOnException write FOnException;
  end;

{ Provides access to <link Bare.Game.TApplication, TApplication class> }
function Application: TApplication;
{ Provides access to <link Bare.Game.TAudio, TAudio class> }
function Audio: TAudio;
{ Provides access to <link Bare.Game.TScreens, TScreens class> }
function Screens: TScreens;
{ Provides access to <link Bare.Game.TKeyboard, Keyboard TKeyboard> }
function Keyboard: TKeyboard;
{ Provides access to <link Bare.Game.TMouse, TMouse class> }
function Mouse: TMouse;
{ Provides access to <link Bare.Game.TJoysticks, TJoysticks class> }
function Joysticks: TJoysticks;

{ Scans mouse, keyboard, and joysticks states
  Remarks
  This method is called for you automatically }
procedure ScanInput;

implementation

const
  WindowInstance = 'WindowInstance';

var
  ApplicationInstance: TObject;
  AudioInstance: TObject;
  KeyboardInstance: TObject;
  MouseInstance: TObject;
  ScreensInstance: TObject;
  JoysticksInstance: TObject;

function Application: TApplication;
begin
  if ApplicationInstance = nil then
  begin
    Lock;
    try
      if ApplicationInstance = nil then
        ApplicationInstance := TApplication.Create;
      { Ensure the video subsystem is initialized }
      Screens;
    finally
      Unlock;
    end;
  end;
  Result := TApplication(ApplicationInstance);
end;

function Audio: TAudio;
begin
  if AudioInstance = nil then
  begin
    Lock;
    try
      if AudioInstance = nil then
      begin
        SDL_InitSubSystem(SDL_INIT_AUDIO);
        AudioInstance := TAudio.Create;
      end;
    finally
      Unlock;
    end;
  end;
  Result := TAudio(AudioInstance);
end;

function Keyboard: TKeyboard;
begin
  if KeyboardInstance = nil then
  begin
    Lock;
    try
      if KeyboardInstance = nil then
        KeyboardInstance := TKeyboard.Create;
    finally
      Unlock;
    end;
  end;
  Result := TKeyboard(KeyboardInstance);
end;

function Mouse: TMouse;
begin
  if MouseInstance = nil then
  begin
    Lock;
    try
      if MouseInstance = nil then
        MouseInstance := TMouse.Create;
    finally
      Unlock;
    end;
  end;
  Result := TMouse(MouseInstance);
end;

function Screens: TScreens;
begin
  if ScreensInstance = nil then
  begin
    Lock;
    try
      if ScreensInstance = nil then
      begin
        SDL_InitSubSystem(SDL_INIT_VIDEO);
        ScreensInstance := TScreens.Create;
      end;
    finally
      Unlock;
    end;
  end;
  Result := TScreens(ScreensInstance);
end;

function Joysticks: TJoysticks;
begin
  if JoysticksInstance = nil then
  begin
    Lock;
    try
      if JoysticksInstance = nil then
      begin
        SDL_InitSubSystem(SDL_INIT_JOYSTICK);
        JoysticksInstance := TJoysticks.Create;
      end;
    finally
      Unlock;
    end;
  end;
  Result := TJoysticks(JoysticksInstance);
end;

procedure ScanInput;
var
  I: Integer;
begin
  Lock;
  try
    if MouseInstance <> nil then
      TInputDevice(MouseInstance).Scan;
    if KeyboardInstance <> nil then
      TInputDevice(KeyboardInstance).Scan;
    if JoysticksInstance <> nil then
      for I := TJoysticks(JoysticksInstance).Count - 1 downto 0 do
        TJoysticks(JoysticksInstance)[I].Scan;
  finally
    Unlock;
  end;
end;

const
  MessageCaptions: array[TMessageType] of PAnsiChar =
    ('Information', 'Confirmation', 'Warning', 'Error');
  MessageFlags: array[TMessageType] of Uint32 =
    (SDL_MESSAGEBOX_INFORMATION, SDL_MESSAGEBOX_INFORMATION,
    SDL_MESSAGEBOX_WARNING, SDL_MESSAGEBOX_ERROR);

procedure MessageBox(const Prompt: string);
begin
  MessageBox(mtInformation, Prompt);
end;

procedure MessageBox(Kind: TMessageType; const Prompt: string);
var
  P: AnsiString;
begin
  P := Prompt;
  SDL_ShowSimpleMessageBox(MessageFlags[Kind], MessageCaptions[Kind],
    PAnsiChar(P), nil);
end;

function MessageBox(Kind: TMessageType; const Prompt: string; Buttons: TMessageButtons): TModalResult;
begin
  Result := MessageBox(Kind, MessageCaptions[Kind], Prompt, Buttons);
end;

function MessageBox(Kind: TMessageType; const Title, Prompt: string; Buttons: TMessageButtons): TModalResult;
const
  RETURN = SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;
  ESCAPE = SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;
  ButtonFlags: array[TMessageButton] of Uint32 =
    (RETURN, ESCAPE, RETURN, ESCAPE, 0, 0, 0, 0);
  ButtonCaptions: array[TMessageButton] of PAnsiChar =
    ('Ok', 'Cancel', 'Yes', 'No', 'All', 'Abort', 'Retry', 'Ignore');
var
  Data: TSDL_MessageBoxData;
  ButtonList: TArrayList<TSDL_MessageBoxButtonData>;
  ButtonData: TSDL_MessageBoxButtonData;
  ButtonId: LongInt;
  T, P: AnsiString;
  B: TMessageButton;
begin
  if Buttons = [] then
    Exit(mrNone);
  T := Title;
  P := Prompt;
  Data.flags := MessageFlags[Kind];
  Data.window := nil;
  Data.title := PAnsiChar(T);
  Data.message := PAnsiChar(P);
  for B := Low(TMessageButton) to High(TMessageButton)do
    if B in Buttons then
    begin
      ButtonData.flags := ButtonFlags[B];
      ButtonData.buttonid := Ord(B) + 1;
      ButtonData.text := ButtonCaptions[B];
      ButtonList.Add(ButtonData);
    end;
  Data.numbuttons := ButtonList.Count;
  Data.buttons := PSDL_MessageBoxButtonData(ButtonList.Items);
  Data.colorScheme := nil;
  if SDL_ShowMessageBox(Data, ButtonId) = 0 then
    Result := TModalResult(ButtonId)
  else
    Result := mrNone;
end;

const
  DefaultQueueSize = 100;

constructor TMessageQueue.Create;
begin
  inherited Create;
  SetLength(FMessages, DefaultQueueSize);
end;

procedure TMessageQueue.Add(var Message: TMessage);
begin
  Lock;
  try
    { Queue ignores adds when the count is over the maximum limit }
    if FCount = DefaultQueueSize then
      Exit;
    FMessages[FCount] := Message;
    Inc(FCount);
  finally
    Unlock;
  end;
end;

function TMessageQueue.Remove: TMessages;
var
  I: Integer;
begin
  Lock;
  try
    SetLength(Result, FCount);
    for I := 0 to FCount - 1 do
      Result[I] := FMessages[I];
    FQueue := Result;
    FCount := 0;
  finally
    Unlock;
  end;
end;

function TMessageQueue.KeyDown(Key: TVirtualKeyCode): Boolean;
var
  I: TMessage;
begin
  for I in FQueue do
    if (I.Msg  = msgKeyDown) and (I.KeyboardArgs.Key = Key) then
      Exit(True);
  Result := False;
end;

function TMessageQueue.KeyUp(Key: TVirtualKeyCode): Boolean;
var
  I: TMessage;
begin
  for I in FQueue do
    if (I.Msg  = msgKeyUp) and (I.KeyboardArgs.Key = Key) then
      Exit(True);
  Result := False;
end;

function TMessageQueue.MouseDown(Button: TMouseButton): Boolean;
var
  I: TMessage;
begin
  for I in FQueue do
    if (I.Msg  = msgMouseButtonDown) and (I.MouseButtonArgs.Button = Button) then
      Exit(True);
  Result := False;
end;

function TMessageQueue.MouseDown(Button: TMouseButton; out X, Y: Integer; out Shift: TShiftState): Boolean;
var
  I: TMessage;
begin
  for I in FQueue do
    if (I.Msg  = msgMouseButtonDown) and (I.MouseButtonArgs.Button = Button) then
    begin
      X := I.MouseButtonArgs.X;
      Y := I.MouseButtonArgs.Y;
      Shift := I.MouseButtonArgs.Shift;
      Exit(True);
    end;
  Result := False;
end;

function TMessageQueue.MouseUp(Button: TMouseButton): Boolean;
var
  I: TMessage;
begin
  for I in FQueue do
    if (I.Msg  = msgMouseButtonUp) and (I.MouseButtonArgs.Button = Button) then
      Exit(True);
  Result := False;
end;


function TMessageQueue.MouseUp(Button: TMouseButton; out X, Y: Integer; out Shift: TShiftState): Boolean;
var
  I: TMessage;
begin
  for I in FQueue do
    if (I.Msg  = msgMouseButtonUp) and (I.MouseButtonArgs.Button = Button) then
    begin
      X := I.MouseButtonArgs.X;
      Y := I.MouseButtonArgs.Y;
      Shift := I.MouseButtonArgs.Shift;
      Exit(True);
    end;
  Result := False;
end;

constructor TStopwatch.Create(SyncTime: Double = 0);
begin
  inherited Create;
  Reset;
  if SyncTime > 0.01 then
  begin
    FStart := SyncTime;
    FStop := SyncTime;
  end;
end;

procedure TStopwatch.Reset;
begin
  FStart := Now;
  FStop := FStop;
  FTime := 0;
  FInterval := 0;
  FFrame := 0;
  FSeconds := 0;
end;

function TStopwatch.Step: Double;
var
  I: LargeWord;
begin
  if FPaused then
    Exit(FTime);
  FTime := Now;
  FInterval := FTime - FStop;
  FStop := FTime;
  FTime := FStop - FStart;
  I := Trunc(FTime);
  if I > FSeconds then
  begin
    FSeconds := I;
    FFrameRate := FFrame;
    FFrame := 0;
  end;
  Inc(FFrame);
  Result := FTime;
end;

procedure TStopwatch.SetPaused(Value: Boolean);
var
  Last: Double;
begin
  if Value <> FPaused then
  begin
    FPaused := Value;
    if Paused then
      Step
    else
    begin
      Last := FStop;
      Step;
      FStart := FStart + (FStop - Last);
      FTime := FStop - FStart;
    end;
  end;
end;

constructor TTimer.Create;
begin
  inherited Create;
  if not FInitialized then
  begin
    FInitialized := True;
    SDL_InitSubSystem(SDL_INIT_TIMER);
  end;
  FInterval := 1000;
end;

destructor TTimer.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

procedure TTimer.DoExpired(var Args: TEmptyArgs);
begin
  if Assigned(FOnExpired) then
    FOnExpired(Self, Args);
end;

function TimerCallback(interval: Uint32; param: Pointer): Uint32; cdecl;
var
  Timer: TTimer absolute param;
begin
  if Timer.FEnabled then
  begin
    Timer.DoExpired(EmptyArgs);
    {TODO: Determine why this is only invoke one time}
    Result := Interval;
  end
  else
    Result := 0;
end;

procedure TTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
      FTimer := TimerCallback(FInterval, Self)
    else
    begin
      SDL_RemoveTimer(FTimer);
      FTimer := 0;
    end;
  end;
end;

procedure TTimer.SetInterval(Value: LongWord);
begin
  Enabled := False;
  if Value < 1 then
    Value := 1;
  FInterval := Value;
end;

{ THardwareCollection<T> }

constructor THardwareCollection<T>.Create;
begin
  inherited Create;
  ScanHardware;
end;

procedure THardwareCollection<T>.ScanHardware;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Free;
  Items := nil;
end;

var
  AudioLockCount: Integer;

procedure LockAudio;
begin
  if AudioLockCount = 0 then
    SDL_LockAudio;
  Inc(AudioLockCount);
end;

procedure UnlockAudio;
begin
  Dec(AudioLockCount);
  if AudioLockCount = 0 then
    SDL_UnlockAudio;
end;

function PositionToDuration(Value: LongWord): Double;
begin
  case TAudio.AudioSpec.format and $FF of
    $10: Value := Value shr 1;
    $20: Value := Value shr 2;
  end;
  Result := Value / (TAudio.AudioSpec.freq * TAudio.AudioSpec.channels);
end;

function DurationToPosition(const Value: Double): LongWord;
begin
  Result := Round(Value * TAudio.AudioSpec.freq * TAudio.AudioSpec.channels);
  case TAudio.AudioSpec.format and $FF of
    $10: Result := Result shl 1;
    $20: Result := Result shl 2;
  end;
end;

constructor TAudioSample.Create(Size: LongWord);
begin
  inherited Create;
  FSize := Size;
  FData := GetMem(FSize);
end;

destructor TAudioSample.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

function TAudioSample.Play: Integer;
var
  Bank: TAudioBank;
  Time: Double;
  I, J: Integer;
begin
  Time := 0;
  I := -1;
  J := -1;
  LockAudio;
  for Bank in Audio.Banks do
  begin
    Inc(I);
    { Skip banks which are locked }
    if Bank.FLocked then
      Continue;
    { This bank hasn't been used yet }
    if (Bank.FSample = nil) or (Bank.FTime = 0) then
    begin
      J := I;
      Break;
    end;
    { Capture the time from the first unlocked bank }
    if J < 0 then
    begin
      J := I;
      Time := Bank.FTime;
    end
    { This bank is the oldest so far }
    else if Time < Bank.FTime then
    begin
      J := I;
      Time := Bank.FTime;
    end;
  end;
  if J > -1 then
    Play(J);
  UnlockAudio;
  Result := J;
end;

procedure TAudioSample.Play(Bank: Integer);
var
  B: TAudioBank;
begin
  LockAudio;
  B := Audio.Banks[Bank];
  B.FSample := Self;
  B.FPosition := 0;
  B.FTime := Now + Duration;
  B.FState := abPlaying;
  UnlockAudio;
end;

function TAudioSample.PlayVolume(Volume: Byte): Integer;
var
  I: Integer;
begin
  LockAudio;
  I := Play;
  if I > -1 then
    Audio.Banks[I].Volume := Volume;
  UnlockAudio;
  Result := I;
end;

procedure TAudioSample.PlayVolume(Volume: Byte; Bank: Integer);
begin
  LockAudio;
  Play(Bank);
  Audio.Banks[Bank].Volume := Volume;
  UnlockAudio;
end;

function TAudioSample.GetDuration: Double;
begin
  Result := PositionToDuration(FSize);
end;

{ TAudioSamples }

function TAudioSamples.Add(Ops: PSDL_RWops): TAudioSample;
var
  Wave: TSDL_AudioSpec;
  Convert: TSDL_AudioCVT;
  Buf: PUint8;
  Len: Uint32;
begin
  if Ops = nil then
    Exit(nil);
  if SDL_LoadWAV_RW(Ops, 1, Wave, Buf, Len) = nil then
    Exit(nil);
  SDL_BuildAudioCVT(Convert, Wave.format, Wave.channels, Wave.freq,
    TAudio.AudioSpec.format, TAudio.AudioSpec.channels, TAudio.AudioSpec.freq);
  Convert.buf := GetMem(Len * Convert.len_mult);
  Move(Buf^, Convert.buf^, Len);
  SDL_FreeWAV(Buf);
  Convert.len := Len;
  SDL_ConvertAudio(Convert);
  Result := TAudioSample.Create(Convert.len_cvt);
  Result.FData := Convert.buf;
  Items.Add(Result);
end;

function TAudioSamples.Add(const FileName: string): TAudioSample;
var
  S: AnsiString;
begin
  S := FileName;
  Result := Add(SDL_RWFromFile(PAnsiChar(S), 'rb'));
  if Result <> nil then
    Result.Name := FileName;
end;

function TAudioSamples.Add(Stream: TStream): TAudioSample;
var
  M: TMemoryStream;
begin
  if Stream is TMemoryStream then
  begin
    M := Stream as TMemoryStream;
    Result := Add(SDL_RWFromMem(M.Memory, M.Size));
  end
  else
  begin
    M := TMemoryStream.Create(Stream.Size);
    try
      M.Copy(Stream, 0);
      Result := Add(SDL_RWFromMem(M.Memory, M.Size));
    finally
      M.Free;
    end;
  end;
end;

procedure TAudioSamples.Remove(Sample: TAudioSample);
var
  B: TAudioBank;
  I: Integer;
begin
  LockAudio;
  for B in Audio.Banks do
    if B.FSample = Sample then
    begin
      B.FSample := nil;
      B.FLocked := False;
      B.FPosition := 0;
      B.FState := abStopped;
    end;
  UnlockAudio;
  for I := 0 to Items.Count - 1 do
    if Items[I] = Sample then
    begin
      Items.Delete(I);
      Sample.Free;
      Break;
    end;
end;

function TAudioSamples.GetSample(const Name: string): TAudioSample;
var
  S: TAudioSample;
begin
  for S in Items do
    if S.Name = Name then
      Exit(S);
  Result := nil;
end;

function TAudioSamples.GetSampleByIndex(Index: Integer): TAudioSample;
begin
  Result := Items[Index];
end;

constructor TAudioBank.Create;
begin
  inherited Create;
  FVolume := $FF;
end;

procedure TAudioBank.Play;
begin
  State := abPlaying;
end;

procedure TAudioBank.Stop;
begin
  State := abStopped;
end;

procedure TAudioBank.Pause;
begin
  State := abPaused;
end;

function TAudioBank.GetDuration: Double;
begin
  if FSample = nil then
    Result := 0
  else
    Result := FSample.Duration;
end;

function TAudioBank.GetDimension: Double;
begin
  LockAudio;
  Result := PositionToDuration(FPosition);
  UnlockAudio;
end;

procedure TAudioBank.SetDimension(const Value: Double);
const
  Tolerance = 0.001;
begin
  LockAudio;
  if Value < Tolerance then
    FPosition := 0
  else
    FPosition := DurationToPosition(Value);
  UnlockAudio;
end;

procedure TAudioBank.SetState(Value: TAudioBankState);
begin
  LockAudio;
  if Value <> FState then
  begin
    FState := Value;
    if FState = abStopped then
      FPosition := 0;
  end;
  UnlockAudio;
end;

procedure TAudioBank.SetVolume(Value: Byte);
begin
  LockAudio;
  FVolume := Value;
  UnlockAudio;
end;

constructor TAudioBanks.Create;
var
  I: Integer;
begin
  inherited Create;
  Items.Count := AudioBankCount;
  for I := 0 to AudioBankCount - 1 do
    Items[I] := TAudioBank.Create;
end;

function TAudioBanks.GetBank(Index: Integer): TAudioBank;
begin
  Result := Items[Index];
end;

{ AudioMixer a callback which executes in a background thread. It is used by
  the SDL audio system and feed samples to the SDL_MixAudio API
  Remarks
  This callback is private to the Bare.Game namespace }

procedure AudioMixer(userdata: Pointer; stream: PUInt8; len: LongInt); cdecl;
var
  Audio: TAudio;
  Bank: TAudioBank;
  Sample: TAudioSample;
  Amount: LongInt;
  Cursor: PUint8;
  V: Single;
  B: Byte;
  I: Integer;
begin
  Audio := TAudio(userdata);
  { Fill the stream with silence }
  FillChar(stream^, len, TAudio.AudioSpec.silence);
  for I := 0 to Audio.Banks.Count - 1 do
  begin
    Bank := Audio.Banks[I];
    Sample := Bank.Sample;
    if Sample = nil then
      Continue;
    { Don't mix banks which are paused or stopped }
    if Bank.FState <> abPlaying then
      Continue;
    Amount := Sample.FSize - Bank.FPosition;
    if Amount > len then
      Amount := len;
    Cursor := Sample.FData;
    if Cursor = nil then
      Continue;
    { Advance the cursor positiom }
    Inc(Cursor, Bank.FPosition);
    Inc(Bank.FPosition, Amount);
    { Stop or repeat banks when the cursor reaches the end of the sample }
    if Bank.FPosition >= Sample.FSize then
    begin
      Bank.FPosition := 0;
      if not Bank.FLooping then
        Bank.FState := abStopped;
      Continue;
    end;
    if Bank.FMuted then
      Continue;
    V := (Audio.FMasterVolume / $FF) * (Bank.FVolume / $FF) * SDL_MIX_MAXVOLUME;
    B := Round(V);
    if B > 0 then
      SDL_MixAudio(stream, Cursor, Amount, B);
  end;
end;

constructor TAudio.Create;
var
  DesiredSpec: TSDL_AudioSpec;
begin
  inherited Create;
  FSamples := TAudioSamples.Create;
  FBanks := TAudioBanks.Create;
  FMasterVolume := $FF;
  { Initialize SDL audio }
  DesiredSpec.format := AUDIO_S16;
  DesiredSpec.channels := AUDIO_CHAN_STEREO;
  { FM Quality uses half the data and less CPU processing power }
  DesiredSpec.freq := AUDIO_FREQ_FM_QAULITY;
  DesiredSpec.samples := AUDIO_SAMPLE_MEDIUM;
  DesiredSpec.callback := @AudioMixer;
  DesiredSpec.userdata := Self;
  SDL_OpenAudio(@DesiredSpec, @TAudio.AudioSpec);
  SDL_PauseAudio(0);
end;

destructor TAudio.Destroy;
begin
  FSamples.Free;
  FBanks.Free;
  inherited Destroy;
end;

function KeyModToShiftState(KeyMod: LongWord): TShiftState;
begin
  Result := [];
  if KMOD_CTRL and KeyMod <> 0 then
    Include(Result, ssCtrl);
  if KMOD_SHIFT and KeyMod <> 0 then
    Include(Result, ssShift);
  if KMOD_ALT and KeyMod <> 0 then
    Include(Result, ssAlt);
end;

procedure TKeyboard.Scan;
begin
  Lock;
  try
    FKeys := SDL_GetKeyboardState(@FCount);
  finally
    Unlock;
  end;
end;

function TKeyboard.GetKey(Index: TVirtualKeyCode): Boolean;
var
  ScanCode: LongWord;
  Key: PUint8;
begin
  if FKeys = nil then
    Scan;
  ScanCode := SDL_GetScancodeFromKey(LongWord(Index));
  if ScanCode >= FCount then
    Exit(False);
  Key := FKeys;
  Inc(Key, ScanCode);
  Result := Key^ <> 0;
end;

function TKeyboard.GetShiftState: TShiftState;
begin
  if FKeys = nil then
    Scan;
  Result := KeyModToShiftState(SDL_GetModState);
end;

procedure TMouse.Scan;
var
  Button: TMouseButton;
  B: Uint32;
begin
  Lock;
  try
    FButtons := [];
    B := SDL_GetMouseState(FX, FY);
    for Button := Low(Button) to High(Button) do
      if SDL_Button(Ord(Button) + 1) and B <> 0 then
        Include(FButtons, Button);
  finally
    Unlock;
  end;
end;

procedure TMouse.Move(X, Y: Integer);
begin
  SDL_WarpMouseInWindow(nil, X, Y);
end;

function TMouse.GetCoord: TPointI;
begin
  Result.X := FX;
  Result.Y := FY;
end;

function TMouse.GetCaptured: Boolean;
begin
  Result := SDL_GetRelativeMouseMode;
end;

procedure TMouse.SetCaptured(Value: Boolean);
begin
  SDL_SetRelativeMouseMode(Value);
end;

function TMouse.GetVisible: Boolean;
begin
  Result := SDL_ShowCursor(-1) <> 0;
end;

procedure TMouse.SetVisible(Value: Boolean);
begin
  if Value then
    SDL_ShowCursor(1)
  else
    SDL_ShowCursor(0);
end;

constructor TJoystickPart<T>.Create(Joystick: PSDL_Joystick);
begin
  inherited Create;
  FJoystick := Joystick;
  DoScan;
end;

function TJoystickPart<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FValues);
end;

procedure TJoystickPart<T>.Scan;
begin
  if FJoystick = nil then
    FValues := nil
  else
  begin
    Lock;
    try
      DoScan;
    finally
      Unlock;
    end;
  end;
end;

function TJoystickPart<T>.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TJoystickPart<T>.GetValue(Index: Integer): T;
begin
  Result := FValues[Index];
end;

{ Scan retrieves the state of the analog sticks }

procedure TJoystickAxes.DoScan;
var
  I: Integer;
begin
  I := SDL_JoystickNumAxes(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Count := I;
  while I > 0 do
  begin
    Dec(I);
    FValues[I] := SDL_JoystickGetAxis(FJoystick, I);
  end;
end;

{ Scan retrieves the pressed state the buttons }

procedure TJoystickButtons.DoScan;
var
  I: Integer;
begin
  I := SDL_JoystickNumButtons(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Count := I;
  while I > 0 do
  begin
    Dec(I);
    FValues[I] := SDL_JoystickGetButton(FJoystick, I) <> 0;
  end;
end;

{ Scan retrieves the state the hats }

procedure TJoystickHats.DoScan;
var
  I: Integer;
begin
  I := SDL_JoystickNumHats(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Count := I;
  while I > 0 do
  begin
    Dec(I);
    case SDL_JoystickGetHat(FJoystick, I) of
      SDL_HAT_UP: FValues[I] := hatUp;
      SDL_HAT_RIGHT: FValues[I] := hatRight;
      SDL_HAT_DOWN: FValues[I] := hatDown;
      SDL_HAT_LEFT: FValues[I] := hatLeft;
      SDL_HAT_RIGHTUP: FValues[I] := hatRightUp;
      SDL_HAT_RIGHTDOWN: FValues[I] := hatRightDown;
      SDL_HAT_LEFTUP: FValues[I] := hatLeftUp;
      SDL_HAT_LEFTDOWN: FValues[I] := hatLeftDown;
    else
      FValues[I] := hatCenter;
    end;
  end;
end;

{ Scan retrieves the pressed state the trackballs }

procedure TJoystickTrackballs.DoScan;
var
  B: TJoystickTrackball;
  I: Integer;
begin
  I := SDL_JoystickNumBalls(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Count := I;
  while I > 0 do
  begin
    Dec(I);
    SDL_JoystickGetBall(FJoystick, I, B.XDelta, B.YDelta);
    FValues[I] := B;
  end;
end;

constructor TJoystick.Create(Index: Integer);
begin
  inherited Create;
  FIndex := Index;
  FJoystick := SDL_JoystickOpen(Index);
  FAxes := TJoystickAxes.Create(FJoystick);
  FButtons := TJoystickButtons.Create(FJoystick);
  FHats := TJoystickHats.Create(FJoystick);
  FTrackballs := TJoystickTrackballs.Create(FJoystick);
end;

destructor TJoystick.Destroy;
begin
  FAxes.Free;
  FButtons.Free;
  FHats.Free;
  FTrackballs.Free;
  SDL_JoystickClose(FJoystick);
  inherited Destroy;
end;

{ Scan retrieves the state of the joystick }

procedure TJoystick.Scan;
begin
  Lock;
  try
    FAxes.Scan;
    FButtons.Scan;
    FHats.Scan;
    FTrackballs.Scan;
  finally
    Unlock;
  end;
end;

procedure TJoysticks.ScanHardware;
var
  I: Integer;
begin
  Lock;
  try
    inherited ScanHardware;
    I := SDL_NumJoysticks;
    if I < 1 then
      Exit;
    Items.Count := I;
    for I := 0 to Items.Count - 1 do
      Items[I] := TJoystick.Create(I);
  finally
    Unlock;
  end;
end;

function TJoysticks.GetJoystick(Index: Integer): TJoystick;
begin
  Result := Items[Index];
end;

constructor TScreen.Create(Index: Integer);
begin
  inherited Create;
  FIndex := Index;
end;

procedure TScreen.GetDisplayModes(out Modes: TDisplayModes);
var
  List: TArrayList<TDisplayMode>;
  Mode: TSDL_DisplayMode;
  I: Integer;
begin
  Modes := nil;
  I := SDL_GetNumDisplayModes(FIndex);
  if I < 1 then
    Exit;
  List.Count := I;
  for I := 0 to List.Count - 1 do
  begin
    SDL_GetDisplayMode(FIndex, I, Mode);
    List.Items[I].Width := Mode.w;
    List.Items[I].Height := Mode.h;
    List.Items[I].ColorDepth := SDL_BITSPERPIXEL(Mode.format);
    List.Items[I].RefreshRate := Mode.refresh_rate;
  end;
  Modes := List.Items;
end;

procedure TScreen.GetBounds(out Rect: TRectI);
var
  R: TSDL_Rect;
begin
  if SDL_GetDisplayBounds(FIndex, R) = 0 then
    Rect := TRectI.Create(R.x, R.y, R.w, R.h)
  else
    Rect := TRectI.Create;
end;

function TScreen.GetName: string;
begin
  Result := SDL_GetDisplayName(FIndex);
end;

procedure TScreens.ScanHardware;
var
  I: Integer;
begin
  Lock;
  try
    inherited ScanHardware;
    I := SDL_GetNumVideoDisplays;
    if I < 1 then
      Exit;
    Items.Count := I;
    for I := 0 to Items.Count - 1 do
      Items[I] := TScreen.Create(I);
  finally
    Unlock;
  end;
end;

function TScreens.GetDriverName: string;
begin
  Result := SDL_GetCurrentVideoDriver;
end;

function TScreens.GetPrimary: TScreen;
begin
  if Count > 0 then
    Result := Items[0]
  else
    Result := nil;
end;

function TScreens.GetScreen(Index: Integer): TScreen;
begin
  Result := Items[Index];
end;

type
  TWindowThread = class(TThread)
  private
    FWindow: TWindow;
    FStopwatch: TStopwatch;
  public
    constructor Create(Window: TWindow; const Time: Double);
    destructor Destroy; override;
    property Window: TWindow read FWindow;
    property Stopwatch: TStopwatch read FStopwatch;
  end;

  TLogicThread = class(TWindowThread)
  protected
    function Execute: LongWord; override;
  end;

  TRenderThread = class(TWindowThread)
  protected
    function Execute: LongWord; override;
  end;

constructor TWindowThread.Create(Window: TWindow; const Time: Double);
begin
  FWindow := Window;
  FStopwatch := TStopwatch.Create(Time);
  inherited Create;
end;

destructor TWindowThread.Destroy;
begin
  inherited Destroy;
  FStopwatch.Free;
end;

function TLogicThread.Execute: LongWord;
const
  MinInterval = 1 / 120;
begin
  Window.LogicInitialize;
  while not Terminated do
  begin
    ScanInput;
    Window.Logic(Stopwatch);
    Stopwatch.Step;
    { Limit logic updates to 120 per second }
    if Stopwatch.Interval < MinInterval then
      Sleep(Round((MinInterval - Stopwatch.Interval) * 1000));
  end;
  Window.LogicFinalize;
  Result := 0;
end;

function TRenderThread.Execute: LongWord;
begin
  Window.FContext := SDL_GL_CreateContext(Window.FWindow);
  SDL_GL_MakeCurrent(FWindow, FWindow.FContext);
  SDL_GL_SetSwapInterval(FWindow.FCreateParams.Attributes.VerticalSync);
  Window.RenderInitialize;
  while not Terminated do
  begin
    Window.Render(Stopwatch);
    Window.SwapBuffers;
    Stopwatch.Step;
  end;
  Window.RenderFinalize;
  SDL_GL_DeleteContext(Window.FContext);
  Window.FContext := nil;
  Result := 0;
end;

constructor TWindow.Create;

  procedure DefaultWindowParams(out Params: TCreateParams);
  const
    DefaultWidth = 640;
    DefaultHeight = 480;
    DefaultAttributes: TAttributes = (
      StencilBits: 0;
      DepthBits: 16;
      MultiSamples: 0;
      VerticalSync: 1;
      HardwareAcceleration: 1;
      Stereo: 0;
      MajorVersion: 0;
      MinorVersion: 0;
    );
    DefaultStyle = [wsBorder, wsVisible];
    DefaultMultithreaded = False;
  var
    Mode: TSDL_DisplayMode;
  begin
    SDL_GetDesktopDisplayMode(0, Mode);
    Params.Caption := ClassName;
    Params.X := WindowPosCentered;
    Params.Y := WindowPosCentered;
    Params.Width := DefaultWidth;
    Params.Height := DefaultHeight;
    Params.DisplayMode.Width := Mode.w;
    Params.DisplayMode.Height := Mode.h;
    Params.DisplayMode.ColorDepth := SDL_BITSPERPIXEL(Mode.format);
    Params.DisplayMode.RefreshRate := Mode.refresh_rate;
    Params.Style := DefaultStyle;
    Params.Attributes := DefaultAttributes;
    Params.Multithreaded := DefaultMultithreaded;
  end;

begin
  inherited Create;
  FMessageQueue := TMessageQueue.Create;
  DefaultWindowParams(FCreateParams);
  CreateParams(FCreateParams);
  CreateWindow(FCreateParams);
  FMultithreaded := FCreateParams.Multithreaded;
end;

destructor TWindow.Destroy;
begin
  if FContext <> nil then
    SDL_GL_DeleteContext(FContext);
  SDL_DestroyWindow(FWindow);
  FMessageQueue.Free;
  inherited Destroy;
end;

procedure TWindow.CreateParams(var Params: TCreateParams);
begin
end;

procedure TWindow.CreateWindow(var Params: TCreateParams);
const
  { For sanity we always use 8 bits for every channel in RGBA (4x8 = 32 bits) }
  ChannelBits = 8;
  Border: array[Boolean] of Uint32 = (SDL_WINDOW_BORDERLESS, 0);
  Resizeable: array[Boolean] of Uint32 = (0, SDL_WINDOW_RESIZABLE);
  Visible: array[Boolean] of Uint32 = (SDL_WINDOW_HIDDEN, SDL_WINDOW_SHOWN);
var
  Style: Uint32;
  Mode: TSDL_DisplayMode;
  S: AnsiString;
begin
  { Attributes must be set before the window is created }
  { See: http://wiki.libsdl.org/moin.fcg/SDL_GL_SetAttribute#Remarks }
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, ChannelBits);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, ChannelBits);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, ChannelBits);
  SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, ChannelBits);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, Params.Attributes.StencilBits);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, Params.Attributes.DepthBits);
  SDL_GL_SetAttribute(SDL_GL_STEREO, Params.Attributes.Stereo);
  if Params.Attributes.MultiSamples > 0 then
  begin
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, Params.Attributes.MultiSamples);
  end
  else
  begin
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 0);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 0);
  end;
  SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL, Params.Attributes.HardwareAcceleration);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, Params.Attributes.MajorVersion);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, Params.Attributes.MinorVersion);
  S := Params.Caption;
  Style := SDL_WINDOW_OPENGL;
  Style := Style or Border[wsBorder in Params.Style];
  {TODO: Allow for full screen on monidtors other than the primary}
  SDL_GetCurrentDisplayMode(0, Mode);
  Mode.w := Params.Width;
  Mode.h := Params.Height;
  if wsFulldesktop in Params.Style then
  begin
    Style := Style or SDL_WINDOW_FULLSCREEN_DESKTOP;
    Params.Style := Params.Style - [wsFullscreen, wsMaximized];
    FFullscreen := True;
  end
  else if wsFullscreen in Params.Style then
  begin
    {TODO: Determine why SDL_SetWindowDisplayMode does not work}
    Style := Style or SDL_WINDOW_FULLSCREEN;
    Mode.w := Params.DisplayMode.Width;
    Mode.h := Params.DisplayMode.Height;
    Params.Style := Params.Style - [wsMaximized];
    FFullscreen := True;
  end
  else if wsMaximized in Params.Style then
    Style := Style or SDL_WINDOW_MAXIMIZED;
  Style := Style or Resizeable[wsResizeable in Params.Style];
  FVisible := wsVisible in Params.Style;
  Style := Style or Visible[FVisible];
  SDL_ClearError;
  FWindow := SDL_CreateWindow(PAnsiChar(S), Params.X, Params.Y, Mode.w, Mode.h, Style);
  if FWindow = nil then
  begin
    {TODO: Determine why on some systems asking for multisampling causes
      SDL_CreateWindow to fail}
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 0);
    SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 0);
    SDL_ClearError;
    FWindow := SDL_CreateWindow(PAnsiChar(S), Params.X, Params.Y, Mode.w, Mode.h, Style);
    if FWindow = nil then
      raise ESDLError.CreateFunc('SDL_CreateWindow');
  end;
  SDL_SetWindowData(FWindow, WindowInstance, Self);
end;

procedure TWindow.CreateLoop(Stopwatch: TStopwatch);
begin
  FMultithreaded := Multithreaded;
  if FMultithreaded then
  begin
    FLogicThread := TLogicThread.Create(Self, Stopwatch.Time);
    FRenderThread := TRenderThread.Create(Self, Stopwatch.Time);
  end
  else
  begin
    LogicInitialize;
    FContext := SDL_GL_CreateContext(FWindow);
    SDL_GL_MakeCurrent(FWindow, FContext);
    SDL_GL_SetSwapInterval(FCreateParams.Attributes.VerticalSync);
    RenderInitialize;
  end;
end;

procedure TWindow.DestroyLoop;
begin
  if not FMultithreaded then
  begin
    RenderFinalize;
    LogicFinalize;
  end;
end;

procedure TWindow.RunLoop(Stopwatch: TStopwatch);
begin
  if not FMultithreaded then
  begin
    ScanInput;
    Logic(Stopwatch);
    Render(Stopwatch);
    SwapBuffers;
  end;
end;

procedure TWindow.Close;
begin
  DoClose(EmptyArgs);
end;

procedure TWindow.LogicInitialize;
begin
end;

procedure TWindow.Logic(Stopwatch: TStopwatch);
begin
  MessageQueue.Remove;
end;

procedure TWindow.LogicFinalize;
begin
end;

procedure TWindow.RenderInitialize;
begin
end;

procedure TWindow.Render(Stopwatch: TStopwatch);
begin
end;

procedure TWindow.RenderFinalize;
begin
end;

procedure TWindow.SwapBuffers;
begin
  SDL_GL_SwapWindow(FWindow);
end;

procedure TWindow.DispatchMessage(var Message: TMessage);
begin
  MessageQueue.Add(Message);
  case Message.Msg of
    SDL_WINDOWEVENT_SHOWN:
      begin
        FVisible := True;
        DoShow(Message.EmptyArgs);
      end;
    SDL_WINDOWEVENT_HIDDEN:
      begin
        FVisible := False;
        DoHide(Message.EmptyArgs);
      end;
    SDL_WINDOWEVENT_CLOSE:
      if not FClosed then
        DoClose(Message.EmptyArgs);
    SDL_WINDOWEVENT_FOCUS_GAINED,SDL_WINDOWEVENT_FOCUS_LOST:
      DoFocus(Message.FocusArgs);
    SDL_WINDOWEVENT_MOVED: DoMove(Message.MoveResizeArgs);
    SDL_WINDOWEVENT_RESIZED: DoResize(Message.MoveResizeArgs);
    SDL_WINDOWEVENT_SIZE_CHANGED: DoSizeChanged(Message.EmptyArgs);
    SDL_QUIT_EVENT:
      if not FClosed then
        DoClose(Message.EmptyArgs);
    SDL_KEYDOWN: DoKeyDown(Message.KeyboardArgs);
    SDL_KEYUP: DoKeyUp(Message.KeyboardArgs);
    SDL_MOUSEMOTION: DoMouseMove(Message.MouseMoveArgs);
    SDL_MOUSEBUTTONDOWN: DoMouseDown(Message.MouseButtonArgs);
    SDL_MOUSEBUTTONUP: DoMouseUp(Message.MouseButtonArgs);
    SDL_MOUSEWHEEL: DoMouseWheel(Message.MouseWheelArgs);
    SDL_WINDOWEVENT_ENTER: DoMouseEnter(Message.EmptyArgs);
    SDL_WINDOWEVENT_LEAVE: DoMouseLeave(Message.EmptyArgs);
    SDL_JOYAXISMOTION: DoJoystickAxis(Message.JoystickAxisArgs);
    SDL_JOYBALLMOTION: DoJoystickBall(Message.JoystickBallArgs);
    SDL_JOYHATMOTION: DoJoystickHat(Message.JoystickHatArgs);
    SDL_JOYBUTTONDOWN: DoJoystickButtonDown(Message.JoystickButtonArgs);
    SDL_JOYBUTTONUP: DoJoystickButtonUp(Message.JoystickButtonArgs);
    SDL_JOYDEVICEADDED: DoJoystickAdded(Message.JoystickDeviceArgs);
    SDL_JOYDEVICEREMOVED: DoJoystickRemoved(Message.JoystickDeviceArgs);
  end;
end;

{ Invokes OnQueryClose and OnClose events }

procedure TWindow.DoClose(var Args: TEmptyArgs);
var
  QueryCloseArgs: TQueryCloseArgs;
begin
  if FClosed then
    Exit;
  QueryCloseArgs.CanClose := True;
  if Assigned(FOnQueryClose) then
    FOnQueryClose(Self, QueryCloseArgs);
  FClosed := QueryCloseArgs.CanClose;
  if FClosed then
  try
    if Assigned(FOnClose) then
      FOnClose(Self, Args);
  finally
    if FMultithreaded then
    begin
      FLogicThread.Terminate;
      FRenderThread.Terminate;
      FLogicThread.Free;
      FRenderThread.Free;
    end;
  end;
end;

{ Invokes OnFocus event }

procedure TWindow.DoFocus(var Args: TFocusArgs);
begin
  if Assigned(FOnFocus) then
    FOnFocus(Self, Args);
end;

{ Invokes OnKeyDown event }

procedure TWindow.DoKeyDown(var Args: TKeyboardArgs);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Args);
end;

{ Invokes OnKeyUp event }

procedure TWindow.DoKeyUp(var Args: TKeyboardArgs);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Args);
  if Args.Key = VK_ESCAPE then
    Close;
end;

{ Invokes OnMouseMove event }

procedure TWindow.DoMouseMove(var Args: TMouseMoveArgs);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Args);
end;

{ Invokes OnMouseDown event }

procedure TWindow.DoMouseDown(var Args: TMouseButtonArgs);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Args);
end;

{ Invokes OnMouseUp event }

procedure TWindow.DoMouseUp(var Args: TMouseButtonArgs);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Args);
end;

{ Invokes OnMouseWheel event }

procedure TWindow.DoMouseWheel(var Args: TMouseWheelArgs);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Args);
end;

{ Invokes OnMouseEnter event }

procedure TWindow.DoMouseEnter(var Args: TEmptyArgs);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self, Args);
end;

{ Invokes OnMouseLeave event }

procedure TWindow.DoMouseLeave(var Args: TEmptyArgs);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self, Args);
end;

{ Invokes OnMove event }

procedure TWindow.DoMove(var Args: TMoveResizeArgs);
begin
  if Assigned(FOnMove) then
    FOnMove(Self, Args);
end;

{ Invokes OnResize event }

procedure TWindow.DoResize(var Args: TMoveResizeArgs);
begin
  if Assigned(FOnResize) then
    FOnResize(Self, Args);
end;

{ Invokes OnSizeChange event }

procedure TWindow.DoSizeChanged(var Args: TEmptyArgs);
begin
  if Assigned(FOnSizeChanged) then
    FOnSizeChanged(Self, Args);
end;

{ Invokes OnShow event }

procedure TWindow.DoShow(var Args: TEmptyArgs);
begin
  if Assigned(FOnShow) then
    FOnShow(Self, Args);
end;

{ Invokes OnHide event }

procedure TWindow.DoHide(var Args: TEmptyArgs);
begin
  if Assigned(FOnHide) then
    FOnHide(Self, Args);
end;

{ Invokes OnJoystickAxis event }

procedure TWindow.DoJoystickAxis(var Args: TJoystickAxisArgs);
begin
  if Assigned(FOnJoystickAxis) then
    FOnJoystickAxis(Self, Args);
end;

{ Invokes OnJoystickBall event }

procedure TWindow.DoJoystickBall(var Args: TJoystickBallArgs);
begin
  if Assigned(FOnJoystickBall) then
    FOnJoystickBall(Self, Args);
end;

{ Invokes OnJoystickHat event }

procedure TWindow.DoJoystickHat(var Args: TJoystickHatArgs);
begin
  if Assigned(FOnJoystickHat) then
    FOnJoystickHat(Self, Args);
end;

{ Invokes OnJoystickButtonDown event }

procedure TWindow.DoJoystickButtonDown(var Args: TJoystickButtonArgs);
begin
  if Assigned(FOnJoystickButtonDown) then
    FOnJoystickButtonDown(Self, Args);
end;

{ Invokes OnJoystickButtonUp event }

procedure TWindow.DoJoystickButtonUp(var Args: TJoystickButtonArgs);
begin
  if Assigned(FOnJoystickButtonUp) then
    FOnJoystickButtonUp(Self, Args);
end;

{ Invokes OnJoystickAdded event }

procedure TWindow.DoJoystickAdded(var Args: TJoystickDeviceArgs);
begin
  if Assigned(FOnJoystickAdded) then
    FOnJoystickAdded(Self, Args);
end;

{ Invokes OnJoystickRemoved event }

procedure TWindow.DoJoystickRemoved(var Args: TJoystickDeviceArgs);
begin
  if Assigned(FOnJoystickRemoved) then
    FOnJoystickRemoved(Self, Args);
end;

procedure TWindow.Maximize;
begin
  SDL_MaximizeWindow(FWindow);
end;

procedure TWindow.Minimize;
begin
  SDL_MinimizeWindow(FWindow);
end;

procedure TWindow.Restore;
begin
  SDL_RestoreWindow(FWindow);
end;

procedure TWindow.Show;
begin
  SDL_ShowWindow(FWindow);
end;

procedure TWindow.Hide;
begin
  SDL_HideWindow(FWindow);
end;

procedure TWindow.Move(X, Y: Integer);
begin
  SDL_SetWindowPosition(FWindow, X, Y);
end;

procedure TWindow.Resize(Width, Height: Integer);
var
  Args: TMoveResizeArgs;
begin
  SDL_SetWindowSize(FWindow, Width, Height);
  Args.X := Width;
  Args.Y := Height;
  DoResize(Args);
end;

procedure TWindow.GetBounds(out Rect: TRectI);
begin
  SDL_GetWindowPosition(FWindow, Rect.X, Rect.Y);
  SDL_GetWindowSize(FWindow, Rect.Width, Rect.Height);
end;

procedure TWindow.SetBounds(const Rect: TRectI);
begin
  SDL_SetWindowPosition(FWindow, Rect.X, Rect.Y);
  SDL_SetWindowSize(FWindow, Rect.Width, Rect.Height);
end;

procedure TWindow.SetDisplayMode(const Mode: TDisplayMode);
var
  M: TSDL_DisplayMode;
begin
  M.h := Mode.Height;
  M.w := Mode.Width;
  M.format := Mode.ColorDepth shl 8;
  M.refresh_rate := 0;
  SDL_SetWindowDisplayMode(FWindow, M);
end;

procedure TWindow.GetDisplayMode(out Mode: TDisplayMode);
var
  M: TSDL_DisplayMode;
begin
  if SDL_GetWindowDisplayMode(FWindow, M) = 0 then
  begin
    Mode.Height := M.h;
    Mode.Width := M.w;
    Mode.ColorDepth := SDL_BITSPERPIXEL(M.format);
    Mode.RefreshRate := M.refresh_rate;
  end
  else
    FillChar(Mode, SizeOf(Mode), 0);
end;

function TWindow.GetDimension(Index: Integer): Integer;
var
  R: TRectI;
begin
  GetBounds(R);
  case Index of
    1: Result := R.Y;
    2: Result := R.Width;
    3: Result := R.Height;
  else
    Result := R.X;
  end;
end;

procedure TWindow.SetDimension(Index, Value: Integer);
var
  R: TRectI;
begin
  GetBounds(R);
  case Index of
    1: R.Y := Value;
    2: R.Width := Value;
    3: R.Height := Value;
  else
    R.X := Value;
  end;
  if Index < 2 then
    Move(R.X, R.Y)
  else
    Resize(R.Width, R.Height);
end;

function TWindow.GetCaption: string;
begin
  Result := SDL_GetWindowTitle(FWindow);
end;

procedure TWindow.SetCaption(const Value: string);
var
  S: string;
begin
  S := Value;
  SDL_SetWindowTitle(FWindow, PAnsiChar(S));
end;

procedure TWindow.SetFullscreen(Value: Boolean);
begin
  if Value <> FFullscreen then
  begin
    FFullscreen := Value;
    SDL_ShowWindow(FWindow);
    SDL_SetWindowFullscreen(FWindow, FFullscreen);
  end;
end;

function TWindow.GetScreen: TScreen;
var
  I: Integer;
begin
  I := SDL_GetWindowDisplayIndex(FWindow);
  if I < 0 then
    Exit(nil);
  if I > Screens.Count then
    Screens.ScanHardware;
  Result := Screens[I];
end;

procedure TWindow.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
    if Visible then
      Show
    else
      Hide;
end;

{ TWindows }

procedure TWindows.Delete(Index: Integer);
begin
  Items[Index].Free;
  Items.Delete(Index);
end;

function TWindows.GetWindow(Index: Integer): TWindow;
begin
  Result := Items[Index];
end;

function OpenGLLoad: Boolean;
begin
  { Just return true because we actually need a context for extension
    querying to work. See TApplication.Init }
  Result := True;
end;

function OpenGLGetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  Result := SDL_GL_GetProcAddress(ProcName);
end;

function OpenGLExtensionSupported(Extension: PAnsiChar): Boolean;
begin
  Result := SDL_GL_ExtensionSupported(Extension);
end;

constructor TApplication.Create;
begin
  inherited Create;
  FWindows := TWindows.Create;
end;

destructor TApplication.Destroy;
begin
  FWindows.Free;
  inherited Destroy;
end;

function TApplication.Init(Strict: Boolean = False): Boolean;
var
  W: PSDL_Window;
  C: PSDL_GLContext;
begin
  Result := @OpenGLManager.Load <> nil;
  if Result then
    Exit;
  if SDL_GL_LoadLibrary(nil) <> 0 then
    Exit;
  { We need an OpenGL context for OpenGLExtensionSupported to work correctly }
  SDL_GL_SetAttribute(SDL_GL_ACCELERATED_VISUAL, 1);
  SDL_ClearError;
  W := SDL_CreateWindow('Hello', 0, 0, 100, 100, SDL_WINDOW_OPENGL or SDL_WINDOW_HIDDEN);
  if W <> nil then
  try
    C := SDL_GL_CreateContext(W);
    if C <> nil then
    try
      SDL_GL_MakeCurrent(W, C);
      SDL_GL_LoadLibrary(nil);
      @OpenGLManager.Load := @OpenGLLoad;
      @OpenGLManager.GetProcAddress := @OpenGLGetProcAddress;
      @OpenGLManager.ExtensionSupported := @OpenGLExtensionSupported;
      Result := OpenGLInit or (not Strict);
   finally
      SDL_GL_MakeCurrent(W, nil);
      SDL_GL_DeleteContext(C);
    end;
  finally
    SDL_DestroyWindow(W);
  end;
  if not Result then
  begin
    @OpenGLManager.Load := nil;
    @OpenGLManager.GetProcAddress :=  nil;
    @OpenGLManager.ExtensionSupported := nil;
  end;
end;

function TApplication.CreateWindow(WindowClass: TWindowClass): TWindow;
begin
  if not Init then
    Exit(nil);
  Result := WindowClass.Create;
  FWindows.Items.Add(Result);
end;

procedure TApplication.ProcessEvents;

  function FindWindow(var Event: TSDL_Event): TWindow;
  var
    W: PSDL_Window;
  begin
    Result := nil;
    case Event.type_ of
      SDL_WINDOW_EVENT..SDL_MOUSEWHEEL,
      SDL_USER_EVENT..SDL_LAST_EVENT:
        begin
          W := SDL_GetWindowFromID(Event.window.windowID);
          if W <> nil then
            Result := TWindow(SDL_GetWindowData(W, WindowInstance));
        end;
    end;
  end;

  procedure TranslateMessage(var Event: TSDL_Event; out Message: TMessage);
  begin
    Message.Msg := Event.type_;
    { Message.Time := Event.timestamp; }
    Message.Time := Now - FTimeDelta;
    case Message.Msg of
      SDL_WINDOW_EVENT:
        begin
          Message.Msg := Event.window.event;
          case Message.Msg of
            SDL_WINDOWEVENT_MOVED, SDL_WINDOWEVENT_RESIZED:
              begin
                Message.MoveResizeArgs.X := Event.window.data1;
                Message.MoveResizeArgs.Y := Event.window.data2;
              end;
            SDL_WINDOWEVENT_FOCUS_GAINED, SDL_WINDOWEVENT_FOCUS_LOST:
              Message.FocusArgs.Focused := Message.Msg = SDL_WINDOWEVENT_FOCUS_GAINED;
          end;
        end;
      SDL_KEYDOWN..SDL_KEYUP:
        begin
          Message.KeyboardArgs.Key := TVirtualKeyCode(Event.key.keysym.sym);
          Message.KeyboardArgs.Repeated := Event.key.repeat_ <> 0;
          Message.KeyboardArgs.Shift := KeyModToShiftState(Event.key.keysym.modifiers);
        end;
      SDL_MOUSEMOTION:
        begin
          Message.MouseMoveArgs.X := Event.motion.x;
          Message.MouseMoveArgs.Y := Event.motion.y;
          Message.MouseMoveArgs.XRel := Event.motion.xrel;
          Message.MouseMoveArgs.YRel := Event.motion.yrel;
          Message.MouseMoveArgs.Shift := KeyModToShiftState(SDL_GetModState);
        end;
      SDL_MOUSEBUTTONDOWN..SDL_MOUSEBUTTONUP:
        begin
          Message.MouseButtonArgs.Button := TMouseButton(Event.button.button - 1);
          Message.MouseButtonArgs.X := Event.button.x;
          Message.MouseButtonArgs.Y := Event.button.y;
          Message.MouseButtonArgs.Shift := KeyModToShiftState(SDL_GetModState);
        end;
      SDL_MOUSEWHEEL:
        begin
          Message.MouseWheelArgs.Delta := Event.wheel.y;
          Message.MouseWheelArgs.Shift := KeyModToShiftState(SDL_GetModState);
        end;
      SDL_JOYAXISMOTION:
        begin
          Message.JoystickAxisArgs.JoystickIndex := Event.jaxis.which;
          Message.JoystickAxisArgs.AxisIndex := Event.jaxis.axis;
          Message.JoystickAxisArgs.Position := Event.jaxis.value;
        end;
      SDL_JOYBALLMOTION:
        begin
          Message.JoystickBallArgs.JoystickIndex := Event.jball.which;
          Message.JoystickBallArgs.BallIndex := Event.jball.ball;
          Message.JoystickBallArgs.XRel := Event.jball.xrel;
          Message.JoystickBallArgs.YRel := Event.jball.yrel;
        end;
      SDL_JOYHATMOTION:
        begin
          Message.JoystickHatArgs.JoystickIndex := Event.jhat.which;
          Message.JoystickHatArgs.HatIndex := Event.jhat.hat;
          case Event.jhat.value of
            SDL_HAT_UP: Message.JoystickHatArgs.Hat := hatUp;
            SDL_HAT_RIGHT: Message.JoystickHatArgs.Hat := hatRight;
            SDL_HAT_DOWN: Message.JoystickHatArgs.Hat := hatDown;
            SDL_HAT_LEFT: Message.JoystickHatArgs.Hat := hatLeft;
            SDL_HAT_RIGHTUP: Message.JoystickHatArgs.Hat := hatRightUp;
            SDL_HAT_RIGHTDOWN: Message.JoystickHatArgs.Hat := hatRightDown;
            SDL_HAT_LEFTUP: Message.JoystickHatArgs.Hat := hatLeftUp;
            SDL_HAT_LEFTDOWN: Message.JoystickHatArgs.Hat := hatLeftDown;
          else
            Message.JoystickHatArgs.Hat := hatCenter;
          end;
        end;
      SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP:
        begin
          Message.JoystickButtonArgs.JoystickIndex := Event.jbutton.which;
          Message.JoystickButtonArgs.ButtonIndex := Event.jbutton.button;
          Message.JoystickButtonArgs.Pressed := Event.jbutton.state = SDL_PRESSED;
        end;
      SDL_JOYDEVICEADDED, SDL_JOYDEVICEREMOVED:
        begin
          Message.JoystickDeviceArgs.JoystickIndex := Event.jdevice.which;
          Message.JoystickDeviceArgs.Added := Message.Msg = SDL_JOYDEVICEADDED;
        end;
    end;
  end;

var
  Event: TSDL_Event;
  Message: TMessage;
  Window: TWindow;
  I: Integer;
begin
  {TODO: Consider using SDL_WaitEvent }
  while SDL_PollEvent(Event) <> 0 do
  begin
    TranslateMessage(Event, Message);
    Window := FindWindow(Event);
    if Window <> nil then
      Window.DispatchMessage(Message)
    else for I := FWindows.Count - 1 downto 0 do
      FWindows[I].DispatchMessage(Message);
  end;
end;

procedure TApplication.Run;

  procedure HandleException(E: Exception);
  var
    Args: TExceptionArgs;
  begin
    if E is EAbortError then
      Exit;
    Args.ExceptObject := E;;
    Args.Handled := False;
    if Assigned(FOnException) then
    try
      FOnException(Self, Args);
    except
      Args.Handled := False;
    end;
    if not Args.Handled then
    begin
      ShowError(E, E.Message);
      Terminate;
    end;
  end;

var
  Stopwatch: TStopwatch;
  Window: TWindow;
  I: Integer;
begin
  if not Init then
    Exit;
  FTimeDelta := Now;
  Stopwatch := TStopwatch.Create;
  for I := FWindows.Count - 1 downto 0 do
    FWindows[I].CreateLoop(Stopwatch);
  while not Terminated do
  begin
    try
      ProcessEvents;
      { Remove windows which have been closed }
      for I := FWindows.Count - 1 downto 0 do
        if FWindows[I].FClosed then
        begin
          FWindows[I].DestroyLoop;
          FWindows.Delete(I);
        end;
      { Terminate when all windows have been closed }
      if FWindows.Count = 0 then
        FTerminated := True;
      if not FTerminated then
      begin
        Stopwatch.Step;
        for I := FWindows.Count - 1 downto 0 do
        begin
          Window := FWindows[I];
          Window.RunLoop(Stopwatch);
        end;
      end;
    except
      on E: Exception do HandleException(E);
    else
      raise;
    end;
    {TODO: To keep things cool, let's try this out for a little bit}
    Sleep(5);
  end;
  Stopwatch.Free;
  for I := FWindows.Count - 1 downto 0 do
    FWindows[I].DestroyLoop;
end;

procedure TApplication.Run(WindowClass: TWindowClass);
var
  Window: TWindow;
begin
  if not Init then
    Exit;
  Window := WindowClass.Create;
  FWindows.Items.Add(Window);
  Run;
end;

procedure TApplication.Run(WindowClass: TWindowClass; out Window: TWindow);
begin
  if not Init then
    Exit;
  Window := WindowClass.Create;
  FWindows.Items.Add(Window);
  Run;
end;

procedure TApplication.Terminate;
begin
  FTerminated := True;
end;

function TApplication.GetKeyboardWindow: TWindow;
var
  W: PSDL_Window;
begin
  W := SDL_GetKeyboardFocus;
  if W <> nil then
    Result := TWindow(SDL_GetWindowData(W, WindowInstance))
  else
    Result := nil;
end;

function TApplication.GetMouseWindow: TWindow;
var
  W: PSDL_Window;
begin
  W := SDL_GetMouseFocus;
  if W <> nil then
    Result := TWindow(SDL_GetWindowData(W, WindowInstance))
  else
    Result := nil;
end;

function TApplication.GetMainWindow: TWindow;
begin
  if FWindows.Count > 0 then
    Result := FWindows[0]
  else
    Result := nil;
end;

initialization
  {$if defined(cpui386) or defined(cpux86_64)}
  { Don't signal floating point errors }
  Set8087CW($133F);
  {$endif}
  SDL_Init(0);
finalization
  JoysticksInstance.Free;
  ApplicationInstance.Free;
  { SDL_Quit here to allow audio system to close gracefully }
  SDL_Quit;
  { Then release the rest of the global singletons }
  AudioInstance.Free;
  KeyboardInstance.Free;
  ScreensInstance.Free;
end.


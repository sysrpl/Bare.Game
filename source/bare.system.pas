(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.system.txt> }
unit Bare.System;

{$i bare.inc}

interface

{TODO: Integrate a unit testing framework at the recommendation of others}

{$region types}
{doc off}
type
  Float = Single;
  PFloat = ^Float;
  LargeInt = Int64;
  PLargeInt = ^LargeInt;
  LargeWord = QWord;
  PLargeWord = ^LargeWord;
  TArray<T> = array of T;
  StringArray = TArray<string>;
  IntArray = TArray<Integer>;
  HModule = Pointer;
  HFile = Pointer;

const
  HiByte = High(Byte);
  InvalidHandle = nil;
{doc on}
{$endregion}

{$region events}
type
  { Arguments for an empty event }
  TEmptyArgs = record end;
  { TEventHandler<T> is a generic event prototype }
  TEventHandler<T> = procedure(Sender: TObject; var Args: T) of object;
  { TEmptyEvent is for events which take no arguments }
  TEmptyEvent = TEventHandler<TEmptyArgs>;

var
  { EmptyArgs provides a blank argument for <link Bare.System.TEmptyEvent, TEmptyEvent> type events }
  EmptyArgs: TEmptyArgs;
{$endregion}

{$region runtime library}
{ The LoadLibrary function loads a dynamic library or shared object file
  See also
  <link Bare.System.FreeLibrary, FreeLibrary procedure>
  <link Bare.System.GetProcAddress, GetProcAddress function> }
function LoadLibrary(const FileName: string): HModule;
{ The FreeLibrary procedure unloads a dynamic library or shared object from memory
  See also
  <link Bare.System.LoadLibrary, LoadLibrary function>
  <link Bare.System.GetProcAddress, GetProcAddress function> }
procedure FreeLibrary(Module: HModule);
{ The GetProcAddress function returns the address of a function in a loaded dynamic
  library or shared object
  See also
  <link Bare.System.LoadLibrary, LoadLibrary function>
  <link Bare.System.FreeLibrary, FreeLibrary procedure> }
function GetProcAddress(Module: HModule; const ProcName: string): Pointer;
{ The Sleep procedure suspends the current thread a for a designated number of
  milliseconds
  See also
  <link Bare.System.Now, Now function> }
procedure Sleep(Milliseconds: LongWord);
{ The Now function returns the time in seconds since application startup
  See also
  <link Bare.System.Sleep, Sleep procedure> }
function Now: Double;
{ Compare two blocks of memory to see if they contain the same data }
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
{ Display a message in a popup window }
procedure ShowMessage(const Msg: string);
{ Display an error in a popup window }
procedure ShowError(ExceptObject: TObject; const Msg: string = '');
{ Write text to the console }
procedure WriteLine(const S: string); overload;
{ Write formattted text to the console }
procedure WriteLine(const S: string; Args: array of const); overload;
{$endregion}

{$region math routines}
{ Returns the uppermost value of a number before rounding }
function Ceil(const X: Extended): Integer;
{ Returns the lowermost value of a number before rounding }
function Floor(const X: Extended): Integer;
{ Returns the whole number of times a quotient be divided
  See also
  <link Bare.System.Remainder, Remainder function> }
function Divide(const Quotient, Divisor: Float): Float;
{ Returns the fractional remainder of a divide
  See also
  <link Bare.System.Divide, Divide function> }
function Remainder(const Quotient, Divisor: Float): Float;
{ Returns the maximum of two singles }
function Max(A, B: Single): Single; overload;
{ Returns the maximum of two doubles }
function Max(constref A, B: Double): Double; overload;
{ Returns the minimum of two singles }
function Min(A, B: Single): Single; overload;
{ Returns the minimum of two doubles }
function Min(constref A, B: Double): Double; overload;
{ Tanget trigometric function }
function Tan(const X: Extended): Extended;
{ Combined sine and cosine single trigometric function }
procedure SinCos(const X: Single; out S, C: Single); overload;
{ Combined sine and cosine dobule trigometric function }
procedure SinCos(constref X: Double; out S, C: Double); overload;
{ Clamps a single between the range 0..1 }
function Clamp(Value: Single): Single;
{$endregion}

{$region string routines}
const
  { End of line characters used by various operating systems [group string] }
  LineBreakStyles: array[TTextLineBreakStyle] of string = (#10, #13#10, #13);
  { The character used to begin command line switches [group string] }
  SwitchChar = '-';

{ Convert a string to uppercase [group string] }
function StrUpper(const S: string): string;
{ Convert a string to lowercase [group string] }
function StrLower(const S: string): string;
{ Copies a substring given a start and length [group string] }
function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
{ Copy a memory buffer into a string [group string] }
function StrCopyData(P: Pointer; Len: Integer): string;
{ Inserts a substring into a string at a position [group string] }
function StrInsert(const S, SubStr: string; Position: Integer): string;
{ Compares two strings optionally ignoring case returning -1 if A comes before
  before B, 1 if A comes after b, ord 0 if A and B are equal [group string] }
function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
{ Searches a string for a substring optionally ignoring case [group string] }
function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer; overload;
{ Searches a string for a substring from a start position optionally ignoring case [group string] }
function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
{ Returns the number of a substring matches within a string [group string] }
function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
{ Returns an array of indices of a substring matches within a string [group string] }
function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
{ Replaces every instance of a pattern in a string [group string] }
function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Replaces the first instance of a pattern in a string [group string] }
function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Trims white space from both sides of a string [group string] }
function StrTrim(const S: string): string;
{ Returns true is an all strings in a string array are the same [group string] }
function StrEquals(const S: string; const Strings: array of string): Boolean;
{ Returns the index of a string in a string array or -1 if there is no match [group string] }
function StrIndex(const S: string; const Strings: array of string): Integer;
{ Splits a string into a string array using a separator [group string] }
function StrSplit(const S, Separator: string): StringArray;
{ Returns the first subsection of a string if it were split using a separator [group string] }
function StrFirstOf(const S, Separator: string): string;
{ Returns the second subsection of a string if it were split using a separator [group string] }
function StrSecondOf(const S, Separator: string): string;
{ Returns the last subsection of a string if it were split using a separator [group string] }
function StrLastOf(const S, Separator: string): string;
{ Search a string for a substring optionally ignoring case [group string] }
function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns true if a string begins with a substring while optionally ignoring case [group string] }
function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns true if a string end with a substring while optionally ignoring case [group string] }
function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns a string of a given length filled with one repeating character [group string] }
function StrOf(C: Char; Len: Integer): string;
{ Returns a string made to fit a given length padded on the left with a character [group string] }
function StrPadLeft(const S: string; C: Char; Len: Integer): string;
{ Returns a string made to fit a given length padded on the right with a character [group string] }
function StrPadRight(const S: string; C: Char; Len: Integer): string;
{ Returns true if a string matches to rules of an identifier [group string] }
function StrIsIdent(const S: string): Boolean;
{ Returns true if a string matches to rules of an attribute [group string] }
function StrIsAttr(const S: string): Boolean;
{ Returns the line break style for a block of text [group string] }
function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
{ Converts the line break style of a block of text using the desired style [group string] }
function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string; overload;
{ Converts the line break style of a block of text using the system defined style [group string] }
function StrAdjustLineBreaks(const S: string): string; overload;
{ Returns true if a program has a matching switch
  See also
  <link Bare.System.SwitchIndex, SwitchIndex function>
  <link Bare.System.SwitchValue, SwitchValue function> [group string] }
function SwitchExists(const Switch: string): Boolean;
{ Returns the index if of a program's matching switch or -1 if no match was found
  See also
  <link Bare.System.SwitchExists, SwitchExists function>
  <link Bare.System.SwitchValue, SwitchValue function> [group string] }
function SwitchIndex(const Switch: string): Integer;
{ Returns the value if of a program's switch
  See also
  <link Bare.System.SwitchExists, SwitchExists function>
  <link Bare.System.SwitchIndex, SwitchIndex function> [group string] }
function SwitchValue(const Switch: string): string;
{ Convert an integer to a string [group string] }
function IntToStr(Value: Integer): string;
{ Convert a string to an integer. Can throw an EConvertError exception. [group string] }
function StrToInt(const S: string): Integer;
{ Convert a string an integer. Returns a default value if conversion cannot be done. [group string] }
function StrToIntDef(const S: string; Default: Integer): Integer;
{ Convert a float to a string [group string] }
function FloatToStr(Value: Extended): string; overload;
{ Convert a float to a string with a given number of decimals [group string] }
function FloatToStr(Value: Extended; Decimals: Integer): string; overload;
{ Convert a string to a float. Can throw an EConvertError exception. [group string] }
function StrToFloat(const S: string): Extended;
{ Convert a string a float. Returns a default value if conversion cannot be done. [group string] }
function StrToFloatDef(const S: string; Default: Extended): Extended;
{ Formats a series of argument into a string
  Remarks
  %c to format an object
  %s to format a string
  %d to format a whole number
  %f to format a float
  %.nf to format a float to n decimal places (e.g. %.2f might return 1.23) [group string] }
function Format(const S: string; Args: array of const): string;
{$endregion}

{$region file system routines}
type
  { Mode used when creating a file handle or file stream }
  TFileMode = (
    { Create a new file overwriting any existing contents }
    fmCreate,
    { Open an existing file }
    fmOpen,
    { Open an existing file or create one if none exists }
    fmOpenOrCreate,
    { Create or opens file which automatically performs writes at the end }
    fmAppend);

  { Determines the relationship of offset when seeking a file handle or stream }
  TSeekOrigin = (
    { Seek offset is distance from the start of the file }
    soBegin,
    { Seek offset is from the current file position }
    soCurrent,
    { Seek offset is distance backwards from the end of file }
    soEnd);

{ Returns true if a file exists }
function FileExists(const FileName: string): Boolean;
{ Delete a file }
function FileDelete(const FileName: string): Boolean;
{ Rename a file }
function FileRename(const OldName, NewName: string): Boolean;
{ Given a file name find the size in bytes of a file or -1 if the file is not present }
function FileSize(const FileName: string): LargeInt; overload;
{ Given a file handle find the in bytes of a file or -1 if the file is not present }
function FileSize(F: HFile): LargeInt; overload;
{ Generates a file handle given a TFileMode }
function FileAccess(const FileName: string; Mode: TFileMode): HFile;
{ Create a new file returning a file handle }
function FileCreate(const FileName: string): HFile;
{ Open or create anew file overwriting any existing contents returning a file handle }
function FileOpen(const FileName: string): HFile;
{ Create or opens file which automatically performs writes at the end returning a file handle }
function FileAppend(const FileName: string): HFile;
{ Moves the file cursor an offset distance from the origin returning the
  new position in relation to the start of the file or -1 if there was an error }
function FileSeek(F: HFile; Offset: LargeInt; Origin: TSeekOrigin): LargeInt;
{ Read from a len bytes from file returning the actual bytes read or -1 if there was an error }
function FileRead(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
{ Read len character from a file }
function FileReadString(F: HFile; Len: LargeInt): string;
{ Write len bytes to a file returning actual bytes written }
function FileWrite(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
{ Write a string to a file returning actual bytes written }
function FileWriteString(F: HFile; const S: string): LargeInt;
{ Write a line a file returning actual bytes written }
function FileWriteLine(F: HFile; const S: string): LargeInt;
{ Close a file handle }
function FileClose(var F: HFile): Boolean;
{ Load the entire contents of a file as text }
function FileLoadText(const FileName: string): string;
{ Save text overwritting the entire contents of a file }
procedure FileSaveText(const FileName, Text: string);
{ Append text to a file }
procedure FileAppendText(const FileName, Text: string); overload;
{ Append formatted text to a file }
procedure FileAppendText(const FileName, S: string; Args: array of const); overload;
{ Append a line to a file }
procedure FileAppendLine(const FileName, Text: string); overload;
{ Append a formatted line to a file }
procedure FileAppendLine(const FileName, S: string; Args: array of const); overload;
{ Extract the name portion of a file name }
function FileExtractName(const FileName: string): string;
{ Extract the extension portion of a file name }
function FileExtractExt(const FileName: string): string;
{ Change the extension portion of a file name }
function FileChangeExt(const FileName, Extension: string): string;
{ Extract the path portion of a file name }
function FileExtractPath(const FileName: string): string;
{ Returns true if a directory exists }
function DirExists(const Directory: string): Boolean;
{ Rename a directory return true is successful }
function DirRename(const OldName, NewName: string): Boolean;
{ Delete a directory return true is successful }
function DirDelete(const Directory: string): Boolean;
{ Create a directory return true is successful }
function DirCreate(const Directory: string): Boolean;
{ Retrieve the current directory }
function DirGetCurrent: string;
{ Change the current directory return true is successful }
function DirSetCurrent(const Directory: string): Boolean;
{ Change path delimiter to match system settings }
function PathAdjustDelimiters(const Path: string): string;
{ Include the system defined path delimiter at the end of the path }
function PathIncludeTrailingDelimiter(const Path: string): string;
{ Exclude the system defined path delimiter from the end of the path }
function PathExcludeTrailingDelimiter(const Path: string): string;
{ Combine two path using the system defined path delimiter }
function PathCombine(const A, B: string): string;
{$endregion}

{$region interfaces}
{ IWindow represents a rectangular screen area
  See also
  <link Overview.Bare.System.IWindow, IWindow members> }

type
  IWindow = interface
  ['{75F3BA72-4662-4B67-9CD0-81AC98463708}']
    { Get a rectangle dimension }
    function GetDimension(Index: Integer): Integer;
    { Set a rectangle dimension }
    procedure SetDimension(Index, Value: Integer);
    { X dimension }
    property X: Integer index 0 read GetDimension write SetDimension;
    { Y dimension }
    property Y: Integer index 1 read GetDimension write SetDimension;
    { Width dimension }
    property Width: Integer index 2 read GetDimension write SetDimension;
    { Height dimension }
    property Height: Integer index 3 read GetDimension write SetDimension;
  end;

{ ICloneable represents an object which can create a new copy of T
  See also
  <link Overview.Bare.System.ICloneable, ICloneable members> }

  ICloneable<T> = interface
  ['{2AF4D64F-3CA2-4777-AAAC-0CDC42B8C34A}']
    { Create a new copy of T }
    function Clone: T;
  end;

{ IAssignable represents an object which can be copied to and from other objects
  See also
  <link Overview.Bare.System.IAssignable, IAssignable members> }

  IAssignable = interface
    ['{6B956879-A408-418F-8A3E-AADE8CEDD727}']
    { Copy source to the object }
    function AssignFrom(Source: TObject): Boolean;
    { Copy the object to dest }
    function AssignTo(Dest: TObject): Boolean;
  end;

{ IEnumerator represents an enumerator of T
  See also
  <link Overview.Bare.System.IEnumerator, IEnumerator members> }

  IEnumerator<T> = interface
  ['{E364253A-20A2-4D6D-9C2B-5D7CEF80DB72}']
    { Get the current T }
    function GetCurrent: T;
    { Enumerate forward returning true if there is a new current item }
    function MoveNext: Boolean;
    { The current T }
    property Current: T read GetCurrent;
  end;

{ TPersistsObject is a class which exposes interfaces while ignoring referencing counting
  See also
  <link Overview.Bare.System.TPersistsObject, TPersistsObject members> }

  TPersistsObject = class(TObject, IInterface, IAssignable)
  protected
    { Returns ok if iid is supported placing the new interface in obj }
    function QueryInterface(constref iid: TGuid; out obj): LongInt; apicall;
    { Does nothing as persists object ignores referencing counting }
    function _AddRef: LongInt; apicall;
    { Does nothing as persists object ignores referencing counting }
    function _Release: LongInt; apicall;
     { Try to copy source to the object }
    function AssignFrom(Source: TObject): Boolean; virtual;
    { Try to copy the object to dest }
    function AssignTo(Dest: TObject): Boolean; virtual;
  end;

{ Copy source object to dest object }
function Assign(Source, Dest: TObject): Boolean; overload;
{ Copy source interface to dest interface  }
function Assign(Source, Dest: IInterface): Boolean; overload;
{$endregion}

{$region streams}
{ The type of stream operation, read or write }
type
  TStreamOperation = (soRead, soWrite);

{ Used by <link Bare.TStream.OnStreamBuffer, OnStreamBuffer event> }

  TStreamBufferArgs = record
    { The type of operation, reading or writing }
    Operation: TStreamOperation;
    { Total bytes transfered during a streaming operation }
    Bytes: LargeWord;
    { Setting cancelled to true ends the streaming operation }
    Cancelled: Boolean;
  end;

{ TStreamBufferEvent allow stream consumers to monitor and cancel reads or writes }

  TStreamBufferEvent = TEventHandler<TStreamBufferArgs>;

{ <include docs/bare.system.tstream.txt> }

  TStream = class(TObject)
  private
    FBufferSize: Word;
    FStreamBuffer: TStreamBufferArgs;
    FOnStreamBuffer: TStreamBufferEvent;
    procedure SetBufferSize(Value: Word);
    function GetCancelled: Boolean;
    procedure SetCancelled(Value: Boolean);
  protected
    { Read len bytes from a buffer and return actual bytes read }
    function DoRead(out Buffer; Len: LargeWord): LargeWord; virtual; abstract;
    { Write len bytes to a buffer and return actual bytes written }
    function DoWrite(const Buffer; Len: LargeWord): LargeWord; virtual; abstract;
    function GetSize: LargeWord; virtual; abstract;
    procedure SetSize(const Value: LargeWord); virtual; abstract;
    function GetPosition: LargeWord; virtual; abstract;
    procedure SetPosition(const Value: LargeWord); virtual; abstract;
  public
    constructor Create;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function Copy(Source: TStream; Len: LargeWord): LargeWord;
    { Reads len bytes from the stream returning actual bytes read
      Remarks
      You should check if stream operations are canceled before calling this method }
    function Read(out Buffer; Len: LargeWord): LargeWord;
    function Write(const Buffer; Len: LargeWord): LargeWord;
    function ReadStr(Len: LargeWord): string;
    procedure WriteStr(const S: string);
    procedure Reset; virtual;
    { Move the position by an offset relative to a seek origin }
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; virtual; abstract;
    { The current stream position }
    property Position: LargeWord read GetPosition write SetPosition;
    { The size in bytes of the stream }
    property Size: LargeWord read GetSize write SetSize;
    { A hint to stream consumers suggesting the maximum number of bytes that
      should be used in a streaming operation }
    property BufferSize: Word read FBufferSize write SetBufferSize;
    { When cancelled is true streaming operations are disabled
      Remarks
      An EStreamError will be thrown when attemtting to read or write on a
      cancelled stream }
    property Cancelled: Boolean read GetCancelled write SetCancelled;
    { Invoked when a read or write operation occurs }
    property OnStreamBuffer: TStreamBufferEvent read FOnStreamBuffer write FOnStreamBuffer;
  end;

{ TFileStream is a stream which reads from and writes to files
  See also
  <link Overview.Bare.TFileStream, TFileStream members> }

  TFileStream = class(TStream)
  private
    FFileName: string;
    FFile: HFile;
  protected
    function GetSize: LargeWord; override;
    procedure SetSize(const Value: LargeWord); override;
    function GetPosition: LargeWord; override;
    procedure SetPosition(const Value: LargeWord); override;
    function DoRead(out Buffer; Len: LargeWord): LargeWord; override;
    function DoWrite(const Buffer; Len: LargeWord): LargeWord; override;
  public
    constructor Create(FileName: string; FileMode: TFileMode = fmOpen);
    destructor Destroy; override;
    procedure Reset; override;
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; override;
    { The underlying file handle }
    property Handle: HFile read FFile;
  end;

{ TStringStream is a stream which reads from and writes to a data string
  See also
  <link Overview.Bare.TStringStream, TStringStream members> }

  TStringStream = class(TStream)
  private
    FData: string;
    FPosition: LargeWord;
    procedure SetData(const Value: string);
  protected
    function DoRead(out Buffer; Len: LargeWord): LargeWord; override;
    function DoWrite(const Buffer; Len: LargeWord): LargeWord; override;
    function GetSize: LargeWord; override;
    procedure SetSize(const Value: LargeWord); override;
    function GetPosition: LargeWord; override;
    procedure SetPosition(const Value: LargeWord); override;
  public
    constructor Create(const S: string = '');
    procedure Reset; override;
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; override;
    { The underlying string data }
    property Data: string read FData write SetData;
  end;

{ TMemoryStream is a stream which reads from and writes to memory
  See also
  <link Overview.Bare.TMemoryStream, TMemoryStream members> }

  TMemoryStream = class(TStream)
  private
    FMemory: Pointer;
    FSize: LargeWord;
    FAllocSize: LargeWord;
    FPosition: LargeWord;
  protected
    function DoRead(out Buffer; Len: LargeWord): LargeWord; override;
    function DoWrite(const Buffer; Len: LargeWord): LargeWord; override;
    function GetSize: LargeWord; override;
    procedure SetSize(const Value: LargeWord); override;
    function GetPosition: LargeWord; override;
    procedure SetPosition(const Value: LargeWord); override;
  public
    constructor Create(AllocSize: LargeWord = 0);
    procedure Reset; override;
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; override;
    { The underlying memory }
    property Memory: Pointer read FMemory;
  end;
{$endregion}

{$region exceptions}
{ <include docs/bare.system.exception.txt> }

type
  Exception = class(TObject)
  private
    FMessage: string;
  public
    { Creates an exception }
    constructor Create(const Msg: string); virtual;
    constructor CreateFmt(const Msg: string; Args: array of const); virtual;
    procedure Show;
    property Message: string read FMessage;
  end;

{ ExceptionClass defines a type of exception }

  ExceptionClass = class of Exception;

{doc off}
  EAssertError = class(Exception)
  private
    FFileName: string;
    FLineNumber: Integer;
    procedure Generate(const FileName: string; LineNumber: Integer);
  public
    property FileName: string read FFileName;
    property LineNumber: Integer read FLineNumber;
  end;

  EIOError = class(Exception)
  private
    FCode: Integer;
    procedure Generate(Code: Integer);
  public
    property Code: Integer read FCode;
  end;

  EHeapError = class(Exception)
  public
    AllowFree: Boolean;
    procedure FreeInstance; override;
  end;

  EOutOfMemoryError = class(EHeapError);
  EInvalidPtrError = class(EHeapError);
  EDivByZeroError = class(Exception);
  ERangeError = class(Exception);
  EIntOverflowError = class(Exception);
  EInvalidOpError = class(Exception);
  EZeroDivideError = class(Exception);
  EOverflowError = class(Exception);
  EUnderflowError = class(Exception);
  EObjectCheckError = class(Exception);
  EInvalidCastError = class(Exception);
  EAccessViolationError = class(Exception);
  EBusError = class(Exception);
  EControlBreakError = class(Exception);
  EPrivInstructionError = class(Exception);
  EStackOverflowError = class(Exception);
  EVariantError = class(Exception);
  EExternalError = class(Exception);
  EIntfCastError = class(Exception);
  ESafeCallError = class(Exception);
  EAbstractError = class(Exception);
  EAbortError = class(Exception);
  EQuitSignalError = class(Exception);
  ENoThreadSupportError = class(Exception);
  ENoWideStringSupportError = class(Exception);
  ENotImplementedError = class(Exception);
  EConvertError = class(Exception);
  EStreamError = class(Exception);
  EResNotFoundError = class(Exception);

  ESDLError = class(Exception)
  public
    constructor CreateFunc(const FuncName: string);
  end;
  
  ESynchronizeError = class(Exception);
{doc on}

{ Raises an EAbortError exception causing the current method chain to silently exit }

procedure Abort;
{$endregion}

{$region threading}
{ TThread is the abstract class for executing multithreaded code
  Remarks
  Derive your own TThread class and override the Execute method with your
  logic. You may want to periodically check Terminated in your Execute method
  to exit the thread early.
  See also
  <link Overview.Bare.System.TThread, TThread members>
  <link Bare.Game.TWindow.Multithreaded, TWindow.Multithreaded property>
  <link Bare.System.Lock, Lock function> }

type
  TThread = class
  private
    FHandle: TThreadID;
    FCreateSuspended: Boolean;
    FSemaphore: Pointer;
    { 0 = not started, 1 = started, 2 = done }
    FState: LongInt;
    FTerminated: LongInt;
    FExitCode: LongWord;
    FFreeOnTerminate: Boolean;
    FOnTerminate: TEmptyEvent;
    function GetTerminated: Boolean;
    function GetDone: Boolean;
  protected
    { Override this method to define your own thread logic returning an exit code }
    function Execute: LongWord; virtual; abstract;
    { Returns true when a request to terminate the thread has been made }
    property Terminated: Boolean read GetTerminated;
    { Set this value to true the to automatically destroy the thread after execution
      Remarks
      It is unsafe to use a thread after its execution begins when this value
      is set to true, as the thread can complete and be destroyed at anytime }
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
  public
    constructor Create(Suspended: Boolean = False); overload;
    constructor Create(OnTerminate: TEmptyEvent; Suspended: Boolean = False); overload;
    destructor Destroy; override;
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor(Timeout: LongInt): Boolean;
    { Returns true if the thread has completed its execution }
    property Done: Boolean read GetDone;
    { The thread's exit code }
    property ExitCode: LongWord read FExitCode;
  end;

{ Causes a thread to wait to acquire a ownership lock
  Remarks
  It is safe to call for a lock multiple nested times in a thread, but each lock
  must be matched with an unlock
  See also
  <link Bare.System.TryLock, TryLock function>
  <link Bare.System.Unlock, Unlock procedure> }
procedure Lock;
{ Causes a thread to wait a given number of milliseconds for a ownership lock
  See also
  <link Bare.System.Lock, Lock function>
  <link Bare.System.Unlock, Unlock procedure> }
function TryLock(Timeout: LongWord = 0): Boolean;
{ Releases ownership of a lock allowing the next waiting thread to continue
  See also
  <link Bare.System.Lock, Lock function>
  <link Bare.System.TryLock, TryLock procedure> }
procedure Unlock;
{$endregion}

implementation

uses
  {$ifdef unix}
  cthreads,
  Bare.Interop.Posix,
  {$endif}
  {$ifdef windows}
  Bare.Interop.Windows,
  {$endif}
  Bare.Interop.SDL2,
  Bare.Constants;

{$region runtime library}
function LoadLibrary(const FileName: string): HModule;
begin
  Result := SDL_LoadObject(PChar(FileName));
end;

procedure FreeLibrary(Module: HModule);
begin
  SDL_UnloadObject(Module);
end;

function GetProcAddress(Module: HModule; const ProcName: string): Pointer;
begin
  Result := SDL_LoadFunction(Module, PChar(ProcName));
end;

procedure Sleep(Milliseconds: LongWord);
begin
  SDL_Delay(Milliseconds);
end;

function Now: Double;
const
  TimeFrequency: Uint64 = 0;
  TimeStart: Double = 0;
begin
  if TimeFrequency = 0 then
  begin
    TimeFrequency := SDL_GetPerformanceFrequency;
    TimeStart := SDL_GetPerformanceCounter / TimeFrequency;
  end;
  Result := SDL_GetPerformanceCounter / TimeFrequency - TimeStart;
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
var
  A: PChar absolute P1;
  B: PChar absolute P2;
begin
  while Length > 0 do
  begin
    Dec(Length);
    if A[Length] <> B[Length] then
      Exit(False);
  end;
  Result := True;
end;

procedure ShowMessage(const Msg: string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_INFORMATION, 'Message', PChar(Msg), nil);
end;

procedure ShowError(ExceptObject: TObject; const Msg: string = '');
var
  S: string;
begin
  if ExceptObject <> nil then
  begin
    S := ExceptObject.ClassName +  ': ';
    if ExceptObject is Exception then
      S := S + (ExceptObject as Exception).Message;
    if Msg <> '' then
      S := S + LineEnding + LineEnding + Msg;
  end
  else
    S := Msg;
  if S = '' then
    S := 'Unknown reason';
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 'Error', PChar(S), nil);
end;

procedure WriteLine(const S: string);
begin
  WriteLn(S);
end;

procedure WriteLine(const S: string; Args: array of const);
begin
  WriteLn(Format(S, Args));
end;

{$endregion}

{$region math routines}
function Ceil(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

function Floor(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;

function Divide(const Quotient, Divisor: Float): Float;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Round(Quotient / Divisor) * Divisor;
end;

function Remainder(const Quotient, Divisor: Float): Float;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Quotient - (Trunc(Quotient) div Trunc(Divisor)) * Divisor;
end;

function Max(A, B: Single): Single;
begin
  if A > B then Result := A else Result := B;
end;

function Max(constref A, B: Double): Double;
begin
  if A > B then Result := A else Result := B;
end;

function Min(A, B: Single): Single;
begin
  if A < B then Result := A else Result := B;
end;

function Min(constref A, B: Double): Double;
begin
  if A < B then Result := A else Result := B;
end;

function Tan(const X: Extended): Extended;
begin
  Result := Sin(X) / Cos(X);
end;

procedure SinCos(const X: Single; out S, C: Single);
begin
  S := Sin(X);
  C := Cos(X);
end;

procedure SinCos(constref X: Double; out S, C: Double);
begin
  S := Sin(X);
  C := Cos(X);
end;

function Clamp(Value: Single): Single;
begin
  if Value < 0 then
    Result := 0
  else if Value > 1 then
    Result := 1
  else
    Result := Value;
end;
{$endregion}

{$region string routines}
function LineBreak: string;
begin
  Result := LineBreakStyles[DefaultTextLineBreakStyle];
end;

function StrUpper(const S: string): string;
begin
  Result := UpCase(S);
end;

function StrLower(const S: string): string;
begin
  Result := LowerCase(S);
end;

function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
var
  C, D: string;
begin
  if IgnoreCase then
  begin
    C := StrUpper(A);
    D := StrUpper(B);
  end
  else
  begin
    C := A;
    D := B;
  end;
  if C < D then
    Result := -1
  else if C > D then
    Result := 1
  else
    Result := 0;
end;

function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
var
  A, B: PChar;
  I: Integer;
begin
  if S = '' then
    Exit('');
  if Start < 1 then
    Exit('');
  I := Length(S);
  if Start > I then
    Exit('');
  if Len = 0 then
    Len := Length(S);
  Dec(Start);
  if Start + Len > I then
    Len := I - Start;
  Setlength(Result, Len);
  A := PChar(S);
  B := PChar(Result);
  Inc(A, Start);
  Move(A^, B^, Len);
end;

function StrCopyData(P: Pointer; Len: Integer): string;
begin
  if Len < 1 then
    Exit('');
  SetLength(Result, Len);
  Move(P^, PChar(Result)^, Len);
end;

function StrInsert(const S, SubStr: string; Position: Integer): string;
begin
  if Position < 1 then
    Position := 1
  else if Position > Length(S) then
    Position := Length(S);
  if Position = 1 then
    Exit(SubStr + S);
  if Position = Length(S) then
    Exit(S + SubStr);
  Result := StrCopy(S, 1, Position - 1) + SubStr + StrCopy(S, Position);
end;

function StrFindBuffer(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if  (SLen = 0) or (SubStrLen = 0) then
    Exit;
  Dec(S);
  Dec(SubStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(SubStr[I]);
    Lookup[B] := SubStrLen - I;
  end;
  Last := SubStr[SubStrLen];
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := S[I];
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if SubStr[K] <> S[J + K] then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function StrFindBufferI(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(S);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(UpCase(SubStr[I]));
    Lookup[B] := SubStrLen - I;
  end;
  Last := UpCase(SubStr[SubStrLen]);
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := UpCase(S[I]);
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if UpCase(SubStr[K]) <> UpCase(S[J + K]) then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function StrTrim(const S: string): string;
const
  WhiteSpace = [#0..' '];
var
  Len, I: Integer;
begin
  Len := Length(S);
  while (Len > 0) and (S[Len] in WhiteSpace) do
   Dec(Len);
  I := 1;
  while ( I <= Len) and (S[I] in WhiteSpace) do
    Inc(I);
  Result := Copy(S, I, 1 + Len - I);
end;

function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  if IgnoreCase then
    Result := StrFindBufferI(PChar(S), PChar(SubStr), Length(S), Length(SubStr))
  else
    Result := StrFindBuffer(PChar(S), PChar(SubStr), Length(S), Length(SubStr));
end;

function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer;
var
  P: PChar;
  I: Integer;
begin
  P := PChar(S);
  I := Length(S);
  if (Start < 1) or (Start > I) then
  begin
    Result := 0;
    Exit;
  end;
  Dec(Start);
  Inc(P, Start);
  Dec(I, Start);
  if IgnoreCase then
    Result := StrFindBufferI(P, PChar(SubStr), I, Length(SubStr))
  else
    Result := StrFindBuffer(P, PChar(SubStr), I, Length(SubStr));
  if Result > 0 then
    Inc(Result, Start);
end;

function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
var
  Start, Index: Integer;
begin
  Result := 0;
  Start := 1;
  repeat
    Index := StrFind(S, SubStr, Start, IgnoreCase);
    if Index > 0 then
    begin
      Inc(Result);
      Start := Index + 1;
    end;
  until Index = 0;
end;

function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
var
  Start, Index: Integer;
begin
  SetLength(Result, StrFindCount(S, SubStr, IgnoreCase));
  Start := 1;
  Index := 0;
  while Index < Length(Result) do
  begin
    Start := StrFind(S, SubStr, Start, IgnoreCase);
    Result[Index] := Start;
    Inc(Start);
    Inc(Index);
  end;
end;

function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  PosIndex: IntArray;
  Diff: Integer;
  I, J, K, L: Integer;
begin
  PosIndex := StrFindIndex(S, OldPattern, IgnoreCase);
  if Length(PosIndex) = 0 then
  begin
    Result := S;
    Exit;
  end;
  Diff := Length(NewPattern) - Length(OldPattern);
  I := Length(S) + Diff * Length(PosIndex);
  SetLength(Result, I);
  I := 0;
  J := 1;
  K := 1;
  while K <= Length(S) do
  begin
    if K = PosIndex[I] then
    begin
      if I < High(PosIndex) then
        Inc(I);
      Inc(K, Length(OldPattern));
      for L := 1 to Length(NewPattern) do
      begin
        Result[J] := NewPattern[L];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := S[K];
      Inc(J);
      Inc(K);
    end;
  end;
end;

function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  I: Integer;
begin
  I := StrFind(S, OldPattern, IgnoreCase);
  if I > 0 then
    Result := Copy(S, 1, I - 1) + NewPattern + Copy(S, I + Length(OldPattern), Length(S))
  else
    Result := S;
end;

function StrEquals(const S: string; const Strings: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Strings) to High(Strings) do
    if S = Strings[I] then
    begin
      Result := True;
      Break;
    end;
end;

function StrIndex(const S: string; const Strings: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Strings) to High(Strings) do
    if S = Strings[I] then
    begin
      Result := I;
      Break;
    end;
end;

function StrSplit(const S, Separator: string): StringArray;
var
  Splits: IntArray;
  Pos: Integer;
  I: Integer;
begin
  Result := nil;
  Splits := nil;
  if Length(S) < 1 then
    Exit;
  if Length(Separator) < 1 then
    Exit;
  if StrFind(S, Separator) < 1 then
  begin
    SetLength(Result, 1);
    Result[0] := S;
    Exit;
  end;
  Splits := StrFindIndex(S, Separator);
  SetLength(Result, Length(Splits) + 1);
  Pos := 1;
  for I := Low(Splits) to High(Splits) do
  begin
    Result[I] := Copy(S, Pos, Splits[I] - Pos);
    Pos := Splits[I] + Length(Separator);
  end;
  Result[Length(Splits)] := Copy(S, Pos, Length(S));
end;

function StrFirstOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    Result := StrCopy(S, 1, I - 1)
  else
    Result := S;
end;

function StrSecondOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    Result := StrCopy(S, I + Length(Separator))
  else
    Result := '';
end;

function StrLastOf(const S, Separator: string): string;
var
  A: StringArray;
begin
  A := StrSplit(S, Separator);
  if Length(A) > 0 then
    Result := A[Length(A) - 1]
  else
    Result := '';
end;

function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrFind(S, SubStr, IgnoreCase) > 0;
end;

function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  C := StrCopy(S, 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  C := StrCopy(S, Length(S) - Length(SubStr) + 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrOf(C: Char; Len: Integer): string;
var
  I: Integer;
begin
  if Len < 1 then
    Exit;
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := C;
end;

function StrPadLeft(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I < 1 then
    Exit;
  if Len < 1 then
    Exit;
  if I > Len then
  begin
    Result := Copy(S, 1, Len);
    Exit;
  end;
  Result := S + StrOf(C, Len - I);
end;

function StrPadRight(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I < 1 then
    Exit;
  if Len < 1 then
    Exit;
  if I > Len then
  begin
    Result := Copy(S, Len - I, Len);
    Exit;
  end;
  Result := StrOf(C, I - Len) + S;
end;

function IsAlpha(C: Char): Boolean;
begin
  Result := (C >= 'A') and (C <= 'Z');
  if Result then Exit;
  Result := (C >= 'a') and (C <= 'z');
end;

function IsUnderscore(C: Char): Boolean;
begin
  Result := C = '_';
end;

function IsNumeric(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function StrIsIdent(const S: string): Boolean;
var
  AlphaFound: Boolean;
  C: Char;
  I: Integer;
begin
  Result := False;
  if Length(S) < 1 then
    Exit;
  C := S[1];
  AlphaFound := IsAlpha(C);
  if (not AlphaFound) and (not IsUnderscore(C)) then
    Exit;
  for I := 2 to Length(S) do
  begin
    C := S[I];
    AlphaFound := AlphaFound or IsAlpha(C);
    if IsAlpha(C) or IsUnderscore(C) or IsNumeric(C) then
      Continue;
    Exit;
  end;
  Result := AlphaFound;
end;

function StrIsAttr(const S: string): Boolean;
begin
  Result := False;
  if Length(S) < 2 then
    Exit;
  if S[1] <> '@' then
    Exit;
  Result := StrIsIdent(Copy(S, 2, Length(S) - 1));
end;

function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
var
  Count: array[TTextLineBreakStyle] of Integer;
  I: TTextLineBreakStyle;
begin
  for I := Low(Count) to High(Count) do
    Count[I] := StrFindCount(S, LineBreakStyles[I]);
  Result := DefaultTextLineBreakStyle;
  for I := Low(Count) to High(Count) do
    if Count[I] > Count[Result] then
      Result := I;
end;

function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
var
  Line: string;
  I, J: Integer;
begin
  if S = '' then
    Exit('');
  I := StrFindCount(S, #10) + StrFindCount(S, #13);
  SetLength(Result, Length(S) + I);
  Line := LineBreakStyles[Style];
  I := 1;
  J := 1;
  while S[I] > #0  do
  begin
    if ((S[I] = #10) and (S[I + 1] = #13)) or ((S[I] = #13) and (S[I + 1] = #10)) then
    begin
      Result[J] := Line[1];
      Inc(J);
      if Length(Line) > 1 then
      begin
        Result[J] := Line[2];
        Inc(J);
      end;
      Inc(I);
    end
    else if (S[I] = #10) or (S[I] = #13) then
    begin
      Result[J] := Line[1];
      Inc(J);
      if Length(Line) > 1 then
      begin
        Result[J] := Line[2];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := S[I];
      Inc(J);
    end;
    Inc(I);
  end;
  SetLength(Result, J - 1);
end;

function StrAdjustLineBreaks(const S: string): string;
begin
  Result := StrAdjustLineBreaks(S, DefaultTextLineBreakStyle);
end;

function SwitchExists(const Switch: string): Boolean;
begin
  Result := SwitchIndex(Switch) > 0;
end;

function SwitchIndex(const Switch: string): Integer;
var
  S: string;
  I: Integer;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S = SwitchChar + Switch then
      Exit(I)
  end;
  Result := -1;
end;

function SwitchValue(const Switch: string): string;
var
  F: Boolean;
  S: string;
  I: Integer;
begin
  F := False;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if F then
      Exit(S);
    if S = SwitchChar + Switch then
      F := True;
  end;
  Result := '';
end;

function IntToStr(Value: Integer): string;
begin
  Str(Value, Result);
end;

function StrToInt(const S: string): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    raise EConvertError.CreateFmt(SConvertError, ['string', 'Integer']);
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    Result := Default;
end;

function FloatToStr(Value: Extended): string;
const
  Epsilon = 0.0001;
var
  E: Extended;
  I: Integer;
begin
  E := Value - Trunc(Value);
  I := 0;
  while E > Epsilon do
  begin
    E := E * 10;
    E := E - Trunc(E);
    Inc(I);
  end;
  Str(Value:0:I, Result);
end;

function FloatToStr(Value: Extended; Decimals: Integer): string;
begin
  Str(Value:0:Decimals, Result);
end;

function StrToFloat(const S: string): Extended;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    raise EConvertError.CreateFmt(SConvertError, ['string', 'Float']);
end;

function StrToFloatDef(const S: string; Default: Extended): Extended;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    Result := Default;
end;

function Format(const S: string; Args: array of const): string;

  function ExtractString(const V: TVarRec): string;
  const
    BoolStr: array[Boolean] of string = ('no', 'yes');
  begin
    case V.VType of
      vtChar: Result := V.VChar;
      vtString: Result := V.VString^;
      vtPChar: Result := V.VPChar;
      {vtWideChar: Result := V.VWideChar;
      vtPWideChar: Result := V.VPWideChar^;}
      vtAnsiString: Result := PChar(V.VAnsiString);
      vtWideString: Result := PWideChar(V.VWideString);
      vtBoolean: Result := BoolStr[V.VBoolean];
    else
      raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractInteger(const V: TVarRec): string;
  begin
    case V.VType of
      vtInteger: Result := IntToStr(V.VInteger);
      vtInt64: Result := IntToStr(V.VInt64^);
      vtQWord: Result := IntToStr(V.VQWord^);
    else
      raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractFloat(const V: TVarRec): string;
  begin
    case V.VType of
      vtInteger: Result := FloatToStr(V.VInteger);
      vtExtended: Result := FloatToStr(V.VExtended^);
      vtCurrency: Result := FloatToStr(V.VCurrency^);
    else
      raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractFloatDecimal(const V: TVarRec; Decimals: Integer): string;
  begin
    case V.VType of
      vtInteger: Result := FloatToStr(V.VInteger, Decimals);
      vtExtended: Result := FloatToStr(V.VExtended^, Decimals);
      vtCurrency: Result := FloatToStr(V.VCurrency^, Decimals);
    else
      raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractObject(const V: TVarRec): string;
  begin
    case V.VType of
      vtObject: Result := V.VObject.ToString;
    else
      raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

var
  Splits: IntArray;
  Item: string;
  Index, I, J: Integer;
  FormatStrings: StringArray;
begin
  Splits := nil;
  Result := S;
  if (S = '') or (StrFind(S, '%') < 1) then
    Exit;
  SetLength(FormatStrings, 13);
  FormatStrings[0] := '%s';
  FormatStrings[1] := '%d';
  FormatStrings[2] := '%f';
  FormatStrings[3] := '%c';
  for I := 4 to High(FormatStrings) do
    FormatStrings[I] := '%.' + IntToStr(I - 2) + 'f';
  Index := 0;
  Splits := StrFindIndex(S, '%');
  for I := Low(Splits) to High(Splits) do
  begin
    Item := StrCopy(S, Splits[I], 2);
    if not StrEquals(Item, ['%s', '%d', '%f']) then
      Item := StrCopy(S, Splits[I], 4);
    J := StrIndex(Item, FormatStrings);
    if J < 0 then
      Continue;
    case J of
      0: Result := StrReplaceOne(Result, Item, ExtractString(Args[Index]));
      1: Result := StrReplaceOne(Result, Item, ExtractInteger(Args[Index]));
      2: Result := StrReplaceOne(Result, Item, ExtractFloat(Args[Index]));
      3..11: Result := StrReplaceOne(Result, Item, ExtractFloatDecimal(Args[Index], J - 2));
    end;
    Inc(Index);
  end;
end;
{$endregion}

{$region file system routines}
function FileExists(const FileName: string): Boolean;
var
  F: PSDL_RWops;
begin
  F := SDL_RWFromFile(PChar(FileName), 'rb');
  Result := F <> nil;
  if Result then
    SDL_RWClose(F);
end;

function FileDelete(const FileName: string): Boolean;
begin
  if FileName = '' then
    Exit(False);
  {$ifdef unix}
  Result := unlink(PChar(FileName)) = 0;
  {$endif}
  {$ifdef windows}
  Result := DeleteFileA(PChar(FileName));
  {$endif}
end;

function FileRename(const OldName, NewName: string): Boolean;
begin
  if (OldName = '') or (NewName = '') then
    Exit(False);
  {$ifdef unix}
  Result := rename(PChar(OldName), PChar(NewName)) = 0;
  {$endif}
  {$ifdef windows}
  Result := MoveFileA(PChar(OldName), PChar(NewName));
  {$endif}
end;

function FileSize(const FileName: string): LargeInt;
var
  F: PSDL_RWops;
begin
  F := SDL_RWFromFile(PChar(FileName), 'rb');
  if F = nil then
    Exit(-1);
  Result := SDL_RWSize(F);
  SDL_RWClose(F);
end;

function FileSize(F: HFile): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if F <> nil then
    Result :=  SDL_RWSize(R)
  else
    Result := -1;
end;

function FileAccess(const FileName: string; Mode: TFileMode): HFile;
begin
  case Mode of
    fmCreate: Result := SDL_RWFromFile(PChar(FileName), 'w+b');
    fmOpen: Result := SDL_RWFromFile(PChar(FileName), 'r+b');
    fmOpenOrCreate:
      begin
        Result := SDL_RWFromFile(PChar(FileName), 'r+b');
        if Result = nil then
          Result := SDL_RWFromFile(PChar(FileName), 'w+b');
      end;
    fmAppend: Result := SDL_RWFromFile(PChar(FileName), 'a+b');
  else
    Result := nil;
  end;
end;

function FileCreate(const FileName: string): HFile;
begin
  Result := SDL_RWFromFile(PChar(FileName), 'w+b');
end;

function FileOpen(const FileName: string): HFile;
begin
  Result := SDL_RWFromFile(PChar(FileName), 'r+b');
end;

function FileAppend(const FileName: string): HFile;
begin
  Result := SDL_RWFromFile(PChar(FileName), 'a+b');
end;

function FileSeek(F: HFile; Offset: LargeInt; Origin: TSeekOrigin): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if F = nil then
    Exit(-1);
  Result := SDL_RWSeek(R, Offset, Ord(Origin));
end;

function FileRead(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if F = nil then
    Exit(0);
  Result := SDL_RWRead(R, Buffer, 1, Len);
end;

function FileReadString(F: HFile; Len: LargeInt): string;
var
  R: PSDL_RWops absolute F;
begin
  if (F = nil) or (Len < 1) then
  begin
    Len := 0;
    Exit('');
  end;
  SetLength(Result, Len);
  Len := SDL_RWRead(R, Pointer(Result), 1, Len);
  if Len < 1 then
    Exit('');
  SetLength(Result, Len);
end;

function FileWrite(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if (F = nil) or (Len < 1) then
    Exit(0);
  Result := SDL_RWWrite(R, Buffer, 1, Len);
end;

function FileWriteString(F: HFile; const S: string): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if (F = nil) or (Length(S) < 1) then
    Exit(0);
  Result := SDL_RWWrite(R, Pointer(S), 1, Length(S));
end;

function FileWriteLine(F: HFile; const S: string): LargeInt;
var
  R: PSDL_RWops absolute F;
  L: string;
begin
  if F = nil then
    Exit(0);
  if Length(S) > 0 then
    Result := SDL_RWWrite(R, Pointer(S), 1, Length(S))
  else
    Result := 0;
  L := LineBreakStyles[DefaultTextLineBreakStyle];
  Result := Result + SDL_RWWrite(R, Pointer(L), 1, Length(L));
end;

function FileClose(var F: HFile): Boolean;
var
  R: PSDL_RWops absolute F;
begin
  if F <> nil then
  begin
    Result := SDL_RWClose(R) = 0;
    F := nil;
  end
  else
    Result := False;
end;

function FileLoadText(const FileName: string): string;
var
  F: HFile;
begin
  F := FileAccess(FileName, fmOpen);
  if F = nil then
    Exit('');
  try
    FileReadString(F, FileSize(F));
  finally
    FileClose(F);
  end;
end;

procedure FileSaveText(const FileName, Text: string);
var
  F: HFile;
begin
  F := FileCreate(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, Text);
  finally
    FileClose(F);
  end;
end;

procedure FileAppendText(const FileName, Text: string);
var
  F: HFile;
begin
  F := FileAppend(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, Text);
  finally
    FileClose(F);
  end;
end;

procedure FileAppendText(const FileName, S: string; Args: array of const);
begin
  FileAppendText(FileName, Format(S, Args));
end;

procedure FileAppendLine(const FileName, Text: string);
var
  F: HFile;
begin
  F := FileAppend(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, Text);
    FileWriteString(F, LineBreakStyles[DefaultTextLineBreakStyle]);
  finally
    FileClose(F);
  end;
end;

procedure FileAppendLine(const FileName, S: string; Args: array of const);
var
  F: HFile;
begin
  F := FileAppend(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, Format(S, Args));
    FileWriteString(F, LineBreakStyles[DefaultTextLineBreakStyle]);
  finally
    FileClose(F);
  end;
end;

function FileExtractName(const FileName: string): string;
begin
  Result := StrLastOf(FileName, DirectorySeparator);
end;

function FileExtractExt(const FileName: string): string;
begin
  Result := StrLastOf(FileName, DirectorySeparator);
  if StrFind(Result, '.') > 0 then
    Result := '.' + StrLastOf(Result, '.')
  else
    Result := '';
end;

function FileChangeExt(const FileName, Extension: string): string;
var
  S: string;
begin
  S := FileExtractExt(FileName);
  if S = '' then
    Result := FileName + Extension
  else
    Result := StrCopy(FileName, 1, Length(FileName) - Length(S)) + Extension;
end;

function FileExtractPath(const FileName: string): string;
var
  S: string;
begin
  S:= StrLastOf(FileName, DirectorySeparator);
  if S = '' then
    Result := ''
  else
    Result := StrCopy(FileName, 1, Length(FileName) - Length(S) - 1);
end;

function DirExists(const Directory: string): Boolean;
var
  S: string;
begin
  if Directory = '' then
    Exit(False);
  Lock;
  try
    S := DirGetCurrent;
    Result := DirSetCurrent(Directory);
    DirSetCurrent(S);
  finally
    Unlock;
  end;
end;

function DirRename(const OldName, NewName: string): Boolean;
begin
  Result := FileRename(OldName, NewName);
end;

function DirDelete(const Directory: string): Boolean;
begin
  if Directory = '' then
    Exit(False);
  {$ifdef unix}
  Result := rmdir(PChar(Directory)) = 0;
  {$endif}
  {$ifdef windows}
  Result := RemoveDirectoryA(PChar(Directory));
  {$endif}
end;

function DirCreate(const Directory: string): Boolean;
begin
  if Directory = '' then
    Exit(False);
  {$ifdef unix}
  Result := mkdir(PChar(Directory), S_IAUSR) = 0;
  {$endif}
  {$ifdef windows}
  Result := CreateDirectoryA(PChar(Directory), nil);
  {$endif}
end;

function DirGetCurrent: string;
var
  Buffer: array[0..$FF] of Char;
begin
  Result := '';
  {$ifdef unix}
  if getcwd(Buffer, SizeOf(Buffer)) <> nil then
    Result := PChar(Buffer);
  {$endif}
  {$ifdef windows}
  if GetCurrentDirectoryA(SizeOf(Buffer), Buffer) > 0 then
    Result := PChar(Buffer);
  {$endif}
end;

function DirSetCurrent(const Directory: string): Boolean;
begin
  if Directory = '' then
    Exit(False);
  {$ifdef unix}
  Result := chdir(PChar(Directory)) = 0;
  {$endif}
  {$ifdef windows}
  Result := SetCurrentDirectoryA(PChar(Directory));
  {$endif}
end;

function PathAdjustDelimiters(const Path: string): string;
begin
  if DirectorySeparator = '/' then
    Result := StrReplace(Path, '\', DirectorySeparator)
  else
    Result := StrReplace(Path, '/', DirectorySeparator);
end;

function PathIncludeTrailingDelimiter(const Path: string): string;
begin
  if StrEndsWith(Path, DirectorySeparator) then
    Result := Path
  else
    Result := Path + DirectorySeparator;
end;

function PathExcludeTrailingDelimiter(const Path: string): string;
begin
  if StrEndsWith(Path, DirectorySeparator) then
    Result := StrCopy(Path, 1, Length(Path) - 1)
  else
    Result := Path;
end;

function PathCombine(const A, B: string): string;
begin
  Result := PathIncludeTrailingDelimiter(A) + B;
end;
{$endregion}

{$region interfaces}
function TPersistsObject.QueryInterface(constref iid: TGuid; out obj): LongInt; apicall;
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := LongInt(E_NOINTERFACE);
end;

function TPersistsObject._AddRef: LongInt; apicall;
begin
  Result := 1;
end;

function TPersistsObject._Release: LongInt; apicall;
begin
  Result := 1;
end;

function TPersistsObject.AssignFrom(Source: TObject): Boolean;
begin
  Result := False;
end;

function TPersistsObject.AssignTo(Dest: TObject): Boolean;
begin
  Result := False;
end;

function Assign(Source, Dest: TObject): Boolean;
begin
  Result := False;
  if (Source = nil) or (Dest = nil) or (Source = Dest) then
    Exit;
  if Dest is IAssignable then
    Result := (Dest as IAssignable).AssignFrom(Source);
  if Result then
    Exit;
  if Source is IAssignable then
    Result := (Source as IAssignable).AssignTo(Dest);
end;

function Assign(Source, Dest: IInterface): Boolean;
begin
  if (Source is TObject) and (Dest is TObject) then
    Result := Assign(Source as TObject, Dest as TObject)
  else
    Result := False;
end;

{$endregion}

{$region streams}
constructor TStream.Create;
const
  { A 4 kilobyte default buffer size }
  DefaultBufferSize = 1024 * 4;
begin
  inherited Create;
  SetBufferSize(DefaultBufferSize);
end;

function TStream.Read(out Buffer; Len: LargeWord): LargeWord;
begin
  if FStreamBuffer.Cancelled then
    raise EStreamError.Create(SStreamCancelledError);
  FStreamBuffer.Operation := soRead;
  if Assigned(FOnStreamBuffer) then
    FOnStreamBuffer(Self, FStreamBuffer);
  if not FStreamBuffer.Cancelled then
    Result := DoRead(Buffer, Len)
  else
    Result := 0;
  Inc(FStreamBuffer.Bytes, Result);
end;

{ Writes len bytes to the stream returning actual bytes written
  Remarks
  You should check if stream operations are canceled before calling this method }

function TStream.Write(const Buffer; Len: LargeWord): LargeWord;
begin
  if Cancelled then
    raise EStreamError.Create(SStreamCancelledError);
  FStreamBuffer.Operation := soWrite;
  if Assigned(FOnStreamBuffer) then
    FOnStreamBuffer(Self, FStreamBuffer);
  if not FStreamBuffer.Cancelled then
    Result := DoWrite(Buffer, Len)
  else
    Result := 0;
  Inc(FStreamBuffer.Bytes, Result);
end;

{ Read a number of characters from the stream
  Remarks
  You should check if stream operations are canceled before calling this method }

function TStream.ReadStr(Len: LargeWord): string;
var
  P, Z: LargeWord;
begin
  P := Position;
  Z := Size;
  if Len = 0 then
    Len := Z;
  if P + Len > Z then
    Len := Z - P;
  SetLength(Result, Len);
  Read(PChar(Result)^, Len);
end;

{ Write a string to the stream
  Remarks
  You should check if stream operations are canceled before calling this method }

procedure TStream.WriteStr(const S: string);
begin
  Write(PChar(S)^, Length(S));
end;

{ Resets the stream setting the size back to zero and sets cancelled to false }

procedure TStream.Reset;
begin
  FStreamBuffer.Bytes := 0;
  FStreamBuffer.Cancelled := False;
end;

procedure TStream.LoadFromFile(const FileName: string);
var
  Source: TStream;
begin
  Source := TFileStream.Create(FileName);
  try
    Copy(Source, 0);
  finally
    Source.Free;
  end;
end;

procedure TStream.SaveToFile(const FileName: string);
var
  Dest: TStream;
begin
  Dest := TFileStream.Create(FileName, fmCreate);
  try
    Dest.Copy(Self, 0);
  finally
    Dest.Free;
  end;
end;

{ Copy to another stream. If count is 0, the entire steam is copied. }

function TStream.Copy(Source: TStream; Len: LargeWord): LargeWord;
var
  BufferSize, I: LargeWord;
  Buffer: PByte;
begin
  if Len = 0 then
  begin
    Seek(0, soBegin);
    Reset;
    Len := Source.Size;
    Size := Len;
    Source.Seek(0, soBegin);
  end;
  Result := Len;
  if Result = 0 then Exit;
  if Len > FBufferSize then
    BufferSize := FBufferSize
  else
    BufferSize := Len;
  GetMem(Buffer, BufferSize);
  try
    while Len > 0 do
    begin
      if Len > BufferSize then
        I := BufferSize
      else
        I := Len;
      if Source.Cancelled or Cancelled then
        Break;
      Source.Read(Buffer^, I);
      if Source.Cancelled then
        Break;
      Write(Buffer^, I);
      Dec(Len, I);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure TStream.SetBufferSize(Value: Word);
begin
  if Value < $100 then
    Value := $100;
  FBufferSize := Value;
end;

function TStream.GetCancelled: Boolean;
begin
  Result := FStreamBuffer.Cancelled;
end;

procedure TStream.SetCancelled(Value: Boolean);
begin
  if Value <> FStreamBuffer.Cancelled then
  begin
    FStreamBuffer.Bytes := 0;
    FStreamBuffer.Cancelled := Value;
  end;
end;

{ Creates a file stream given a file name and mode
  Remarks
  Raises EIOError exception if the file cannot be accessed }

constructor TFileStream.Create(FileName: string; FileMode: TFileMode = fmOpen);
begin
  inherited Create;
  FFileName := FileName;
  FFile := FileAccess(FFileName, FileMode);
  if FFile = InvalidHandle then
    raise EIOError.Create(SIOFileHandleError);
end;

destructor TFileStream.Destroy;
begin
  if FFile <> InvalidHandle then
    FileClose(FFile);
  inherited Destroy;
end;

procedure TFileStream.Reset;
begin
  inherited Reset;
  if FileSize(FFile) > 0 then
  begin
    FileClose(FFile);
    FFile := FileCreate(FFileName);
    if FFile = InvalidHandle then
      raise EIOError.Create(SIOFileHandleError);
  end;
end;

function SignedToUnsigned(I: LargeInt): LargeWord;
begin
  if I < 0 then
    Result := 0
  else
    Result := LargeWord(I);
end;

function TFileStream.DoRead(out Buffer; Len: LargeWord): LargeWord;
begin
  Result := SignedToUnsigned(FileRead(FFile, @Buffer, Len));
end;

function TFileStream.DoWrite(const Buffer; Len: LargeWord): LargeWord;
begin
  Result := SignedToUnsigned(FileWrite(FFile, @Buffer, Len));
end;

function TFileStream.Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord;
begin
  Result := SignedToUnsigned(FileSeek(FFile, Offset, Origin));
end;

function TFileStream.GetSize: LargeWord;
begin
  Result := SignedToUnsigned(FileSize(FFile));
end;

procedure TFileStream.SetSize(const Value: LargeWord);
begin
end;

function TFileStream.GetPosition: LargeWord;
begin
  Result := SignedToUnsigned(FileSeek(FFile, 0, soCurrent));
end;

procedure TFileStream.SetPosition(const Value: LargeWord);
begin
  FileSeek(FFile, LargeInt(Value), soBegin);
end;

{ Creates a string steam with an optional initial value }

constructor TStringStream.Create(const S: string = '');
begin
  inherited Create;
  FData := S;
end;

procedure TStringStream.Reset;
begin
  inherited Reset;
  FData := '';
  FPosition := 0;
end;

function TStringStream.DoRead(out Buffer; Len: LargeWord): LargeWord;
begin
  Result := Length(FData) - FPosition;
  if Result > Len then
    Result := Len;
  if Result < 1 then
    Exit(0);
  Move(PChar(FData)[FPosition], Buffer, Result);
  Inc(FPosition, Result);
end;

function TStringStream.DoWrite(const Buffer; Len: LargeWord): largeWord;
begin
  Result := Len;
  SetSize(FPosition + Len);
  Move(Buffer, PChar(FData)[FPosition], Len);
  Inc(FPosition, Len);
end;

function TStringStream.Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord;
begin
  {TODO: Fix these typical mixing of signed/unsigned types}
  case Origin of
    soBegin: FPosition := SignedToUnsigned(Offset);
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := Length(FData) - Offset;
  end;
  if FPosition > Length(FData) then
    FPosition := Length(FData);
  Result := FPosition;
end;

function TStringStream.GetSize: LargeWord;
begin
  Result := Length(FData);
end;

procedure TStringStream.SetSize(const Value: LargeWord);
begin
  { Streams can only grow. To resize call Reset. }
  if Value > Length(FData) then
    SetLength(FData, Value);
end;

function TStringStream.GetPosition: LargeWord;
begin
  Result := FPosition;
end;

procedure TStringStream.SetPosition(const Value: LargeWord);
begin
  if Value > Length(FData) then
    FPosition := Length(FData)
  else
    FPosition := Value;
end;

procedure TStringStream.SetData(const Value: string);
begin
  FData := Value;
  FPosition := 0;
end;

{ TMemoryStream }

constructor TMemoryStream.Create(AllocSize: LargeWord = 0);
begin
  inherited Create;
  SetSize(AllocSize);
end;

procedure TMemoryStream.Reset;
begin
  inherited Reset;
  if FMemory <> nil then
  begin
    FreeMem(FMemory);
    FMemory := nil;
    FSize := 0;
    FAllocSize := 0;
  end;
end;

function TMemoryStream.DoRead(out Buffer; Len: LargeWord): LargeWord;
begin
  Result := FSize - FPosition;
  if Result > Len then
    Result := Len;
  Move(PChar(FMemory)[FPosition], Buffer, Result);
  Inc(FPosition, Result);
end;

function TMemoryStream.DoWrite(const Buffer; Len: LargeWord): LargeWord;
begin
  Result := Len;
  SetSize(FPosition + Len);
  Move(Buffer, PChar(FMemory)[FPosition], Len);
  Inc(FPosition, Len);
end;

function TMemoryStream.Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord;
begin
  case Origin of
    soBegin: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize - Offset;
  end;
  if FPosition > FSize then
    FPosition := FSize;
  Result := FPosition;
end;

function TMemoryStream.GetSize: LargeWord;
begin
  Result := FSize;
end;

procedure TMemoryStream.SetSize(const Value: LargeWord);
const
  GrowSize = 4096;
begin
  if Value > FSize then
  begin
    FSize := Value;
    if FSize > FAllocSize then
    begin
      FAllocSize := (5 * FSize) div 4;
      {TODO: Review this line}
      FAllocSize := (FAllocSize + (GrowSize - 1)) and not (GrowSize - 1);
      ReallocMem(FMemory, FAllocSize);
    end;
  end;
end;

function TMemoryStream.GetPosition: LargeWord;
begin
  Result := FPosition;
end;

procedure TMemoryStream.SetPosition(const Value: LargeWord);
begin
  if Value > FSize then
    FPosition := FSize
  else
    FPosition := Value;
end;
{$endregion}

{$region exceptions}

constructor Exception.Create(const Msg: string);
begin
  inherited Create;
  FMessage := Msg;
end;

{ Creates an exception using a formatted string }

constructor Exception.CreateFmt(const Msg: string; Args: array of const);
begin
  inherited Create;
  FMessage := Format(Msg, Args);
end;

procedure Exception.Show;
begin
  ShowError(Self);
end;

{ Do not use }

procedure EAssertError.Generate(const FileName: string; LineNumber: Integer);
begin
  FFileName := FileName;
  FLineNumber := LineNumber;
end;

{ Do not use }

procedure EIOError.Generate(Code: Integer);
begin

  FCode := Code;
end;

{ Do not use }

procedure EHeapError.FreeInstance;
begin
  if AllowFree then
    inherited FreeInstance;
end;

constructor ESDLError.CreateFunc(const FuncName: string);
var
  S: string;
begin
  S := SDL_GetError;
  if S = '' then
    S := SUnknownReason;
  inherited CreateFmt(SSDLFunctionFailed, [FuncName, S]);
end;

procedure Abort;
begin
  raise EAbortError.Create(SNoneError) at Pointer(Get_Caller_addr(Get_Frame));
end;

var
  OutOfMemoryError: EHeapError;
  InvalidPointerError: EHeapError;

procedure AbstractErrorProcHandler;
begin
  raise EAbstractError.Create(SAbstractError);
end;

procedure AssertErrorHandler(const Msg, FileName: ShortString; LineNum: LongInt; Address: Pointer);
var
  E: EAssertError;
  S: string;
begin
  if Msg = '' then
    S := SAssertionFailed
  else
    S := Msg;
  E := EAssertError.CreateFmt(SAssertionError, [S, FileName, LineNum]);
  E.Generate(FileName, LineNum);
  raise E at get_caller_addr(Address), get_caller_frame(Address);
end;

procedure ErrorProcHandler(ErrNo: LongInt; Address: Pointer; Frame: Pointer);
var
  E: Exception;
begin
  { This routine is called when the system unit detects an error occurs }
  case ErrNo of
    1: E := OutOfMemoryError;
    200: E := EDivByZeroError.Create(SDivByZero);
    201: E := ERangeError.Create(SRangeError);
    202: E := EStackOverflowError.Create(SStackOverflow);
    203: E := OutOfMemoryError;
    204: E := InvalidPointerError;
    205: E := EOverflowError.Create(SOverflow);
    206: E := EUnderflowError.Create(SUnderflow);
    207: E := EInvalidOpError.Create(SInvalidOp);
    208: E := EDivByZeroError.Create(SZeroDivide);
    210: E := EObjectCheckError.Create(SObjectCheck);
    211: E := EAbstractError.Create(SAbstractError);
    212: E := EExternalError.Create(SExternalException);
    214: E := EBusError.Create(SBusInvalid);
    215: E := EIntOverflowError.Create(SIntOverflow);
    216: E := EAccessViolationError.Create(SAccessViolation);
    217: E := EControlBreakError.Create(SControlBreak);
    218: E := EPrivInstructionError.Create(SPrivInstruction);
    219: E := EInvalidCastError.Create(SInvalidCast);
    220: E := EVariantError.Create(SVarTypeCast);
    221: E := EVariantError.Create(SVarInvalidOp);
    222: E := EVariantError.Create(SVarDispatch);
    223: E := EVariantError.Create(SVarArrayCreate);
    224: E := EVariantError.Create(SVarNotArray);
    225: E := EVariantError.Create(SVarArrayBounds);
    227: E := EAssertError.Create(SAssertionFailed);
    228: E := EIntfCastError.Create(SIntfCast);
    229: E := ESafeCallError.Create(SSafeCallError);
    231: E := EConvertError.Create(SConvertNoneError);
    232: E := ENoThreadSupportError.Create(SNoThreadSupport);
    233: E := EQuitSignalError.Create(SQuitSignal);
    234: E := ENoWideStringSupportError.Create(SNoWideStringSupport);
  else
    E := Exception.CreateFmt(SUnknownError, [ErrNo]);
  end;
  raise E at Address, Frame;
end;

procedure ExceptProcHandler(Obj: TObject; Address: Pointer; FrameCount: LongInt; Frame: PPointer);//[public, alias: 'FPC_BREAK_UNHANDLED_EXCEPTION'];
var
  Addr: PtrUInt absolute Address;
  OutHandle: ^Text;
  S: string;
  I: LongInt;
begin
  { This routine is called when an unhandled error occurs
    i.e. an error that is not stopped by a except block }
  OutHandle := @StdOut;
  WriteLn(OutHandle^, 'An unhandled exception occurred at $', HexStr(Addr, SizeOf(Addr) * 2));
  if Obj is Exception then
  begin
    S := Exception(Obj).ClassName + ' : ' + Exception(Obj).Message;
    Writeln(OutHandle^,S);
  end
  else
    WriteLn(OutHandle^,'Exception object ', Obj.ClassName, ' is not of class Exception');
  WriteLn(OutHandle^, BackTraceStrFunc(Address));
  if FrameCount > 0 then
    for I := 0 to FrameCount - 1 do
      WriteLn(OutHandle^, BackTraceStrFunc(Frame[I]));
  WriteLn(OutHandle^, '');
end;

procedure SafeCallErrorProcHandler(ErrorCode: Integer; Address: Pointer);
begin
  raise ESafeCallError.Create(SSafeCallError) at ErrorAddr;
end;

procedure InitExceptions;
begin
  OutOfMemoryError := EOutOfMemoryError.Create(SOutOfMemory);
  OutOfMemoryError.AllowFree := False;
  InvalidPointerError := EInvalidPtrError.Create(SInvalidPointer);
  InvalidPointerError.AllowFree := False;
  AbstractErrorProc := @AbstractErrorProcHandler;
  AssertErrorProc := @AssertErrorHandler;
  ErrorProc := @ErrorProcHandler;
  ExceptProc := @ExceptProcHandler;
  SafeCallErrorProc := @SafeCallErrorProcHandler;
end;

procedure DoneExceptions;
begin
  OutOfMemoryError.AllowFree := True;
  OutOfMemoryError.Free;
  InvalidPointerError.AllowFree := True;
  InvalidPointerError.Free;
end;
{$endregion}

{$region threading}
var
  ThreadCount: LongInt;

function ThreadRun(Param: Pointer): PtrInt;
var
  Thread: TThread absolute Param;
  ExitCode: LongWord;
begin
  { Track the number of running threads so that DoneThreads waits }
  InterLockedIncrement(ThreadCount);
  SDL_SemWait(Thread.FSemaphore);
  InterLockedIncrement(Thread.FState);
  ExitCode := 0;
  try
    ExitCode := Thread.Execute;
  except
    ExitCode := 1;
  end;
  Thread.Terminate;
  InterLockedExchange(Thread.FExitCode, ExitCode);
  InterLockedIncrement(Thread.FState);
  SDL_SemPost(Thread.FSemaphore);
  if Assigned(Thread.FOnTerminate) then
  try
    Thread.FOnTerminate(Thread, EmptyArgs);
  except
    ExitCode := 1;
  end;
  if Thread.FFreeOnTerminate then
    Thread.Free;
  InterLockedDecrement(ThreadCount);
  Result := ExitCode;
  EndThread(Result);
end;

{ Create a thread with and optionally suspend its execution }

constructor TThread.Create(Suspended: Boolean = False);
begin
  inherited Create;
  FCreateSuspended := True;
  if not Suspended then
    Resume;
end;

constructor TThread.Create(OnTerminate: TEmptyEvent; Suspended: Boolean = False);
begin
  FOnTerminate := OnTerminate;
  Create(Suspended);
end;

destructor TThread.Destroy;
begin
  Terminate;
  WaitFor(0);
  if not FCreateSuspended then
  begin
    CloseThread(FHandle);
    SDL_DestroySemaphore(FSemaphore);
  end;
  inherited Destroy;
end;

{ Resumes execution of a thread }

procedure TThread.Resume;
begin
  if FCreateSuspended then
  begin
    if Terminated then
      Exit;
    FCreateSuspended := False;
    FSemaphore := SDL_CreateSemaphore(0);
    FHandle := BeginThread(@ThreadRun, Self);
    SDL_SemPost(FSemaphore);
    while FState < 1 do
      ThreadSwitch;
  end
  else if FHandle <> 0 then
    ResumeThread(FHandle);
end;

{ Suspends execution of a thread }

procedure TThread.Suspend;
begin
  if FHandle = 0 then
    Exit
  else if Done then
    Exit
  else
    SuspendThread(FHandle);
end;

{ Politely request a thread to exit. It is up to the thread's Execute method to
  check Terminated status and exit early. }

procedure TThread.Terminate;
begin
  InterLockedIncrement(FTerminated);
end;

{ Waits for thread to complete given a timeout in milliseconds
  Remarks
  Pass zero to wait indefinitely. Returns true if the thread was done before the
  timeout expired }

function TThread.WaitFor(Timeout: LongInt): Boolean;
begin
  if FHandle = 0 then
    Result := False
  else if GetCurrentThreadId = FHandle then
    Result := False
  else if Done then
  begin
    WaitForThreadTerminate(FHandle, Timeout);
    Result := True;
  end
  else
  begin
    Resume;
    if Timeout = 0 then
    begin
      WaitForThreadTerminate(FHandle, 0);
      Result := True;
    end
    else
    begin
      Result := SDL_SemWaitTimeout(FSemaphore, Timeout) = 0;
      if Result then
        WaitForThreadTerminate(FHandle, 0);
    end;
  end;
end;

function TThread.GetTerminated: Boolean;
begin
  Result := FTerminated <> 0;
end;

function TThread.GetDone: Boolean;
begin
  Result := FState > 1;
end;

var
  Semaphore: Pointer;

threadvar
  SemaphoreLock: Integer;

procedure Lock;
begin
  Inc(SemaphoreLock);
  if SemaphoreLock = 1 then
    SDL_SemWait(Semaphore);
end;

function TryLock(Timeout: LongWord = 0): Boolean;
begin
  Inc(SemaphoreLock);
  if SemaphoreLock = 1 then
    if Timeout = 0 then
      Result := SDL_SemTryWait(Semaphore) = 0
    else
      Result := SDL_SemWaitTimeout(Semaphore, Timeout) = 0
  else
    Result := True;
end;

procedure Unlock;
begin
  Dec(SemaphoreLock);
  if SemaphoreLock = 0 then
    SDL_SemPost(Semaphore)
  else if SemaphoreLock < 0 then
    raise ESynchronizeError.Create(SSynchronizeError);
end;

function ThreadStack(Param: Pointer): PtrInt;
begin
  Result := 0;
  EndThread(Result);
end;

procedure InitThreads;
var
  Thread: TThreadID;
begin
  { I've been told this is the only way to setup rtl threading safely }
  Thread := BeginThread(ThreadStack);
  WaitForThreadTerminate(Thread, 0);
  CloseThread(Thread);
  Semaphore := SDL_CreateSemaphore(1);
  if Semaphore = nil then
    raise ESynchronizeError.Create(SSynchronizeError);
end;

procedure DoneThreads;
begin
  { Wait for threads to complete }
  while ThreadCount > 0 do
    Sleep(1);
  SDL_DestroySemaphore(Semaphore);
end;
{$endregion}

initialization
  InitExceptions;
  InitThreads;
finalization
  DoneThreads;
  DoneExceptions;
end.

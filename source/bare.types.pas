(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.types.txt> }
unit Bare.Types;

{$i bare.inc}

interface

uses
  Bare.System;

{$region collections}
{doc off}
type
  TArrayEnumerator<T> = class
  private
    FItems: TArray<T>;
    FPosition: Integer;
  public
    constructor Create(const Items: TArray<T>);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;
{doc on}


{ TArrayList\<T\> is a simple extension to dynamic arrays
  See also
  <link Overview.Bare.Types.TArrayList, TArrayList members> }

  TArrayList<T> = record
  private
    function GetEmpty: Boolean;
    function GetFirst: T;
    procedure SetFirst(const Value: T);
    function GetLast: T;
    procedure SetLast(const Value: T);
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetData: Pointer;
     function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    { The array acting as a list }
    Items: TArray<T>;
    { The enumerator type for the list }
    type TEnumerator = TArrayEnumerator<T>;
    { Get the enumerator for the list }
    function GetEnumerator: TEnumerator;
    { Convert a list to an array }
    class operator Implicit(const Value: TArrayList<T>): TArray<T>;
    { Convert an array to a list }
    class operator Implicit(const Value: TArray<T>): TArrayList<T>;
    { Reverses theitems in the list }
    procedure Reverse;
    { Adds and item to the end of the list }
    procedure Add(const Item: T);
    { Appends an array of items to the list }
    procedure AddRange(const Collection: array of T);
    { Removes an item by index from the list and decresaes the count by one }
    procedure Delete(Index: Integer);
    { Set the size of the list to 0 }
    procedure Clear;
    { Returns true if ther are no items in the list }
    property Empty: Boolean read GetEmpty;
    { First item in the list }
    property First: T read GetFirst write SetFirst;
    { Last item in the list }
    property Last: T read GetFirst write SetLast;
    { Number of items in the list }
    property Count: Integer read GetCount write SetCount;
    { Address where to the first item is located }
    property Data: Pointer read GetData;
    { Get or set an item }
    property Item[Index: Integer]: T read GetItem write SetItem; default;
  end;

{ TCollection\<TItem\> it a simple collection of items
  See also
  <link Overview.Bare.Types.TCollection, TCollection members> }

  TCollection<TItem> = class
  public
    { The list type }
    type TItems = TArrayList<TItem>;
  protected
    { The list of items }
    Items: TItems;
    { Return the number of items in the collection }
    function GetCount: Integer;
  public
    { Get the enumerator for the collection }
    function GetEnumerator: TItems.TEnumerator;
    { Number of items in the collection }
    property Count: Integer read GetCount;
  end;

{ TOwnerCollection\<T\> is a simple managed collection of items
  See also
  <link Overview.Bare.Types.TOwnerCollection, TOwnerCollection members> }

  TOwnerCollection<TItem: TObject> = class(TCollection<TItem>)
  public
    destructor Destroy; override;
  end;

{doc off}
  TListEnumerator<TItem> = class
  private
    FItems: TArray<TItem>;
    FCount: Integer;
    FPosition: Integer;
  public
    constructor Create(Items: TArray<TItem>; Count: Integer);
    function GetCurrent: TItem;
    function MoveNext: Boolean;
    property Current: TItem read GetCurrent;
  end;
{doc on}

{ TComparer\<T\> is a function template which returns -1 when A \< B, 1 when A \> B
  and 0 when A = B }

  TComparer<TItem> = function(const A, B: TItem): Integer;

{ Comparer of objects }

function FindObject(const A, B: TObject): Integer;

{ TList\<TItem\> is a generic growable list of items
  See also
  <link Overview.Bare.Types.TList, TList members> }

type
  TList<TItem> = class
  private
    FItems: TArray<TItem>;
    FCount: Integer;
    FCapacity: Integer;
    type PItem = ^TItem;
    procedure CheckBounds(const Method: string; Index: Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetReference(Index: Integer): Pointer;
    function GetItem(Index: Integer): TItem;
    procedure SetItem(Index: Integer; const Value: TItem);
  protected
    { Allows list types to take action on add }
    procedure AddItem(constref Item: TItem); virtual;
    { Allows list types to take action on delete }
    procedure DeleteItem(var Item: TItem); virtual;
    { If require delete is true delete item will be called for every item on clear }
    function RequiresDelete: Boolean; virtual;
    { Search the list for an equal item }
    function Find(Comparer: TComparer<TItem>; const Item: TItem): Integer;
    { Request room for at least a minimum number of items }
    procedure Grow(MinCapacity: Integer);
  public
    { The enumerator type for the list }
    type TEnumerator = TListEnumerator<TItem>;
    { Get the enumerator for the list }
    function GetEnumerator: TEnumerator;
    destructor Destroy; override;
    { Add an item to the end of the list }
    procedure Add(const Item: TItem);
    { Delete an item by index from the list }
    procedure Delete(Index: Integer);
    { Exchange positions of two items in the list }
    procedure Exchange(A, B: Integer);
    { Remove all items from the list setting capcity and count to zero }
    procedure Clear;
    { Reclaim unused capacity }
    procedure Compact;
    { Number of items in the list }
    property Count: Integer read FCount;
    { Allocated space in terms of number of items }
    property Capacity: Integer read GetCapacity write SetCapacity;
    { Get the memory location of an item }
    property Reference[Index: Integer]: Pointer read GetReference;
    { Get or set an item in the list
      Remarks
      When setting the existing item will be deleted }
    property Item[Index: Integer]: TItem read GetItem write SetItem; default;
  end;

{ TIndexedList\<TItem\> allows for search and removing of items by value
  See also
  <link Overview.Bare.Types.TIndexedList, TCollection members> }

  TIndexedList<TItem> = class(TList<TItem>)
  public
    { Returns the index of the item or -1 if it cannot be found }
    function IndexOf(const Item: TItem): Integer; virtual; abstract;
    { Return the item by value }
    function Remove(const Item: TItem): Boolean;
  end;

{ TObjectList\<TItem\> holds objects and can optionally be set to manage their life
  See also
  <link Overview.Bare.Types.TObjectList, TObjectList members> }

  TObjectList<TItem: TObject> = class(TIndexedList<TItem>)
  private
    FOwnsObjects: Boolean;
  protected
    { Frees the items if the list owns the objects }
    procedure DeleteItem(var Item: TItem); override;
    { Returns true if the list owns the objects }
    function RequiresDelete: Boolean; override;
  public
    { Create the lsit optionally owning objects added to it }
    constructor Create(OwnsObjects: Boolean);
    { Returns the index of the object or -1 if it cannot be found }
    function IndexOf(const Item: TItem): Integer; override;
  end;

{TODO: TDictionary<K, V> is pretty old and probably needs to be reworked}


{ TDictionary\<K, V\> holds key value pairs allowing items to be indexed by a key
  See also
  <link Overview.Bare.Types.TDictionary, TDictionary members> }

  TDictionary<K, V> = class
  public
    { TDictionary\<K, V\>.TKeyValue holds a key value pairs
      See also
      <link Overview.Bare.Types.TDictionary.TKeyValue, TDictionary\<K, V\>.TKeyValue members> }

    type
      TKeyValue = class
      private
        FKey: K;
        FValue: V;
      public
        { The key }
        property Key: K read FKey;
        { The value }
        property Value: V read FValue write FValue;
      end;

{doc off}
      TKeyValueEnumerator = class
      private
        FList: TList<TKeyValue>;
        FPosition: Integer;
      public
        constructor Create(List: TList<TKeyValue>);
        function GetCurrent: TKeyValue;
        function MoveNext: Boolean;
        property Current: TKeyValue read GetCurrent;
      end;
{doc on}
  private
    FList: TList<TKeyValue>;
    FComparer: TComparer<K>;
    function KeyEquals(const A, B: K): Boolean;
    function GetCount: Integer;
    function GetKey(Index: Integer): K;
    function GetValue(const Key: K): V;
    procedure SetValue(const Key: K; const Value: V);
  public
    { Create the dictionary }
    constructor Create;
    destructor Destroy; override;
    { Get the enumerator for the list }
    function GetEnumerator: TKeyValueEnumerator;
    { Remove an item by key index }
    procedure Remove(const Key: K);
    { Remove all items from the dictionary }
    procedure Clear;
    { Returns true if the key is in the dictionary }
    function KeyExists(const Key: K): Boolean;
    { Used to compare keys }
    property Comparer: TComparer<K> read FComparer write FComparer;
    { Keys indexed by integer }
    property Keys[Index: Integer]: K read GetKey;
    { Values indexed by key }
    property Values[const Key: K]: V read GetValue write SetValue; default;
    { Number of key value pai }
    property Count: Integer read GetCount;
  end;
{$endregion}

{$region types}
{ TPoint }

  TPoint = record
    X, Y: Integer;
  end;

{ TRect }

  TRect = record
    Left, Top, Right, Bottom: Integer;
  end;

{ TPointI }

  TPointI = record
  public
    X, Y: Integer;
    class operator Implicit(const Value: TPointI): TPoint;
    class operator Implicit(const Value: TPoint): TPointI;
    class operator Negative(const A: TPointI): TPointI;
    class operator Equal(const A, B: TPointI): Boolean;
    class operator NotEqual(const A, B: TPointI): Boolean;
    class operator Add(const A, B: TPointI): TPointI;
    class operator Subtract(const A, B: TPointI): TPointI;
    function Equals(const Value: TPointI): Boolean;
    class function Create: TPointI; overload; static;
    class function Create(X, Y: Integer): TPointI; overload; static;
    function Angle(const P: TPointI): Float;
    function Dist(const P: TPointI): Float;
    function Mid(const P: TPointI): TPointI;
    procedure Offset(X, Y: Integer); overload;
    procedure Offset(const P: TPointI); overload;
    function Move(X, Y: Integer): TPointI; overload;
    function Move(const P: TPointI): TPointI; overload;
  end;
  PPointI = ^TPointI;

{ TRectI }

  TRectI = record
  private
    function GetEmpty: Boolean;
    procedure SetTop(Value: Integer);
    procedure SetLeft(Value: Integer);
    function GetRight: Integer;
    procedure SetRight(Value: Integer);
    function GetBottom: Integer;
    procedure SetBottom(Value: Integer);
    function GetSize: TPointI;
    function GetTopLeft: TPointI;
    function GetBottomRight: TPointI;
    function GetMidPoint: TPointI;
  public
    X, Y, Width, Height: Integer;
    class operator Implicit(const Value: TRectI): TRect;
    class operator Implicit(const Value: TRect): TRectI;
    class operator Equal(const A, B: TRectI): Boolean;
    class operator NotEqual(const A, B: TRectI): Boolean;
    class function Create: TRectI; overload; static;
    class function Create(Size: TPointI): TRectI; overload; static;
    class function Create(W, H: Integer): TRectI; overload; static;
    class function Create(X, Y, W, H: Integer): TRectI; overload; static;
    function Equals(const Value: TRectI): Boolean;
    function Contains(X, Y: Integer): Boolean; overload;
    function Contains(const P: TPointI): Boolean; overload;
    procedure Center(X, Y: Integer); overload;
    procedure Center(const P: TPointI); overload;
    procedure Inflate(X, Y: Integer); overload;
    procedure Inflate(const P: TPointI); overload;
    procedure Offset(X, Y: Integer); overload;
    procedure Offset(const P: TPointI); overload;
    property Empty: Boolean read GetEmpty;
    property Left: Integer read X write SetLeft;
    property Top: Integer read Y write SetTop;
    property Right: Integer read GetRight write SetRight;
    property Bottom: Integer read GetBottom write SetBottom;
    property Size: TPointI read GetSize;
    property TopLeft: TPointI read GetTopLeft;
    property BottomRight: TPointI read GetBottomRight;
    property MidPoint: TPointI read GetMidPoint;
  end;
  PRectI = ^TRectI;

{ TPointF }

  TPointF = record
  public
    X, Y: Float;
    class operator Implicit(const Value: TPointI): TPointF;
    class operator Implicit(const Value: TPoint): TPointF;
    class operator Explicit(const Value: TPointF): TPointI;
    class operator Explicit(const Value: TPointF): TPoint;
    class operator Negative(const A: TPointF): TPointF;
    class operator Equal(const A, B: TPointF): Boolean;
    class operator NotEqual(const A, B: TPointF): Boolean;
    class operator Add(const A, B: TPointF): TPointF;
    class operator Subtract(const A, B: TPointF): TPointF;
    function Equals(const Value: TPointF): Boolean;
    class function Create: TPointF; overload; static;
    class function Create(X, Y: Float): TPointF; overload; static;
    procedure Offset(X, Y: Float); overload;
    procedure Offset(const P: TPointF); overload;
    function Move(X, Y: Float): TPointF; overload;
    function Move(const P: TPointF): TPointF; overload;
    function Angle(const P: TPointF): Float;
    function Dist(const P: TPointF): Float;
    function Mid(const P: TPointF): TPointF;
    function Extend(const P: TPointF; Dist: Float): TPointF;
    function Rotate(const P: TPointF; Angle: Float): TPointF;
  end;
  PPointF = ^TPointF;

{ TRectF }

  TRectF = record
  private
    function GetEmpty: Boolean;
    procedure SetTop(Value: Float);
    procedure SetLeft(Value: Float);
    function GetRight: Float;
    procedure SetRight(Value: Float);
    function GetBottom: Float;
    procedure SetBottom(Value: Float);
    function GetSize: TPointF;
    function GetTopLeft: TPointF;
    function GetTopRight: TPointF;
    function GetBottomLeft: TPointF;
    function GetBottomRight: TPointF;
    function GetMidPoint: TPointF;
    function GetMidLeft: TPointF;
    function GetMidTop: TPointF;
    function GetMidRight: TPointF;
    function GetMidBottom: TPointF;
  public
    X, Y, Width, Height: Float;
    class operator Implicit(const Value: TRectI): TRectF;
    class operator Implicit(const Value: TRect): TRectF;
    class operator Explicit(const Value: TRectF): TRectI;
    class operator Explicit(const Value: TRectF): TRect;
    class operator Equal(const A, B: TRectF): Boolean;
    class operator NotEqual(const A, B: TRectF): Boolean;
    class function Create: TRectF; overload; static;
    class function Create(Size: TPointF): TRectF; overload; static;
    class function Create(W, H: Float): TRectF; overload; static;
    class function Create(X, Y, W, H: Float): TRectF; overload; static;
    function Equals(const Value: TRectF): Boolean;
    function Contains(X, Y: Float): Boolean; overload;
    function Contains(const P: TPointF): Boolean; overload;
    procedure Center(X, Y: Float); overload;
    procedure Center(const P: TPointF); overload;
    procedure Center(const R: TRectF); overload;
    procedure Inflate(X, Y: Float); overload;
    procedure Inflate(const P: TPointF); overload;
    procedure Offset(X, Y: Float); overload;
    procedure Offset(const P: TPointF); overload;
    property Empty: Boolean read GetEmpty;
    property Left: Float read X write SetLeft;
    property Top: Float read Y write SetTop;
    property Right: Float read GetRight write SetRight;
    property Bottom: Float read GetBottom write SetBottom;
    property Size: TPointF read GetSize;
    property TopLeft: TPointF read GetTopLeft;
    property TopRight: TPointF read GetTopRight;
    property BottomLeft: TPointF read GetBottomLeft;
    property BottomRight: TPointF read GetBottomRight;
    property MidPoint: TPointF read GetMidPoint;
    property MidLeft: TPointF read GetMidLeft;
    property MidTop: TPointF read GetMidTop;
    property MidRight: TPointF read GetMidRight;
    property MidBottom: TPointF read GetMidBottom;
  end;
  PRectF = ^TRectF;

{ THSL }

  THSL = record
  public
    Hue, Saturation, Lightness: Float;
    class operator Implicit(Value: Float): THSL;
    class operator Implicit(const Value: THSL): Float;
  end;
  PHSL = ^THSL;

{ TColorB }

  TColorB = packed record
  public
    class operator Implicit(const Value: THSL): TColorB;
    class operator Explicit(Value: TColorB): THSL;
    class function Create(R, G, B: Byte; A: Byte = $FF): TColorB; static;
    function Blend(Value: TColorB; Percent: Float): TColorB;
    function Desaturate(Percent: Float): TColorB;
    function Darken(Percent: Float): TColorB;
    function Lighten(Percent: Float): TColorB;
    function Fade(Percent: Float): TColorB;
  public
    case Integer of
      0: (R, G, B, A: Byte);
      1: (Red, Green, Blue, Alpha: Byte);
  end;
  PColorB = ^TColorB;

  TRGBA = TColorB;
  PRGBA = PColorB;
  TPixel = TColorB;
  PPixel = PColorB;

{ TColorF }

  TColorF = record
  public
    class operator Implicit(const Value: THSL): TColorF;
    class operator Explicit(const Value: TColorF): THSL;
    class operator Implicit(Value: TColorB): TColorF;
    class operator Explicit(const Value: TColorF): TColorB;
    class function Create(B, G, R: Float; A: Byte = 1): TColorF; static;
    function Blend(const Value: TColorF; Percent: Float): TColorF;
    function Desaturate(Percent: Float): TColorF;
    function Fade(Percent: Float): TColorF;
  public
    case Integer of
      0: (R, G, B, A: Float);
      1: (Red, Green, Blue, Alpha: Float);
  end;
  PColorF = ^TColorF;

{ Color routines }

function Hue(H: Float): TColorB;
function Blend(A, B: TColorB; Percent: Float): TColorB;
function Fade(Color: TColorB; Percent: Float): TColorB;
function Darken(Color: TColorB; Percent: Float): TColorB;
function Lighten(Color: TColorB; Percent: Float): TColorB;
{$endregion}

implementation

uses
  Bare.Constants;

{$region collections}
{ TArrayEnumerator<T> }

constructor TArrayEnumerator<T>.Create(const Items: TArray<T>);
begin
  inherited Create;
  FItems := Items;
  FPosition := -1;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FPosition];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < Length(FItems);
end;

{ TArrayList<T> }

function TArrayList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Items);
end;

class operator TArrayList<T>.Implicit(const Value: TArrayList<T>): TArray<T>;
begin
  Result := Value.Items;
end;

class operator TArrayList<T>.Implicit(const Value: TArray<T>): TArrayList<T>;
begin
  Result.Items := Value;
end;

procedure TArrayList<T>.Reverse;
var
  Swap: T;
  I, J: Integer;
begin
  I := 0;
  J := Length(Items);
  while I < J do
  begin
    Swap := Items[I];
    Items[I] := Items[J];
    Items[J] := Swap;
    Inc(I);
    Dec(J);
  end;
end;

procedure TArrayList<T>.Add(const Item: T);
var
  I: Integer;
begin
  I := Length(Items);
  SetLength(Items, I + 1);
  Items[I] := Item;
end;

procedure TArrayList<T>.AddRange(const Collection: array of T);
var
  I, J: Integer;
begin
  I := Length(Items);
  J := High(Collection);
  if J < 1 then
    Exit;
  SetLength(Items, I + J);
  for J := Low(Collection) to High(Collection) do
  begin
    Items[I] := Collection[J];
    Inc(I);
  end;
end;

procedure TArrayList<T>.Delete(Index: Integer);
var
  I, J: Integer;
begin
  I := Length(Items) - 1;
  for J := Index + 1 to I do
    Items[J - 1] := Items[J];
  SetLength(Items, I);
end;

procedure TArrayList<T>.Clear;
begin
  Items := nil;
end;

function TArrayList<T>.GetEmpty: Boolean;
begin
  Result := Length(Items) = 0;
end;

function TArrayList<T>.GetFirst: T;
begin
  Result := Items[0];
end;

procedure TArrayList<T>.SetFirst(const Value: T);
begin
  Items[0] := Value;
end;

function TArrayList<T>.GetLast: T;
begin
  Result := Items[Length(Items) - 1];
end;

procedure TArrayList<T>.SetLast(const Value: T);
begin
  Items[Length(Items) - 1] := Value;
end;

function TArrayList<T>.GetCount: Integer;
begin
  Result := Length(Items);
end;

procedure TArrayList<T>.SetCount(Value: Integer);
begin
  if Value <> Length(Items) then
    SetLength(Items, Value);
end;

function TArrayList<T>.GetData: Pointer;
begin
  Result := @Items[0];
end;

function TArrayList<T>.GetItem(Index: Integer): T;
begin
  Result := Items[Index];
end;

procedure TArrayList<T>.SetItem(Index: Integer; const Value: T);
begin
  Items[Index] := Value;
end;

{ TCollection<TItem> }

function TCollection<TItem>.GetEnumerator: TItems.TEnumerator;
begin
  Result := Items.GetEnumerator;
end;

function TCollection<TItem>.GetCount: Integer;
begin
  Result := Items.Count;
end;

{ TOwnerCollection<T> }

destructor TOwnerCollection<TItem>.Destroy;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Free;
  Items := nil;
  inherited Destroy;
end;

{ TListEnumerator<TItem> }

constructor TListEnumerator<TItem>.Create(Items: TArray<TItem>; Count: Integer);
begin
  inherited Create;
  FItems := Items;
  FCount := Count;
  FPosition := -1;
end;

function TListEnumerator<TItem>.GetCurrent: TItem;
begin
  Result := FItems[FPosition];
end;

function TListEnumerator<TItem>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FCount;
end;

function TList<TItem>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FItems, FCount);
end;

procedure TList<TItem>.AddItem(constref Item: TItem);
begin
  Grow(FCount + 1);
  FItems[FCount] := Item;
  Inc(FCount);
end;

procedure TList<TItem>.DeleteItem(var Item: TItem);
begin
  Item := default(TItem);
end;

function TList<TItem>.RequiresDelete: Boolean;
begin
  Result := False;
end;

function TList<TItem>.Find(Comparer: TComparer<TItem>; const Item: TItem): Integer;
var
  I: Integer;
begin
  for I := Low(FItems) to High(FItems) do
    if Comparer(Item, FItems[I]) = 0 then
      Exit(I);
  Result := -1;
end;

procedure TList<TItem>.Grow(MinCapacity: Integer);
begin
  if MinCapacity > FCapacity then
  begin
    if MinCapacity < 10 then
      MinCapacity := 10;
    FCapacity := MinCapacity + FCapacity div 4;
    SetLength(FItems, FCapacity);
  end;
end;

destructor TList<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TList<TItem>.CheckBounds(const Method: string; Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise ERangeError.CreateFmt(SRangeMethodError, [ClassName, Method]);
end;

procedure TList<TItem>.Clear;
var
  I: Integer;
begin
  if FCount = 0 then
    Exit;
  if RequiresDelete then
    for I := Low(FItems) to High(Fitems) do
      DeleteItem(FItems[I]);
  FCount := 0;
  FCapacity := 0;
  FItems := nil;
end;

procedure TList<TItem>.Compact;
begin
  if FCount = 0 then
    Clear
  else
  begin
    SetLength(FItems, FCount);
    FCapacity := FCount;
  end;
end;

procedure TList<TItem>.Add(const Item: TItem);
begin
  AddItem(Item);
end;

procedure TList<TItem>.Delete(Index: Integer);
var
  I: Integer;
begin
  CheckBounds('Delete', Index);
  DeleteItem(FItems[Index]);
  for I := Index + 1 to High(FItems) do
    FItems[I - 1] := FItems[I];
  if Index <> High(FItems) then
    FItems[High(FItems)] := default(TItem);
end;

procedure TList<TItem>.Exchange(A, B: Integer);
var
  I: TItem;
begin
  CheckBounds('Exchange', A);
  CheckBounds('Exchange', B);
  I := FItems[A];
  FItems[A] := FItems[B];
  FItems[B] := I;
end;

function TList<TItem>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TList<TItem>.SetCapacity(Value: Integer);
begin
  if Value > FCapacity then
    Grow(Value);
end;

function TList<TItem>.GetReference(Index: Integer): Pointer;
begin
  CheckBounds('GetReference', Index);
  Result := @FItems[Index];
end;

function TList<TItem>.GetItem(Index: Integer): TItem;
begin
  CheckBounds('GetItem', Index);
  Result := FItems[Index];
end;

procedure TList<TItem>.SetItem(Index: Integer; const Value: TItem);
begin
  CheckBounds('SetItem', Index);
  DeleteItem(FItems[Index]);
  FItems[Index] := Value;
end;

{ TIndexedList<TItem> }

function TIndexedList<TItem>.Remove(const Item: TItem): Boolean;
var
  I: Integer;
begin
  I := IndexOf(Item);
  Result := (I > -1) and (I < Count);
  if Result then
    Delete(I);
end;

{ TObjectList<TItem> }

constructor TObjectList<TItem>.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
end;

procedure TObjectList<TItem>.DeleteItem(var Item: TItem);
begin
  if FOwnsObjects then
    Item.Free;
  Item := nil;
end;

function TObjectList<TItem>.RequiresDelete: Boolean;
begin
  Result := FOwnsObjects;
end;

function FindObject(const A, B: TObject): Integer;
begin
  Result := PtrInt(A) - PtrInt(B);
end;

function TObjectList<TItem>.IndexOf(const Item: TItem): Integer;
begin
  Result := Find(TComparer<TItem>(@FindObject), Item);
end;

{ TDictionary<K, V>.TKeyValueEnumerator }

constructor TDictionary<K, V>.TKeyValueEnumerator.Create(List: TList<TKeyValue>);
begin
  inherited Create;
  FList := List;
  FPosition := -1;
end;

function TDictionary<K, V>.TKeyValueEnumerator.GetCurrent: TKeyValue;
begin
  Result := TKeyValue(FList[FPosition]);
end;

function TDictionary<K, V>.TKeyValueEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TDictionary<K, V> }

constructor TDictionary<K, V>.Create;
begin
  inherited Create;
  FList := TList<TKeyValue>.Create;
end;

destructor TDictionary<K, V>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDictionary<K, V>.KeyEquals(const A, B: K): Boolean;
begin
  if Assigned(FComparer) then
    Result := FComparer(A, B) = 0
  else
    Result := A = B;
end;

function TDictionary<K, V>.GetEnumerator: TKeyValueEnumerator;
begin
  Result := TKeyValueEnumerator.Create(FList);
end;

procedure TDictionary<K, V>.Remove(const Key: K);
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  I := 0;
  repeat
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
    begin
      KeyValue.Free;
      FList.Delete(I);
      Exit;
    end;
    Inc(I);
  until False;
end;

procedure TDictionary<K, V>.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TObject(FList[I]).Free;
  FList.Clear;
end;

function TDictionary<K, V>.KeyExists(const Key: K): Boolean;
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
      Exit(True);
  end;
  Result := False;
end;

function TDictionary<K, V>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDictionary<K, V>.GetKey(Index: Integer): K;
begin
  Result := TKeyValue(FList[Index]).Key;
end;

function TDictionary<K, V>.GetValue(const Key: K): V;
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  I := 0;
  repeat
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
      Exit(KeyValue.Value);
    Inc(I);
  until False;
end;

procedure TDictionary<K, V>.SetValue(const Key: K; const Value: V);
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
    begin
      KeyValue.Value := Value;
      Exit;
    end;
  end;
  KeyValue := TKeyValue.Create;
  KeyValue.FKey := Key;
  KeyValue.FValue := Value;
  FList.Add(KeyValue);
end;
{$endregion}

{$region types}
{ TPointI }

class operator TPointI.Implicit(const Value: TPointI): TPoint;
var
  R: TPoint absolute Value;
begin
  Result := R;
end;

class operator TPointI.Implicit(const Value: TPoint): TPointI;
var
  R: TPointI absolute Value;
begin
  Result := R;
end;

class operator TPointI.Negative(const A: TPointI): TPointI;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TPointI.Equal(const A, B: TPointI): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TPointI.NotEqual(const A, B: TPointI): Boolean;
begin
  Result := not A.Equals(B);
end;

class operator TPointI.Add(const A, B: TPointI): TPointI;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TPointI.Subtract(const A, B: TPointI): TPointI;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class function TPointI.Create: TPointI;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class function TPointI.Create(X, Y: Integer): TPointI;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointI.Equals(const Value: TPointI): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y);
end;

function TPointI.Angle(const P: TPointI): Float;
begin
  Result := TPointF(Self).Angle(P);
end;

function TPointI.Dist(const P: TPointI): Float;
begin
  Result := TPointF(Self).Dist(P);
end;

function TPointI.Mid(const P: TPointI): TPointI;
begin
  Result := TPointI(TPointF(Self).Mid(P));
end;

procedure TPointI.Offset(X, Y: Integer);
begin
  Inc(Self.X, X);
  Inc(Self.Y, Y);
end;

procedure TPointI.Offset(const P: TPointI);
begin
  Inc(X, P.X);
  Inc(Y, P.Y);
end;

function TPointI.Move(X, Y: Integer): TPointI;
begin
  Result.X := Self.X + X;
  Result.Y := Self.Y + Y;
end;

function TPointI.Move(const P: TPointI): TPointI;
begin
  Result.X := X + P.X;
  Result.Y := Y + P.Y;
end;

{ TRectI }

class operator TRectI.Implicit(const Value: TRectI): TRect;
begin
  Result.Left := Value.X;
  Result.Top := Value.Y;
  Result.Right := Value.X + Value.Width;
  Result.Bottom := Value.Y + Value.Height;
end;

class operator TRectI.Implicit(const Value: TRect): TRectI;
begin
  Result.X := Value.Left;
  Result.Y := Value.Top;
  Result.Width := Value.Right - Value.Left;
  Result.Height := Value.Bottom - Value.Top;
end;

class operator TRectI.Equal(const A, B: TRectI): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TRectI.NotEqual(const A, B: TRectI): Boolean;
begin
  Result := not A.Equals(B);
end;

class function TRectI.Create: TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
end;

class function TRectI.Create(Size: TPointI): TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := Size.X;
  Result.Height := Size.Y;
end;

class function TRectI.Create(W, H: Integer): TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := W;
  Result.Height := H;
end;

class function TRectI.Create(X, Y, W, H: Integer): TRectI;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := W;
  Result.Height := H;
end;

function TRectI.Equals(const Value: TRectI): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y) and
    (Width = Value.Width) and (Width = Value.Width);
end;

function TRectI.Contains(X, Y: Integer): Boolean;
begin
  Result := Contains(TPointI.Create(X, Y));
end;

function TRectI.Contains(const P: TPointI): Boolean;
begin
  if Empty then
    Exit(False);
  Result := (X <= P.X) and (X + Width > P.X) and
    (Y <= P.Y) and (Y + Height > P.Y);
end;

procedure TRectI.Center(X, Y: Integer);
begin
  Self.X := X - Width shr 1;
  Self.Y := Y - Height shr 1;
end;

procedure TRectI.Center(const P: TPointI);
begin
  X := P.X - Width shr 1;
  Y := P.Y - Height shr 1;
end;

procedure TRectI.Inflate(X, Y: Integer);
begin
  Dec(Self.X, X);
  Inc(Width, X shl 1);
  Dec(Self.Y, Y);
  Inc(Height, Y shl 1);
end;

procedure TRectI.Inflate(const P: TPointI);
begin
  Dec(X, P.X);
  Inc(Width, P.X shl 1);
  Dec(Y, P.Y);
  Inc(Height, P.Y shl 1);
end;

procedure TRectI.Offset(X, Y: Integer);
begin
  Inc(Self.X, X);
  Inc(Self.Y, Y);
end;

procedure TRectI.Offset(const P: TPointI);
begin
  Inc(X, P.X);
  Inc(Y, P.Y);
end;

function TRectI.GetEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

procedure TRectI.SetLeft(Value: Integer);
var
  I: Integer;
begin
  I := X + Width;
  X := Value;
  Width := I - X;
end;

procedure TRectI.SetTop(Value: Integer);
var
  I: Integer;
begin
  I := Y + Height;
  Y := Value;
  Height := I - Y;
end;

function TRectI.GetRight: Integer;
begin
  Result := X + Width;
end;

procedure TRectI.SetRight(Value: Integer);
begin
  Width := Value - X;
end;

function TRectI.GetBottom: Integer;
begin
  Result := Y + Height;
end;

procedure TRectI.SetBottom(Value: Integer);
begin
  Height := Value - Y;
end;

function TRectI.GetSize: TPointI;
begin
  Result := TPointI.Create(Width, Height);
end;

function TRectI.GetTopLeft: TPointI;
begin
  Result := TPointI.Create(X, Y);
end;

function TRectI.GetBottomRight: TPointI;
begin
  Result := TPointI.Create(X + Width, Y + Height);
end;

function TRectI.GetMidPoint: TPointI;
begin
  Result := TPointI.Create(X + Width div 2, Y + Height div 2);
end;

{ TPointF }

class operator TPointF.Implicit(const Value: TPointI): TPointF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TPointF.Implicit(const Value: TPoint): TPointF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TPointF.Explicit(const Value: TPointF): TPointI;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
end;

class operator TPointF.Explicit(const Value: TPointF): TPoint;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
end;

class operator TPointF.Negative(const A: TPointF): TPointF;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TPointF.Equal(const A, B: TPointF): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TPointF.NotEqual(const A, B: TPointF): Boolean;
begin
  Result := not A.Equals(B);
end;

class operator TPointF.Add(const A, B: TPointF): TPointF;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TPointF.Subtract(const A, B: TPointF): TPointF;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class function TPointF.Create: TPointF;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class function TPointF.Create(X, Y: Float): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointF.Equals(const Value: TPointF): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y);
end;

procedure TPointF.Offset(X, Y: Float);
begin
  Self.X :=  Self.X + X;
  Self.Y :=  Self.Y + Y;
end;

procedure TPointF.Offset(const P: TPointF);
begin
  X :=  X + P.X;
  Y :=  Y + P.Y;
end;

function TPointF.Move(X, Y: Float): TPointF;
begin
  Result.X := Self.X + X;
  Result.Y := Self.Y + Y;
end;

function TPointF.Move(const P: TPointF): TPointF;
begin
  Result.X := X + P.X;
  Result.Y := Y + P.Y;
end;

function TPointF.Angle(const P: TPointF): Float;
var
  X, Y: Float;
begin
  X := Self.X - P.X;
  Y := Self.Y - P.Y;
  if X = 0 then
    if Y < 0 then
      Exit(Pi)
    else
      Exit(0);
  Result := Arctan(Y / X) + Pi / 2;
  if X > 0 then
    Result := Result + Pi;
end;

function TPointF.Dist(const P: TPointF): Float;
var
  X, Y: Float;
begin
  X := Self.X - P.X;
  Y := Self.Y - P.Y;
  Result := Sqrt(X * X + Y * Y);
end;

function TPointF.Mid(const P: TPointF): TPointF;
begin
  Result.X := (X + P.X) / 2;
  Result.Y := (Y + P.Y) / 2;
end;

function TPointF.Extend(const P: TPointF; Dist: Float): TPointF;
var
  X, Y, R: Float;
begin
  X := Self.X - P.X;
  Y := Self.Y - P.Y;
  R := Sqrt(X * X + Y * Y);
  if R = 0 then
    Exit(Self);
  R := 1 / R;
  Result.X := Self.X - X * R * Dist;
  Result.Y := Self.Y - Y * R * Dist;
end;

function TPointF.Rotate(const P: TPointF; Angle: Float): TPointF;
var
  S, C: Float;
  X, Y: Float;
begin
  if Angle = 0 then
    Exit(P);
  SinCos(Angle, S, C);
  X := Self.Y * S - Self.X * C + Self.X;
  Y := -Self.X * S - Self.Y * C + Self.Y;
  Result.X := P.X * C - P.Y * S + X;
  Result.Y := P.X * S + P.Y * C + Y;
end;

{ TRectF }

class operator TRectF.Implicit(const Value: TRectI): TRectF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
  Result.Width := Value.Width;
  Result.Height := Value.Height;
end;

class operator TRectF.Implicit(const Value: TRect): TRectF;
begin
  Result.X := Value.Left;
  Result.Y := Value.Top;
  Result.Width := Value.Right - Value.Left;
  Result.Height := Value.Bottom - Value.Top;
end;

class operator TRectF.Explicit(const Value: TRectF): TRectI;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
  Result.Width := Round(Value.Width);
  Result.Height := Round(Value.Height);
end;

class operator TRectF.Explicit(const Value: TRectF): TRect;
begin
  Result.Left := Round(Value.X);
  Result.Top := Round(Value.Y);
  Result.Right := Result.Left + Round(Value.Width);
  Result.Bottom := Result.Top + Round(Value.Height);
end;

class operator TRectF.Equal(const A, B: TRectF): Boolean;
begin
  Result := A.Equals(B);
end;

class operator TRectF.NotEqual(const A, B: TRectF): Boolean;
begin
  Result := not A.Equals(B);
end;

class function TRectF.Create: TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
end;

class function TRectF.Create(Size: TPointF): TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := Size.X;
  Result.Height := Size.Y;
end;

class function TRectF.Create(W, H: Float): TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := W;
  Result.Height := H;
end;

class function TRectF.Create(X, Y, W, H: Float): TRectF;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := W;
  Result.Height := H;
end;

function TRectF.Equals(const Value: TRectF): Boolean;
begin
  Result := (X = Value.X) and (Y = Value.Y) and
    (Width = Value.Width) and (Width = Value.Width);
end;

function TRectF.Contains(X, Y: Float): Boolean;
begin
  Result := Contains(TPointF.Create(X, Y));
end;

function TRectF.Contains(const P: TPointF): Boolean;
begin
  if Empty then
    Exit(False);
  Result := (X <= P.X) and (X + Width > P.X) and
    (Y <= P.Y) and (Y + Height > P.Y);
end;

procedure TRectF.Center(X, Y: Float);
begin
  Self.X := X - Width / 2;
  Self.Y := Y - Height / 2;
end;

procedure TRectF.Center(const P: TPointF);
begin
  X := P.X - Width / 2;
  Y := P.Y - Height / 2;
end;

procedure TRectF.Center(const R: TRectF);
begin
  Center(R.MidPoint)
end;

procedure TRectF.Inflate(X, Y: Float);
begin
  Self.X := Self.X - X;
  Self.Y := Self.Y - Y;
  Width := Width + X * 2;
  Height := Height + Y * 2;
end;

procedure TRectF.Inflate(const P: TPointF);
begin
  X := X - P.X;
  Y := Y - P.Y;
  Width := Width + P.X * 2;
  Height := Height + P.Y * 2;
end;

procedure TRectF.Offset(X, Y: Float);
begin
  Self.X := Self.X + X;
  Self.Y := Self.Y + Y;
end;

procedure TRectF.Offset(const P: TPointF);
begin
  X := X + P.X;
  Y := Y + P.Y;
end;

function TRectF.GetEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

procedure TRectF.SetLeft(Value: Float);
var
  I: Float;
begin
  I := X + Width;
  X := Value;
  Width := I - X;
end;

procedure TRectF.SetTop(Value: Float);
var
  I: Float;
begin
  I := Y + Height;
  Y := Value;
  Height := I - Y;
end;

function TRectF.GetRight: Float;
begin
  Result := X + Width;
end;

procedure TRectF.SetRight(Value: Float);
begin
  Width := Value - X;
end;

function TRectF.GetBottom: Float;
begin
  Result := Y + Height;
end;

procedure TRectF.SetBottom(Value: Float);
begin
  Height := Value - Y;
end;

function TRectF.GetSize: TPointF;
begin
  Result := TPointF.Create(Width, Height);
end;

function TRectF.GetTopLeft: TPointF;
begin
  Result := TPointF.Create(X, Y);
end;

function TRectF.GetTopRight: TPointF;
begin
  Result := TPointF.Create(X + Width, Y);
end;

function TRectF.GetBottomLeft: TPointF;
begin
  Result := TPointF.Create(X, Y + Height);
end;

function TRectF.GetBottomRight: TPointF;
begin
  Result := TPointF.Create(X + Width, Y + Height);
end;

function TRectF.GetMidPoint: TPointF;
begin
  Result := TPointF.Create(X + Width / 2, Y + Height / 2);
end;

function TRectF.GetMidLeft: TPointF;
begin
  Result := TPointF.Create(X, Y + Height / 2);
end;

function TRectF.GetMidTop: TPointF;
begin
  Result := TPointF.Create(X + Width / 2, Y);
end;

function TRectF.GetMidRight: TPointF;
begin
  Result := TPointF.Create(X + Width, Y + Height / 2);
end;

function TRectF.GetMidBottom: TPointF;
begin
  Result := TPointF.Create(X + Width / 2, Y + Height);
end;

{ THSL }

class operator THSL.Implicit(Value: Float): THSL;
begin
  Result.Hue := Remainder(Value, 1);
  if Result.Hue < 0 then
    Result.Hue := 1 - Result.Hue;
  Result.Lightness := 0.5;
  Result.Saturation := 1;
end;

class operator THSL.Implicit(const Value: THSL): Float;
begin
  Result := Value.Hue;
end;

{ TColorB }

class operator TColorB.Implicit(const Value: THSL): TColorB;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Float;
  H, S, L, R, G, B: Float;

  function HueToColor(Hue: Float): Float;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      V := M2
    else if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := V;
  end;

begin
  H := Remainder(Value.Hue, 1);
  if H < 0 then
    H := 1 - H;
  S := Clamp(Value.Saturation);
  L := Clamp(Value.Lightness);
  if S = 0 then
  begin
    R := L;
    G := L;
    B := L;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColor(H + OneOverThree);
    G := HueToColor(H);
    B := HueToColor(H - OneOverThree)
  end;
  Result.Red := Round(R * HiByte);
  Result.Green := Round(G * HiByte);
  Result.Blue := Round(B * HiByte);
  Result.Alpha := HiByte;
end;

class operator TColorB.Explicit(Value: TColorB): THSL;
var
  B, R, G, D, CMax, CMin: Single;
begin
  B := Value.Blue / HiByte;
  G := Value.Green / HiByte;
  R := Value.Red / HiByte;
  CMax := Max(R, Max(G, B));
  CMin := Min(R, Min(G, B));
  Result.Lightness := (CMax + CMin) / 2;
  if CMax = CMin then
  begin
    Result.Hue := 0;
    Result.Saturation := 0
  end
  else
  begin
    D := CMax - CMin;
    if Result.Lightness < 0.5 then Result.Saturation := D / (CMax + CMin)
    else Result.Saturation := D / (2 - CMax - CMin);
    if R = CMax then Result.Hue := (G - B) / D
    else
      if G = CMax then Result.Hue  := 2 + (B - R) / D
      else Result.Hue := 4 + (R - G) / D;
    Result.Hue := Result.Hue / 6;
    if Result.Hue < 0 then Result.Hue := Result.Hue + 1
  end;
end;

class function TColorB.Create(R, G, B: Byte; A: Byte = $FF): TColorB;
begin
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;
  Result.Alpha := A;
end;

function MixB(V1: Byte; P1: Float; V2: Byte; P2: Float): Byte; overload;
begin
  Result := Round(V1 * P1 + V2 * P2);
end;

function MixF(V1, P1, V2, P2: Float): Float; overload;
begin
  Result := V1 * P1 + V2 * P2;
end;

function TColorB.Blend(Value: TColorB; Percent: Float): TColorB;
var
  Compliment: Float;
begin
  if Percent = 0 then
    Exit(Self);
  if Percent = 1 then
    Exit(Value);
  Compliment := 1 - Percent;
  Result.Red := MixB(Value.Red, Percent, Red, Compliment);
  Result.Green := MixB(Value.Green, Percent, Green, Compliment);
  Result.Blue := MixB(Value.Blue, Percent, Blue, Compliment);
  Result.Alpha := MixB(Value.Alpha, Percent, Alpha, Compliment);
end;

function TColorB.Desaturate(Percent: Float): TColorB;
var
  Gray: Byte;
  Compliment: Float;
begin
  if Percent = 0 then
    Exit(Self);
  Gray := Round(Red * 0.2126 + Green * 0.7152 + Blue * 0.0722);
  Result.Alpha := Alpha;
  if Percent = 1 then
  begin
    Result.Red := Gray;
    Result.Green := Gray;
    Result.Blue := Gray;
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixB(Gray, Percent, Blue, Compliment);
    Result.Green := MixB(Gray, Percent, Green, Compliment);
    Result.Red := MixB(Gray, Percent, Red, Compliment);
  end;
end;

function TColorB.Darken(Percent: Float): TColorB;
var
  Compliment: Float;
begin
  if Percent = 0 then
    Exit(Self);
  Result.Alpha := Alpha;
  if Percent = 1 then
  begin
    Result.Blue := 0;
    Result.Green := 0;
    Result.Red := 0;
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixB(0, Percent, Blue, Compliment);
    Result.Green := MixB(0, Percent, Green, Compliment);
    Result.Red := MixB(0, Percent, Red, Compliment);
  end;
end;

function TColorB.Lighten(Percent: Float): TColorB;
var
  Compliment: Float;
begin
  if Percent = 0 then
    Exit(Self);
  Result.Alpha := Alpha;
  if Percent = 1 then
  begin
    Result.Blue := HiByte;
    Result.Green := HiByte;
    Result.Red := HiByte;
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixB(HiByte, Percent, Blue, Compliment);
    Result.Green := MixB(HiByte, Percent, Green, Compliment);
    Result.Red := MixB(HiByte, Percent, Red, Compliment);
  end;
end;

function TColorB.Fade(Percent: Float): TColorB;
begin
  Result := Self;
  if Percent = 1 then
    Exit(Self);
  Result.Alpha := Round(Result.Alpha * Percent);
end;

{ TColorF }

function GetColorF(const C: TColorB): TColorF;
begin
  Result.Blue := C.Blue / HiByte;
  Result.Green := C.Green / HiByte;
  Result.Red := C.Red / HiByte;
  Result.Alpha := C.Alpha / HiByte;
end;

function GetColorB(const C: TColorF): TColorB;
begin
  Result.Blue := Round(Clamp(C.Blue) * HiByte);
  Result.Green := Round(Clamp(C.Green) * HiByte);
  Result.Red := Round(Clamp(C.Red) * HiByte);
  Result.Alpha := Round(Clamp(C.Alpha) * HiByte);
end;

class operator TColorF.Implicit(const Value: THSL): TColorF;
begin
  Result := GetColorF(TColorB(Value));
end;

class operator TColorF.Explicit(const Value: TColorF): THSL;
begin
  Result := THSL(GetColorB(Value));
end;

class operator TColorF.Implicit(Value: TColorB): TColorF;
begin
  Result := GetColorF(Value);
end;

class operator TColorF.Explicit(const Value: TColorF): TColorB;
begin
  Result := GetColorB(Value);
end;

class function TColorF.Create(B, G, R: Float; A: Byte = 1): TColorF;
begin
  Result.Blue := B;
  Result.Green := G;
  Result.Red := R;
  Result.Alpha := A;
end;

function TColorF.Blend(const Value: TColorF; Percent: Float): TColorF;
var
  Compliment: Float;
begin
  Percent := Clamp(Percent);
  if Percent < 0.0001 then
    Exit(Self);
  if Percent > 0.9999 then
    Exit(Value);
  Compliment := 1 - Percent;
  Result.Blue := MixF(Blue, Compliment, Value.Blue, Percent);
  Result.Green := MixF(Green, Compliment, Value.Green, Percent);
  Result.Red := MixF(Red, Compliment, Value.Red, Percent);
  Result.Alpha := MixF(Alpha, Compliment, Value.Alpha, Percent);
end;

function TColorF.Desaturate(Percent: Float): TColorF;
var
  Gray: Float;
  Compliment: Float;
begin
  Percent := Clamp(Percent);
  if Percent < 0.0001 then
    Exit(Self);
  Gray := Red * 0.2126 + Green * 0.7152 + Blue * 0.0722;
  Result.Alpha := Alpha;
  if Percent > 0.9999 then
  begin
    Result.Blue := Gray;
    Result.Green := Gray;
    Result.Red := Gray;
  end
  else
  begin
    Compliment := 1 - Percent;
    Result.Blue := MixF(Gray, Percent, Blue, Compliment);
    Result.Green := MixF(Gray, Percent, Green, Compliment);
    Result.Red := MixF(Gray, Percent, Red, Compliment);
  end;
end;

function TColorF.Fade(Percent: Float): TColorF;
begin
  Result := Self;
  Result.Alpha := Clamp(Percent);
end;

{ Color routines }

function Hue(H: Float): TColorB;
begin
  Result := THSL(H);
end;

function Blend(A, B: TColorB; Percent: Float): TColorB;
begin
  Result := A.Blend(B, Percent);
end;

function Fade(Color: TColorB; Percent: Float): TColorB;
begin
  Result := Color.Fade(Percent);
end;

function Darken(Color: TColorB; Percent: Float): TColorB;
begin
  Result := Color.Darken(Clamp(Percent));
end;

function Lighten(Color: TColorB; Percent: Float): TColorB;
begin
  Result := Color.Lighten(Clamp(Percent));
end;
{$endregion}

end.


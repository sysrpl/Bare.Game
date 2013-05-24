(********************************************************)
(*                                                      *)
(*  Bare Game Library                                   *)
(*  http://www.baregame.org                             *)
(*  0.5.0.0 Released under the LGPL license 2013        *)
(*                                                      *)
(********************************************************)

{ <include docs/bare.text.xml.txt> }
unit Bare.Text.Xml;

{$i bare.inc}

interface

{TODO: Convert xml parsers}

type
  TNodeKind = (nkDocument, nkElement, nkAttribute, nkText, nkOther);

  INodeList = interface;
  IDocument = interface;

{ IFiler }

  IFiler = interface
    ['{3DC4CC5C-AFFC-449F-9983-11FE39194CF5}']
    function GetDocument: IDocument;
    function ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
    procedure WriteStr(const Key, Value: string);
    function ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
    procedure WriteBool(const Key: string; Value: Boolean);
    function ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
    procedure WriteInt(const Key: string; Value: Integer);
    function ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
    procedure WriteFloat(const Key: string; Value: Single);
    function ReadDate(const Key: string; const DefValue: TDateTime = 0; Stored: Boolean = False): TDateTime;
    procedure WriteDate(const Key: string; Value: TDateTime);
    property Document: IDocument read GetDocument;
  end;

{ INode }

  INode = interface
    ['{BC90FD97-E83D-41BB-B4D8-3E25AA5EB2C6}']
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetFiler: IFiler;
    function GetAttributes: INodeList;
    function GetNodes: INodeList;
    function GetKind: TNodeKind;
    function GetName: string;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetXml: string;
    procedure SetXml(const Value: string);
    function Instance: Pointer;
    function Next: INode;
    function SelectNode(const XPath: string): INode;
    function SelectList(const XPath: string): INodeList;
    function Force(const Path: string): INode;
    property Document: IDocument read GetDocument;
    property Parent: INode read GetParent;
    property Filer: IFiler read GetFiler;
    property Attributes: INodeList read GetAttributes;
    property Nodes: INodeList read GetNodes;
    property Kind: TNodeKind read GetKind;
    property Name: string read GetName;
    property Text: string read GetText write SetText;
    property Xml: string read GetXml write SetXml;
  end;

{ INodeList }

  INodeList = interface
    ['{D36A2B84-D31D-4134-B878-35E8D33FD067}']
    function GetCount: Integer;
    function GetByName(const Name: string): INode; overload;
    function GetByIndex(Index: Integer): INode; overload;
    procedure Clear;
    procedure Add(Node: INode); overload;
    function Add(const Name: string): INode; overload;
    procedure Remove(Node: INode); overload;
    procedure Remove(const Name: string); overload;
    property Count: Integer read GetCount;
    property ByName[const Name: string]: INode read GetByName;
    property ByIndex[Index: Integer]: INode read GetByIndex; default;
  end;

{ IDocument }

  IDocument = interface(INode)
    ['{B713CB91-C809-440A-83D1-C42BDF806C4A}']
    procedure SetRoot(Value: INode);
    function GetRoot: INode;
    procedure Beautify;
    function CreateAttribute(const Name: string): INode;
    function CreateElement(const Name: string): INode;
    procedure Load(const FileName: string);
    procedure Save(const FileName: string);
    property Root: INode read GetRoot write SetRoot;
  end;

implementation

end.


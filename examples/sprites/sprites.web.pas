unit Sprites.Web;

{$mode delphi}

interface

uses
  Bare.System,
  Bare.Networking.Web;

var
  WebLoad: function(const Url: string): TStream of object;

function WebGet(const Url: string; OnProgress: TWebProgressEvent = nil): TStream;

implementation

var
  WebStream: TStream;

function WebGet(const Url: string; OnProgress: TWebProgressEvent = nil): TStream;
const
  BaseUrl = 'http://download.baregame.org/';
var
  Client: TWebClient;
  S: string;
begin
  WebStream.Free;
  WebStream := TMemoryStream.Create;
  S := PathAdjustDelimiters(Url);
  if FileExists(S) then
    WebStream.LoadFromFile(S)
  else
  begin
    Client := TWebClient.Create;
    try
      Client.OnProgress := OnProgress;
    finally
      Client.Get(BaseUrl + Url, WebStream);
    end;
    DirCreate(FileExtractPath(S));
    WebStream.SaveToFile(S);
  end;
  WebStream.Position := 0;
  Result := WebStream;
end;

initialization
  WebStream := nil;
finalization
  WebStream.Free;
end.


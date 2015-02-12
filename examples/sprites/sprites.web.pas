unit Sprites.Web;

{$mode delphi}

interface

uses
  Bare.System,
  Bare.Networking.Web;

function WebGet(const Url: string): TStream;

implementation

var
  WebStream: TStream;

function WebGet(const Url: string): TStream;
const
  BaseUrl = 'http://download.baregame.org/';
var
  S: string;
begin
	WebStream.Free;
  WebStream := TMemoryStream.Create;
  S := PathAdjustDelimiters(Url);
  if FileExists(S) then
	  WebStream.LoadFromFile(S)
	else
  begin
	  Bare.Networking.Web.WebGet(BaseUrl + Url, WebStream);
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


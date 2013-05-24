program clean;

uses
  SysUtils,
  CrossConfig;

{$apptype console}

procedure ShowUsage;
begin
  WriteLn('usage ', ExtractFileName(ParamStr(0)), ' s|u');
  WriteLn('  s - set configuration');
  WriteLn('  u - unset configuration');
end;

begin
  if ParamCount <> 1 then
    ShowUsage
  else if ParamStr(1) = 's' then
    SetConfigAll(ExpandFileName('.'))
  else if ParamStr(1) = 'u' then
    UnsetConfigAll(ExpandFileName('.'))
  else
    ShowUsage;
end.


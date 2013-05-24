unit BareGameRegister;

{$mode delphi}

interface

uses
  Forms, Dialogs, LazIDEIntf, ProjectIntf, MenuIntf, BareGameDescriptors,
  CrossAbout;

procedure Register;

implementation

procedure ShowAboutBareGame(Sender: TObject);
begin
  with TCrossAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure Register;
var
  UnitDescriptor: TBareGameUnitDescriptor;
  Menu: TIDEMenuSection;
begin
  RegisterProjectDescriptor(TBareGameProjectDescriptor.Create);
  UnitDescriptor := TBareGameUnitDescriptor.Create;
  RegisterProjectFileDescriptor(UnitDescriptor);
  RegisterIDEMenuCommand(itmFileNew, 'NewBareGameUnitItem', 'New Game Unit',
    UnitDescriptor.Execute, nil, nil, 'item_unit');
  RegisterIDEMenuCommand(itmInfoHelps, 'AboutBareGameItem', 'About Bare Game',
    nil, ShowAboutBareGame, nil, 'menu_information');
end;

end.


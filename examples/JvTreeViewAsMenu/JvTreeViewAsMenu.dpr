program JvTreeViewAsMenu;

uses
  Forms,
  JvTreeViewAsMenuMainFormU in 'JvTreeViewAsMenuMainFormU.pas' {JvTreeViewAsMenuMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvTreeViewAsMenuMainForm, JvTreeViewAsMenuMainForm);
  Application.Run;
end.

program JvTreeViewAsMenu;

uses
  Forms, JvTreeViewAsMenuMainFormU;  

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvTreeViewAsMenuMainForm, JvTreeViewAsMenuMainForm);
  Application.Run;
end.

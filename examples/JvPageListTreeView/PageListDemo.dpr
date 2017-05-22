program PageListDemo;

uses
  Forms,
  JvPageListTreeViewMainForm in 'JvPageListTreeViewMainForm.pas' {JvPageListTreeViewMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvPageListTreeViewMainFrm, JvPageListTreeViewMainFrm);
  Application.Run;
end.

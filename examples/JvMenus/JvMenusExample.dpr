program JvMenusExample;

uses
  Forms,
  JvMenusExampleMainForm in 'JvMenusExampleMainForm.pas' {JvMenusExampleMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvMenusExampleMainFrm, JvMenusExampleMainFrm);
  Application.Run;
end.

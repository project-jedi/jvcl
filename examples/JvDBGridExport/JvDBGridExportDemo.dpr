program JvDBGridExportDemo;

uses
  Forms,
  JvDBGridExportDemoMainForm in 'JvDBGridExportDemoMainForm.pas' {JvDBGridExportDemoMainFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBGridExportDemoMainFrm, JvDBGridExportDemoMainFrm);
  Application.Run;
end.


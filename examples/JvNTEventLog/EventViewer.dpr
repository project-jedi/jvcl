program EventViewer;

uses
  Forms,
  JvNTEventLogMainFormU in 'JvNTEventLogMainFormU.pas' {JvNTEventLogMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvNTEventLogMainForm, JvNTEventLogMainForm);
  Application.Run;
end.


program EventViewer;

uses
  Forms, JvNTEventLogMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvNTEventLogMainForm, JvNTEventLogMainForm);
  Application.Run;
end.


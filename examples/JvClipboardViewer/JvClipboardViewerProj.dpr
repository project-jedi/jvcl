program JvClipboardViewerProj;

uses
  Forms,
  JvClipboardViewerMainFormU in 'JvClipboardViewerMainFormU.pas' {JvClipboardViewerMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvClipboardViewerMainForm, JvClipboardViewerMainForm);
  Application.Run;
end.

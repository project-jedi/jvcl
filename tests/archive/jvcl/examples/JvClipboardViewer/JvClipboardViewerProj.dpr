program JvClipboardViewerProj;

uses
  Forms, JvClipboardViewerMainFormU;  

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvClipboardViewerMainForm, JvClipboardViewerMainForm);
  Application.Run;
end.

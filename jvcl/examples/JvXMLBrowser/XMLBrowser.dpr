program XMLBrowser;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {XMLBrowserDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TXMLBrowserDemo, XMLBrowserDemo);
  Application.Run;
end.

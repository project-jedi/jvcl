program ColorButtonDemo;

uses
  QForms,
  Main in 'Main.pas' {ColorDemoMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TColorDemoMainForm, ColorDemoMainForm);
  Application.Run;
end.

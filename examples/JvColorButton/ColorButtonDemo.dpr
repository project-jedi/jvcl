program ColorButtonDemo;

uses
  Forms,
  ColorButtonDemoMain in 'ColorButtonDemoMain.pas' {ColorDemoMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TColorDemoMainForm, ColorDemoMainForm);
  Application.CreateForm(TColorDemoMainForm, ColorDemoMainForm);
  Application.Run;
end.

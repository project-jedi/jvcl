program OfficeColorButtonDemo;

uses
  Forms,
  Main in 'Main.pas' {ColorDemoMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TColorDemoMainForm, ColorDemoMainForm);
  Application.Run;
end.

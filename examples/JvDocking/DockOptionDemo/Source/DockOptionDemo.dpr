program DockOptionDemo;

uses
  Forms, Windows,
  MainForm in 'MainForm.pas' {Main_Form},
  DockWindow in 'DockWindow.pas' {DockWindow_Form};

{$R *.RES}
begin
  Application.Initialize;
  Application.CreateForm(TMain_Form, Main_Form);
//  Main_Form.CreateDockWidnow;
  Application.Run;
end.

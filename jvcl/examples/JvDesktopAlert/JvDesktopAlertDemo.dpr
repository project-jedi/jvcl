program JvDesktopAlertDemo;

uses
  Forms,
  JvDesktopAlertDemoForm in 'JvDesktopAlertDemoForm.pas' {JvDesktopAlertDemoFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDesktopAlertDemoFrm, JvDesktopAlertDemoFrm);
  Application.Run;
end.

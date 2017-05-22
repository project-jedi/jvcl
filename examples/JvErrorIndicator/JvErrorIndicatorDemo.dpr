program JvErrorIndicatorDemo;

uses
  Forms,
  JvErrorIndicatorMainDemoForm in 'JvErrorIndicatorMainDemoForm.pas' {JvErrorIndicatorMainDemoFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvErrorIndicator Demo';
  Application.CreateForm(TJvErrorIndicatorMainDemoFrm, JvErrorIndicatorMainDemoFrm);
  Application.Run;
end.

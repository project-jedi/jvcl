program JvDbMaskEditDemo;

uses
  Forms,
  JvDbMaskEditDemoForm in 'JvDbMaskEditDemoForm.pas' {JvDbMaskEditDemoFrm},
  StdCtrls;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDbMaskEditDemoFrm, JvDbMaskEditDemoFrm);
  Application.Run;
end.

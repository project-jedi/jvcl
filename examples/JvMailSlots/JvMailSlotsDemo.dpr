program JvMailSlotsDemo;

uses
  Forms,
  FrmMain in 'FrmMain.pas' {FormMain},
  JvMailSlots in '..\..\run\JvMailSlots.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

program Demo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  JvFillImpl in 'JvFillImpl.pas',
  JvFillerControls in 'JvFillerControls.pas',
  JvTestReg in 'JvTestReg.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program JvFormatEditDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  JvValidateCtrls in '..\..\source\JvValidateCtrls.pas',
  JvErrProvider in '..\..\source\JvErrProvider.pas',
  JvValidators in '..\..\source\JvValidators.pas',
  JvCharStrEditor in '..\..\source\JvCharStrEditor.pas' {frmJvCharEditDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


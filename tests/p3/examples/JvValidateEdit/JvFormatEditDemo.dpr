program JvFormatEditDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  JvValidateEdit in '..\..\source\JvValidateEdit.pas',
  JvCharStrEditor in '..\..\source\JvCharStrEditor.pas' {frmJvCharEditDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


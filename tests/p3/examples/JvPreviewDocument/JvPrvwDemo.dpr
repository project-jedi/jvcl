program JvPrvwDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvPrvwDoc in '..\..\source\JvPrvwDoc.pas',
  JvPrvwRender in '..\..\source\JvPrvwRender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

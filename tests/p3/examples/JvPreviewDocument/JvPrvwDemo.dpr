program JvPrvwDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  JvPrvwDoc in '..\..\source\JvPrvwDoc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

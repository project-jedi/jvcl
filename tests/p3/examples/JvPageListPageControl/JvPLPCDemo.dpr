program JvPLPCDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  JvPageListPCDemo in '..\..\source\JvPageListPCDemo.pas',
  JvPageListTreeView in '..\..\source\JvPageListTreeView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program JvComboListBoxDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  JvComboListBox in '..\..\run\JvComboListBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


program JvComboListBoxDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  DropFrm in 'DropFrm.pas' {frmDrop};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


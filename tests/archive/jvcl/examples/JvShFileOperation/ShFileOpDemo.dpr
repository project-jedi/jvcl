program ShFileOpDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

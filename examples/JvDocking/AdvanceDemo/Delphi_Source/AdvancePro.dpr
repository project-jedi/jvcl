program AdvancePro;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  frmDelphiStyle in 'frmDelphiStyle.pas' {Form1},
  frmVCStyle in 'frmVCStyle.pas' {Form2},
  frmVIDStyle in 'frmVIDStyle.pas' {Form3},
  frmVSNetStyle in 'frmVSNetStyle.pas' {Form4};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

program SimpleHIDWrite;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMain},
  Unit2 in 'Unit2.pas' {InfoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.

program AdvancePro;

uses
  Forms,
  AdvanceProMainForm in 'AdvanceProMainForm.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

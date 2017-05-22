program FAQBuilder;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  OptionsFrm in 'OptionsFrm.pas' {frmOptions},
  FAQGlobals in 'FAQGlobals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

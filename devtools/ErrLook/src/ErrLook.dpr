program ErrLook;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  ModulesFrm in 'ModulesFrm.pas' {frmModules};

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := '';
  Application.Title := 'Delphi Error Lookup';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

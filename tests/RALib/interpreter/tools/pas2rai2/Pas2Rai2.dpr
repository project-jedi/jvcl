program Pas2JvInterpreter;

uses
  Forms,
  fPasImport in 'fPasImport.pas' {PasImport},
  fDebug in 'fDebug.pas' {DebugLog},
  fRegClasses in 'fRegClasses.pas' {RegClasses};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TPasImport, PasImport);
  Application.CreateForm(TDebugLog, DebugLog);
  Application.CreateForm(TRegClasses, RegClasses);
  Application.Run;
end.

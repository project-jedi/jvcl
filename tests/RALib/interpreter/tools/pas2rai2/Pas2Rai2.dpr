program Pas2JvInterpreter;

uses
  Forms,
  fJvPasImport in 'fJvPasImport.pas' {PasImport},
  fJvDebug in 'fJvDebug.pas' {DebugLog},
  fJvRegClasses in 'fJvRegClasses.pas' {RegClasses};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvPasImport , PasImport);
  Application.CreateForm(TJvDebugLog , DebugLog);
  Application.CreateForm(TJvRegClasses , RegClasses);
  Application.Run;
end.

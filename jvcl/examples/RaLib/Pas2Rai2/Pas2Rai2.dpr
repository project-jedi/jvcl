program Pas2Rai2;

uses
  Forms,
  JvPasImportForm in 'JvPasImportForm.pas' {PasImport},
  JvDebugForm in 'JvDebugForm.pas' {DebugLog},
  JvRegClassesForm in 'JvRegClassesForm.pas' {RegClasses};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvPasImport , PasImport);
  Application.CreateForm(TJvDebugLog , DebugLog);
  Application.CreateForm(TJvRegClasses , RegClasses);
  Application.Run;
end.

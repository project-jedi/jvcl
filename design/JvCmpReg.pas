{$I JVCL.INC}

unit JvCmpReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf,
  JvAlarms, JvConverter, JvDataEmbedded, JvCreateProcess,
  JvEnterTab, JvMergeManager, JvPageManager, JvPatchFile,
  JvStringHolder, JvTimeLimit, JvWinHelp, JvTranslator, JvPrint, JvEasterEgg,
  JvDataEmbeddedEditor, JvPatcherEditor, JvAlarmsEditor,
  JvDsgnEditors;

{.$R ..\resources\JvCmpReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Components',[
    TJvAlarms, TJvConverter, TJvDataEmbedded, TJvCreateProcess,
    TJvEnterAsTab, TJvMergeManager, TJvPageManager, TJvPatchFile,
    TJvStrHolder, TJvTimeLimit, TJvWinHelp, TJvTranslator, TJvPrint, TJvEasterEgg
    ]);

  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, '', TJvExeNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, 'CurrentDirectory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TStream), TJvDataEmbedded, 'Data', TJvDataEmbeddedEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvPatchFile, 'Differences', TJvPatcherEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvAlarms, 'Alarms', TJvAlarmsEditor);
end;

end.

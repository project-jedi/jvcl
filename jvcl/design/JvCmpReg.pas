{$I JVCL.INC}

unit JvCmpReg;

interface

procedure Register;

implementation
uses
  Classes, Controls,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvAlarms, JvConverter, JvDataEmbedded, JvCreateProcess,
  JvEnterTab, JvMergeManager, JvPageManager, JvPatchFile,
  JvStringHolder, JvTimeLimit, JvWinHelp, JvTranslator, JvPrint, JvEasterEgg,
  JvMouseGesture,
  JvDataEmbeddedEditor, JvPatcherEditor, JvProfilerForm,
  JvPageManagerForm,
  JvDsgnEditors;

{$R ..\resources\JvCmpReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteNonVisual,[
    TJvAlarms, TJvConverter, TJvDataEmbedded, TJvCreateProcess,
    TJvEnterAsTab, TJvMergeManager, TJvPageManager, TJvPatchFile, TJvProfiler,
    TJvStrHolder, TJvTimeLimit, TJvWinHelp, TJvTranslator, TJvTranslatorStrings, TJvPrint, TJvEasterEgg,
    TJvMouseGesture, TJvMouseGestureHook
    ]);

  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, '', TJvExeNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, 'CurrentDirectory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TStream), TJvDataEmbedded, 'Data', TJvDataEmbeddedEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvPatchFile, 'Differences', TJvPatcherEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvPageManager, 'PageProxies', TJvProxyListProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPageProxy, 'PageName', TJvPageNameProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager, 'PriorBtn', TJvPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager, 'NextBtn', TJvPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvMergeManager, 'MergeFrame', TJvComponentFormProperty);

  RegisterComponentEditor(TJvPageManager, TJvPageManagerEditor);
  RegisterComponentEditor(TJvStrHolder, TJvStringsEditor);

  RegisterNoIcon([TJvPageProxy]);
end;

end.

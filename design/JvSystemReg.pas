{$I JVCL.INC}

unit JvSystemReg;

interface

procedure Register;

implementation
uses
  Classes, Controls,
  {$IFDEF COMPILER6_UP}
  FiltEdit, DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvClipboardMonitor,  JvClipboardViewer,  JvCommStatus,  JvComputerInfo,  
  JvDdeCmd,  JvDeviceChanged,  JvDirectories, JvDragDrop,  JvHidControllerClass,  JvJoystick,  JvKeyboardStates,
  JvMRUList,  JvMRUManager, JvNTEventLog,  JvRas32,
  JvScreenSaver,  JvShellHook,  JvSHFileOperation, JvSoundControl,  JvSystemColors,
  JvThread,  JvThreadTimer,  JvTimerList, JvChangeNotify,  JvSimpleXml, JvXMLDatabase,
  JvWndProcHook, JvFormPlacement, JvTimer, JvSearchFiles, JvPerfMon95,
  JvChangeNotifyEditor, JvMinMaxForm, JvFormPropertiesForm,
  JvPerfStatEditor, JvTimerListEditor,
  JvDsgnEditors;

{$R ..\resources\JvSystemReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteSystem,[TJvClipboardMonitor,TJvClipboardViewer,
    {TJvComputerInfo, // - do not register this component as default}
    TJvSHFileOperation, TJvChangeNotify, TJvDragDrop, TJvHidDeviceController,
    TJvNTEventLog, TJvScreenSaver,
    TJvDeviceChanged, TJvJoystick, TJvSoundControl, 
    TJvKeyboardStates, TJvDirectories, TJvSystemColors, TJvAppDdeCmd, TJvPerfStat95]);

  RegisterComponents(SPaletteInternetWork,[
    TJvSimpleXML, TJvXMLDatabase, TJvRas32, TJvCommStatus
    ]);
  RegisterComponents(SPaletteNonVisual,[
    TJvFormStorage, TJvSearchFiles,
    TJvMRUList, TJvMRUManager,
    TJvShellHook, TJvWindowHook, 
    TJvTimer, TJvThread, TJvThreadTimer, TJvTimerList
    ]);

  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TMinMaxProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvFormStorage, 'StoredProps',
    TJvStoredPropsProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvWindowHook,
    'WinControl', TJvComponentFormProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvChangeItem,
    'Directory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPerfStatItem,
    'PerfStatKey', TJvPerfStatProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvSearchFiles,
    'RootDirectory', TJvDirectoryProperty);

  RegisterComponentEditor(TJvFormStorage, TJvFormStorageEditor);
  RegisterComponentEditor(TJvChangeNotify, TJvChangeNotifyEditor);
  RegisterComponentEditor(TJvTimerList, TJvTimerListDefaultEditor);
end;

end.

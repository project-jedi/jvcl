{$I JVCL.INC}

unit JvSystemReg;

interface

procedure Register;

implementation
uses
  Classes, Controls, DesignIntf, DesignEditors,

  JvClipboardMonitor,  JvClipboardViewer,  JvCommStatus,  JvComputerInfo,  JvCpuUsage,
  JvDdeCmd,  JvDeviceChanged,  JvDirectories, JvDragDrop,  JvHidControllerClass,  JvJoystick,  JvKeyboardStates,
  JvMemoryInfos,  JvMRUList,  JvMRUManager, JvNTEventLog,  JvRas32,
  JvScreenSaver,  JvShellHook,  JvSHFileOperation, JvSoundControl,  JvSystemColors,
  JvThread,  JvThreadTimer,  JvTimerList, JvChangeNotify,  JvSimpleXml,
  JvWndProcHook, JvFormPlacement, JvTimer, JvSearchFiles, JvPerfMon95,
  JvChangeNotifyEditor, JvTimerListForm, JvMinMaxForm, JvFormPropertiesForm,
  JvPerfStatEditor, 
  JvDsgnEditors;

{$R ..\resources\JvSystemReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv System',[TJvClipboardMonitor,TJvClipboardViewer,TJvCommStatus,
    {TJvComputerInfo, // - do not register this component as default}{TJvCpuUsage,}
    TJvAppDdeCmd, TJvDeviceChanged, TJvDirectories, TJvDragDrop, TJvHidDeviceController,
    TJvJoystick, TJvKeyboardStates, TJvMemoryInfos, TJvMruList, TJvMRUManager,
    TJvNTEventLog, TJvRas32, TJvScreenSaver, TJvShellHook, TJvSHFileOperation,
    TJvSoundControl, TJvSystemColors, TJvTimer, TJvThread, TJvThreadTimer, TJvTimerList,
    TJvChangeNotify, TJvSimpleXML, TJvWindowHook, TJvFormStorage,TJvSearchFiles, TJvPerfStat95]);

  RegisterPropertyEditor(TypeInfo(TList), TJvTimerList, 'Events', TJvTimersItemListProperty);
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
  RegisterComponentEditor(TJvTimerList, TJvTimersCollectionEditor);
  RegisterNoIcon([TJvTimerEvent]);
end;

end.

{$I JVCL.INC}

unit JvSystemReg;

interface

procedure Register;

implementation
uses
  Classes,

  JvClipboardMonitor,  JvClipboardViewer,  JvCommStatus,  JvComputerInfo,  JvCpuUsage,
  JvDdeCmd,  JvDeviceChanged,  JvDirectories, JvDragDrop,  JvHidControllerClass,  JvJoystick,  JvKeyboardStates,
  JvMemoryInfos,  JvMRUList,  JvMRUManager, JvNTEventLog,  JvRas32,
  JvScreenSaver,  JvShellHook,  JvSHFileOperation, JvSoundControl,  JvSystemColors,
  JvThread,  JvThreadTimer,  JvTimerList, JvChangeNotify,  JvSimpleXml,
  JvWndProcHook, JvFormPlacement, JvTimer, JvSearchFiles;

{.$R ..\resources\JvSystemReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv System',[TJvClipboardMonitor,TJvClipboardViewer,TJvCommStatus,
    {TJvComputerInfo, // - do not register this component as default}{TJvCpuUsage,}
    TJvAppDdeCmd, TJvDeviceChanged, TJvDirectories, TJvDragDrop, TJvHidDeviceController,
    TJvJoystick, TJvKeyboardStates, TJvMemoryInfos, TJvMruList, TJvMRUManager,
    TJvNTEventLog, TJvRas32, TJvScreenSaver, TJvShellHook, TJvSHFileOperation,
    TJvSoundControl, TJvSystemColors, TJvTimer, TJvThread, TJvThreadTimer, TJvTimerList,
    TJvChangeNotify, TJvSimpleXML, TJvWindowHook, TJvFormStorage,TJvSearchFiles]);
end;

end.

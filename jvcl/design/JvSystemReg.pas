{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSystemReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

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
  JvDsgnConsts,
  JvClipboardMonitor, JvClipboardViewer, JvCommStatus, {JvComputerInfo,}
  JvDdeCmd, JvDeviceChanged, JvDirectories, JvDragDrop, JvHidControllerClass,
  JvJoystick, JvKeyboardStates, JvMRUList, JvMRUManager, JvNTEventLog, JvRas32,
  JvAppInst, JvScreenSaver, JvShellHook, JvSHFileOperation, JvSoundControl,
  JvSystemColors, JvThread, JvThreadTimer, JvTimerList, JvChangeNotify,
  JvSimpleXml, JvXMLDatabase, JvWndProcHook, JvFormPlacement, JvTimer,
  JvSearchFiles, JvPerfMon95, JvChangeNotifyEditor, JvMinMaxForm,
  JvFormPropertiesForm, JvPerfStatEditor, JvTimerListEditor, JvDsgnEditors,
  JvAppXmlStore;

{$R ..\resources\JvSystemReg.dcr}

procedure Register;
begin
  RegisterComponents(RsPaletteSystem, [TJvClipboardMonitor, TJvClipboardViewer,
    {TJvComputerInfo, // - do not register this component as default}
    TJvSHFileOperation, TJvChangeNotify, TJvDropTarget, TJvDragDrop, TJvAppInstances,
      TJvHidDeviceController, TJvNTEventLog, TJvScreenSaver, TJvDeviceChanged,
      TJvJoystick, TJvSoundControl, TJvKeyboardStates, TJvDirectories,
      TJvSystemColors, TJvAppDdeCmd, TJvPerfStat95]);
  RegisterComponents(RsPaletteInternetWork, [TJvSimpleXML, TJvXMLDatabase,
    TJvRas32, TJvCommStatus]);
  RegisterComponents(RsPaletteNonVisual, [TJvFormStorage, TJvSearchFiles,
      TJvMRUList, TJvMRUManager, TJvShellHook, TJvWindowHook,
      TJvTimer, TJvThread, TJvThreadTimer, TJvTimerList, TJvAppXmlStore]);

  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TMinMaxProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvFormStorage,
    'StoredProps', TJvStoredPropsProperty);
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


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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSystemReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes,
  Controls,
  FiltEdit, DesignEditors, DesignIntf,
  JvDsgnConsts,
  JvClipboardMonitor, JvClipboardViewer, JvHidControllerClass,
  JvDragDrop, JvDdeCmd, JvAppCommand, JvScreenSaveSuppress, JvWndProcHook, JvSysRequirements,
  JvMRUList, JvMRUManager, JvCommStatus, JvJoystick,
  JvNTEventLog, JvRas32, JvAppInst, JvScreenSaver,
  JvShellHook, JvSHFileOperation, JvSoundControl, JvChangeNotify, JvSearchFiles,
  JvPerfMon95, JvComputerInfoEx,
  JvChangeNotifyEditor, JvPerfStatEditor, JvTimerList, JvTimerListEditor,
  JvDebugHandler,
  JvThread, JvThreadDialog, JvThreadTimer, JvTimer, JvSimpleXml, JvXMLDatabase,
  JvFormPlacement, JvAppXMLStorage, JvFormPlacementSelectList,
  JvMinMaxForm, JvFormPropertiesForm, JvDsgnEditors, JvMailSlots;

{$R JvSystemReg.dcr}

procedure Register;
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvSimpleXML, TControl);
  GroupDescendentsWith(TJvTimer, TControl);
  GroupDescendentsWith(TJvAppInstances, TControl);
  GroupDescendentsWith(TJvTimerList, TControl);
  {$ENDIF COMPILER7_UP}

  RegisterComponents(RsPalettePersistence, [TJvFormStorage, TJvFormStorageSelectList,
    TJvAppXMLFileStorage]);
  RegisterComponents(RsPaletteInternetWork, [TJvSimpleXML, TJvXMLDatabase]);
  RegisterComponents(RsPaletteSystem, [TJvClipboardMonitor, TJvClipboardViewer,
    TJvAppDdeCmd, TJvHidDeviceController, TJvDropTarget, TJvDragDrop, TJvAppCommand,
    TJvScreenSaveSuppressor, TJvSysRequirements]);
  RegisterComponents(RsPaletteSystem, [{TJvComputerInfo, // - do not register this component as default}
    TJvSHFileOperation, TJvChangeNotify, TJvAppInstances, TJvNTEventLog,
    TJvMailSlotServer, TJvMailSlotClient,
    TJvScreenSaver, TJvJoystick, TJvSoundControl,
    TJvPerfStat95, TJvComputerInfoEx, TJvDebugHandler]);
  RegisterComponents(RsPaletteInternetWork, [TJvRas32, TJvCommStatus]);
  RegisterComponents(RsPaletteNonVisual, [
    TJvSearchFiles, TJvMRUList, TJvMRUManager,
    TJvTimerList, TJvWindowHook, TJvTimer, TJvShellHook]);
  RegisterComponents(RsPaletteThreading, [
    TJvThread, TJvThreadSimpleDialog, TJvThreadAnimateDialog, TJvThreadTimer]);

  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TJvMinMaxProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvWindowHook,
    'WinControl', TJvComponentFormProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvChangeItem,
    'Directory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPerfStatItem,
    'PerfStatKey', TJvPerfStatProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvSearchFiles,
    'RootDirectory', TJvDirectoryProperty);
  RegisterComponentEditor(TJvChangeNotify, TJvChangeNotifyEditor);
  RegisterComponentEditor(TJvTimerList, TJvTimerListDefaultEditor);

  RegisterComponentEditor(TJvFormStorage, TJvFormStorageEditor);
end;

end.

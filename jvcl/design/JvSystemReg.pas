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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvSystemReg;

interface

procedure Register;

implementation

uses
  Classes,
  {$IFDEF VCL}
  Controls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  FiltEdit, DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnConsts,
  {$IFDEF VCL}
  JvClipboardMonitor, JvClipboardViewer, JvHidControllerClass,
  JvDragDrop, JvDdeCmd, JvWndProcHook,
  {$ENDIF VCL}
  {$IFDEF MSWINDOWS}
  JvMRUList, JvMRUManager,
  JvCommStatus, {JvComputerInfo,}
  {JvDeviceChanged, JvDirectories, JvSystemColors, JvKeyboardStates,} JvJoystick,
  JvNTEventLog, JvRas32, JvAppInst, JvScreenSaver,
  JvShellHook, JvSHFileOperation, JvSoundControl, JvChangeNotify, JvSearchFiles,
  JvPerfMon95, JvComputerInfoEx,
  JvChangeNotifyEditor, JvPerfStatEditor,
  {$ENDIF MSWINDOWS}
  JvThread, JvThreadTimer, JvTimer, JvTimerList, JvSimpleXml, JvXMLDatabase,
  JvFormPlacement, JvAppXMLStorage, JvFormPlacementSelectList,
  JvMinMaxForm, JvFormPropertiesForm, JvTimerListEditor, JvDsgnEditors;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvSystemReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvSystemReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvSimpleXML, TControl);
  GroupDescendentsWith(TJvTimer, TControl);
  {$ENDIF COMPILER7_UP}

  RegisterComponents(RsPalettePersistence, [TJvFormStorage, TJvFormStorageSelectList,
      TJvAppXMLFileStorage]);
  RegisterComponents(RsPaletteInternetWork, [TJvSimpleXML, TJvXMLDatabase]);
  {$IFDEF VCL}
  RegisterComponents(RsPaletteSystem, [TJvClipboardMonitor, TJvClipboardViewer,
    TJvAppDdeCmd, TJvHidDeviceController, TJvDropTarget, TJvDragDrop]);
  {$ENDIF VCL}
  {$IFDEF MSWINDOWS}
  RegisterComponents(RsPaletteSystem, [{TJvComputerInfo, // - do not register this component as default}
    TJvSHFileOperation, TJvChangeNotify, TJvAppInstances, TJvNTEventLog,
    TJvScreenSaver, TJvNTEventLog, TJvScreenSaver, TJvJoystick, TJvSoundControl,
    {TJvDeviceChanged, TJvSystemColors, TJvKeyboardStates, TJvDirectories, these are not needed - included in JvComputerInfoEx instead}
    TJvPerfStat95, TJvComputerInfoEx]);
  RegisterComponents(RsPaletteInternetWork, [TJvRas32, TJvCommStatus]);
  {$ENDIF MSWINDOWS}
  RegisterComponents(RsPaletteNonVisual, [
    {$IFDEF MSWINDOWS}
    TJvSearchFiles, TJvMRUList, TJvMRUManager, TJvShellHook,
    {$ENDIF MSWINDOWS}
    {$IFDEF VCL}
    TJvWindowHook,
    {$ENDIF VCL}
    TJvTimer, TJvThread, TJvThreadTimer, TJvTimerList]);

  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TMinMaxProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvFormStorage,
    'StoredProps', TJvStoredPropsProperty);
  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvWindowHook,
    'WinControl', TJvComponentFormProperty);
  {$ENDIF VCL}
  {$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(string), TJvChangeItem,
    'Directory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPerfStatItem,
    'PerfStatKey', TJvPerfStatProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvSearchFiles,
    'RootDirectory', TJvDirectoryProperty);
  RegisterComponentEditor(TJvChangeNotify, TJvChangeNotifyEditor);
  {$ENDIF MSWINDOWS}

  RegisterComponentEditor(TJvFormStorage, TJvFormStorageEditor);
  RegisterComponentEditor(TJvTimerList, TJvTimerListDefaultEditor);
end;

end.


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

unit JvSystemReg;

{$I jvcl.inc}

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

interface

procedure Register;

implementation

uses
  Classes,
  Controls,
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
  {$IFDEF USEWINDOWS}
  JvMRUList, JvMRUManager,
  JvCommStatus, {JvComputerInfo,}
  {JvDeviceChanged, JvDirectories, JvSystemColors, JvKeyboardStates,} JvJoystick,
  JvNTEventLog, JvRas32, JvAppInst, JvScreenSaver,
  JvShellHook, JvSHFileOperation, JvSoundControl, JvChangeNotify, JvSearchFiles,
  JvPerfMon95, JvComputerInfoEx,
  JvChangeNotifyEditor, JvPerfStatEditor, JvTimerList, JvTimerListEditor,
  {$ENDIF USEWINDOWS}
  JvThread, JvThreadTimer, JvTimer, JvSimpleXml, JvXMLDatabase,
  JvFormPlacement, JvAppXMLStorage, JvFormPlacementSelectList,
  JvMinMaxForm, JvFormPropertiesForm, JvDsgnEditors;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvSystemReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvSystemReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF COMPILER7_UP}
  GroupDescendentsWith(TJvSimpleXML, TControl);
  GroupDescendentsWith(TJvTimer, TControl);
  GroupDescendentsWith(TJvAppInstances, TControl);
  GroupDescendentsWith(TJvTimerList, TControl);
  {$ENDIF COMPILER7_UP}
  {$ENDIF MSWINDOWS}

  RegisterComponents(RsPalettePersistence, [TJvFormStorage, TJvFormStorageSelectList,
      TJvAppXMLFileStorage]);
  RegisterComponents(RsPaletteInternetWork, [TJvSimpleXML, TJvXMLDatabase]);
  {$IFDEF VCL}
  RegisterComponents(RsPaletteSystem, [TJvClipboardMonitor, TJvClipboardViewer,
    TJvAppDdeCmd, TJvHidDeviceController, TJvDropTarget, TJvDragDrop]);
  {$ENDIF VCL}
  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteSystem, [{TJvComputerInfo, // - do not register this component as default}
    TJvSHFileOperation, TJvChangeNotify, TJvAppInstances, TJvNTEventLog,
    TJvScreenSaver, TJvNTEventLog, TJvScreenSaver, TJvJoystick, TJvSoundControl,
    {TJvDeviceChanged, TJvSystemColors, TJvKeyboardStates, TJvDirectories, these are not needed - included in JvComputerInfoEx instead}
    TJvPerfStat95, TJvComputerInfoEx]);
  RegisterComponents(RsPaletteInternetWork, [TJvRas32, TJvCommStatus]);
  {$ENDIF USEWINDOWS}
  RegisterComponents(RsPaletteNonVisual, [
    {$IFDEF USEWINDOWS}
    TJvSearchFiles, TJvMRUList, TJvMRUManager, TJvShellHook,
    TJvTimerList,
    {$ENDIF USEWINDOWS}
    {$IFDEF VCL}
    TJvWindowHook,
    {$ENDIF VCL}
    TJvTimer, TJvThread, TJvThreadTimer]);

  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TMinMaxProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvFormStorage,
    'StoredProps', TJvStoredPropsProperty);
  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvWindowHook,
    'WinControl', TJvComponentFormProperty);
  {$ENDIF VCL}
  {$IFDEF USEWINDOWS}
  RegisterPropertyEditor(TypeInfo(string), TJvChangeItem,
    'Directory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPerfStatItem,
    'PerfStatKey', TJvPerfStatProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvSearchFiles,
    'RootDirectory', TJvDirectoryProperty);
  RegisterComponentEditor(TJvChangeNotify, TJvChangeNotifyEditor);
  RegisterComponentEditor(TJvTimerList, TJvTimerListDefaultEditor);
  {$ENDIF USEWINDOWS}

  RegisterComponentEditor(TJvFormStorage, TJvFormStorageEditor);
end;

end.


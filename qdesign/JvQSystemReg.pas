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

unit JvQSystemReg;

interface

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

procedure Register;

implementation

uses
  Classes,


  QControls,


  DesignEditors, DesignIntf,

  JvQDsgnConsts,

  {$IFDEF USEWINDOWS}
  JvQMRUList, JvQMRUManager,
  JvQCommStatus, {JvComputerInfo,}
  {JvDeviceChanged, JvDirectories, JvSystemColors, JvKeyboardStates,} JvQJoystick,
  JvQNTEventLog, JvQRas32, JvQAppInst, JvQScreenSaver,
  JvQShellHook, JvQSHFileOperation, JvQSoundControl, JvQChangeNotify, JvQSearchFiles,
  JvQPerfMon95, JvQComputerInfoEx,
  JvQChangeNotifyEditor, JvQPerfStatEditor, JvQTimerList, JvQTimerListEditor,
  {$ENDIF USEWINDOWS}
  JvQSystemColors,	
  JvQThread, JvQThreadTimer, JvQTimer, JvQSimpleXml, JvQXMLDatabase,
  JvQFormPlacement, JvQAppXMLStorage, JvQFormPlacementSelectList,
  JvQMinMaxForm, JvQFormPropertiesForm, JvQDsgnEditors;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvSystemReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvSystemReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  {$IFDEF MSWINDOWS}
  GroupDescendentsWith(TJvSimpleXML, TControl);
  GroupDescendentsWith(TJvTimer, TControl);
  GroupDescendentsWith(TJvAppInstances, TControl);
  GroupDescendentsWith(TJvTimerList, TControl);
  {$ENDIF MSWINDOWS}

  RegisterComponents(RsPalettePersistence, [TJvFormStorage, TJvFormStorageSelectList,
      TJvAppXMLFileStorage]);
  RegisterComponents(RsPaletteInternetWork, [TJvSimpleXML, TJvXMLDatabase]);

  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteSystem, [{TJvComputerInfo, // - do not register this component as default}
    TJvSHFileOperation, TJvChangeNotify, TJvAppInstances, TJvNTEventLog,
    TJvScreenSaver, TJvNTEventLog, TJvScreenSaver, TJvJoystick, TJvSoundControl,
    {TJvDeviceChanged,  TJvKeyboardStates, TJvDirectories, these are not needed - included in JvComputerInfoEx instead}
    TJvPerfStat95, TJvComputerInfoEx, TJvSystemColors]);
  RegisterComponents(RsPaletteInternetWork, [TJvRas32, TJvCommStatus]);
  {$ENDIF USEWINDOWS}
  RegisterComponents(RsPaletteNonVisual, [
    {$IFDEF USEWINDOWS}
    TJvSearchFiles, TJvMRUList, TJvMRUManager, TJvShellHook,
    {$ENDIF USEWINDOWS}

    TJvTimer, TJvThread, TJvThreadTimer
    {$IFDEF USEWINDOWS}, TJvTimerList {$ENDIF}
    ]);
  {$IFDEF LINUX}
  RegisterComponents(RsPaletteVisual,[TJvSystemColors]);	
  {$ENDIF LINUX}
  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TMinMaxProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvFormStorage,
    'StoredProps', TJvStoredPropsProperty);

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


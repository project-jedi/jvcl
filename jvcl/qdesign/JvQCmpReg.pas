{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQCmpReg;

{$I jvcl.inc}

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

interface

procedure Register;

implementation

uses
  Classes,
  QControls, 
  DesignEditors, DesignIntf, 
  JvQDsgnConsts,
  {$IFDEF USEWINDOWS}
  JvQCreateProcess, JvQWinHelp,
  {$ENDIF USEWINDOWS}
  JvQAlarms, JvQConverter, JvQDataEmbedded, JvQEnterTab, JvQMergeManager,
  JvQPageManager, JvQPatchFile, JvQStringHolder, JvQTimeLimit,
  JvQTranslator, JvQPrint, JvQEasterEgg, JvQMouseGesture, JvQLogFile,
  JvQDataEmbeddedEditor, JvQPatcherEditor, JvQProfilerForm, JvQPageManagerForm,
  JvQDsgnEditors;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCmpReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCmpReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin 
  GroupDescendentsWith(TJvDataEmbedded, TControl);
  GroupDescendentsWith(TJvStrHolder, TControl);
  GroupDescendentsWith(TJvPageManager, TControl); 

  RegisterComponents(RsPaletteNonVisual, [TJvAlarms, TJvConverter,
    TJvDataEmbedded,
    TJvEnterAsTab, TJvMergeManager, TJvPageManager, TJvPatchFile, TJvProfiler,
    TJvStrHolder, TJvTimeLimit, TJvTranslator, TJvTranslatorStrings,
    TJvPrint, TJvEasterEgg, TJvMouseGesture, TJvMouseGestureHook, TJvLogFile]);
  {$IFDEF USEWINDOWS}
  RegisterComponents(RsPaletteNonVisual, [TJvCreateProcess, TJvWinHelp]);
  {$ENDIF USEWINDOWS}


  {$IFDEF USEWINDOWS}
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess,
    '', TJvExeNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess,
    'CurrentDirectory', TJvDirectoryProperty);
  {$ENDIF USEWINDOWS}
//  RegisterPropertyEditor(TypeInfo(TStream), TJvDataEmbedded,
//    'Data', TJvDataEmbeddedEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvPatchFile,
    'Differences', TJvPatcherEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvPageManager,
    'PageProxies', TJvProxyListProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPageProxy,
    'PageName', TJvPageNameProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager,
    'PriorBtn', TJvPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager,
    'NextBtn', TJvPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvMergeManager,
    'MergeFrame', TJvComponentFormProperty);

  RegisterComponentEditor(TJvPageManager, TJvPageManagerEditor);
  RegisterComponentEditor(TJvStrHolder, TJvStringsEditor);
  RegisterComponentEditor(TJvDataEmbedded,TJvDataEmbeddedComponentEditor);

  RegisterNoIcon([TJvPageProxy]);
end;

end.

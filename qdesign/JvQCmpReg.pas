{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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

{$I jvcl.inc}

unit JvQCmpReg;

interface

procedure Register;

implementation

uses
  Classes,
  
  
  QControls,
  
  
  DesignEditors, DesignIntf,
  
  JvQDsgnConsts,
  JvQAlarms, JvQConverter, JvQDataEmbedded, JvQCreateProcess,
  JvQEnterTab, JvQMergeManager, JvQPageManager, JvQPatchFile, JvQStringHolder,
  JvQTimeLimit, JvQWinHelp, JvQTranslator, JvQPrint, JvQEasterEgg,
  JvQMouseGesture, JvQLogFile, JvQDataEmbeddedEditor, JvQPatcherEditor,
  JvQProfilerForm, JvQPageManagerForm, JvQDsgnEditors;



{$R ../Resources/JvCmpReg.dcr}


procedure Register;
begin
  
  GroupDescendentsWith(TJvDataEmbedded, TControl);
  GroupDescendentsWith(TJvStrHolder, TControl);
  

  RegisterComponents(RsPaletteNonVisual,[TJvAlarms, TJvConverter,
    TJvDataEmbedded, TJvCreateProcess,
    TJvEnterAsTab, TJvMergeManager, TJvPageManager, TJvPatchFile, TJvProfiler,
    TJvStrHolder, TJvTimeLimit, TJvWinHelp, TJvTranslator, TJvTranslatorStrings,
    TJvPrint, TJvEasterEgg, TJvMouseGesture, TJvMouseGestureHook, TJvLogFile]);

  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess,
    '', TJvExeNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess,
    'CurrentDirectory', TJvDirectoryProperty);
//  RegisterPropertyEditor(TypeInfo(TStream), TJvDataEmbedded,
//    'Data', TJvDataEmbeddedEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvPatchFile,
    'Differences', TJvPatcherEditor);
  
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvMergeManager,
    'MergeFrame', TJvComponentFormProperty);

  RegisterComponentEditor(TJvStrHolder, TJvStringsEditor);
  RegisterComponentEditor(TJvDataEmbedded,TJvDataEmbeddedComponentEditor);
  
end;

end.

{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCmpReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQCmpReg;

interface

procedure Register;

implementation

uses
  Classes, QControls,

  DesignEditors, DesignIntf,

  JvQDsgnConsts,
  JvQAlarms, JvQConverter, JvQDataEmbedded,
  JvQMergeManager, JvQPatchFile, JvQStringHolder,
  JvQTimeLimit, JvQTranslator, JvQPrint,
  JvQLogFile, JvQDataEmbeddedEditor, JvQPatcherEditor,
  JvQProfilerForm, JvQDsgnEditors
  {$IFDEF MSWINDOWS}
  , JvQWinHelp
  {$ENDIF MSWINDOWS}
  ;

{$IFDEF MSWINDOWS}
{$R ..\resources\JvCmpReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCmpReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  RegisterComponents(RsPaletteNonVisual,[TJvAlarms, TJvConverter, TJvDataEmbedded,
    
    TJvMergeManager,
    
    TJvPatchFile, TJvProfiler,
    TJvStrHolder, TJvTimeLimit,
    {$IFDEF MSWINDOWS}
    TJvWinHelp,
    {$ENDIF MSWINDOWS}
    TJvTranslator, TJvTranslatorStrings,  TJvPrint,
    
    TJvLogFile]);
  
  RegisterPropertyEditor(TypeInfo(TStrings), TJvPatchFile,
    'Differences', TJvPatcherEditor);
  
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvMergeManager,
    'MergeFrame', TJvComponentFormProperty);
  
  RegisterComponentEditor(TJvStrHolder, TJvStringsEditor);
  RegisterComponentEditor(TJvDataEmbedded,TJvDataEmbeddedComponentEditor);
  
end;

end.

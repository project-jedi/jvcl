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

The Original Code is: JvDlgsReg.PAS, released on 2002-05-26.

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

unit JvQDlgsReg;

interface

procedure Register;

implementation

uses
  Classes,


  QDialogs, QActnList, QExtDlgs,


  DesignEditors, DesignIntf,

  JvQDsgnConsts,
  JvQBaseDlg, JvQFindReplace, JvQCommonExecDlg, JvQTipOfDay,
  JvQActnResForm, JvQProgressComponent,
  JvQImageDlg, JvQDualList, {JvQDSADialogs,}
  JvQProgressDialog, JvQBaseDlgEditor, JvQTipOfDayEditor;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvDlgsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvDlgsReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  RegisterComponents(RsPaletteDialog, [
    TOpenPictureDialog, TSavePictureDialog, TPrinterSetupDialog,
    TJvDualListDialog, TJvImageDialog, TJvFindReplace, {TJvDSADialog,}
    TJvProgressDialog, TJvProgressComponent, TJvTipOfDay]);
  RegisterComponentEditor(TJvTipOfDay, TJvTipOfDayEditor);

  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);
  {$ENDIF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
//  RegisterComponentEditor(TJvOpenDialog, TJvBaseDlgEditor);
//  RegisterComponentEditor(TJvSaveDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogP, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogF, TJvBaseDlgEditor);


end;

end.
